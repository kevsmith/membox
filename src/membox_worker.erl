-module(membox_worker).

-behaviour(gen_server).

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock, db, command, resp_spec, complete=false}).

start(Sock) ->
  {ok, Pid} = gen_server:start(?MODULE, [Sock], []),
  gen_tcp:controlling_process(Sock, Pid),
  gen_server:call(Pid, configure_socket),
  {ok, Pid}.

init([Sock]) ->
  {ok, #state{sock=Sock,
              db=db1}}.
handle_call(configure_socket, _From, #state{sock=Sock}=State) ->
  inet:setopts(Sock, [{packet, line}, {active, once}, {nodelay, true},
                      {recbuf, 16384}, {sndbuf, 8192}]),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ignore, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info({tcp_closed, _Socket}, State) ->
  {stop, shutdown, State#state{sock=undefined}};

handle_info({tcp_error, _Socket, _Reason}, State) ->
  {stop, shutdown, State};

handle_info({tcp, Socket, Data}, State) ->
  case membox_parser:parse_string(Data) of
    {ok, {Command, ResponseSpec}} ->
      handle_command(Command, ResponseSpec, State);
    _Error ->
      io:format("Error: ~p~n", [_Error]),
      inet:setopts(Socket, [{active, once}]),
      {noreply, State}
  end;

handle_info({inet_async, _Sock, _, {ok, Data}}, #state{command=Command, resp_spec=RespSpec}=State) ->
  Size = size(Data) - 2,
  <<Payload:Size/binary, _/binary>> = Data,
  FinalCommand = list_to_tuple(Command ++ [Payload]),
  handle_command(FinalCommand, RespSpec, State#state{complete=true});

handle_info(_Info, State) ->
  io:format("Info: ~p~n", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
handle_command(quit, _, State) ->
  {stop, shutdown, State};

handle_command(Cmd, ResponseSpec, #state{db=Db, sock=Sock, complete=true}=State) ->
  Response = gen_server:call(Db, Cmd),
  membox_response:send(Response, ResponseSpec, Sock),
  {noreply, reset_state(State)};

handle_command(Command, ResponseSpec, #state{sock=Sock, db=Db}=State) when length(Command) == 2 ->
  Response = gen_server:call(Db, list_to_tuple(Command)),
  membox_response:send(Response, ResponseSpec, Sock),
  {noreply, reset_state(State)};

handle_command([Cmd|_]=Command, ResponseSpec, #state{sock=Sock, db=Db}=State) when Cmd =:= incrby;
                                                                                   Cmd =:= decrby;
                                                                                   Cmd =:= expire;
                                                                                   Cmd =:= keys ->
  Response = gen_server:call(Db, list_to_tuple(Command)),
  membox_response:send(Response, ResponseSpec, Sock),
  {noreply, reset_state(State)};

handle_command([Cmd|_]=Command, ResponseSpec, #state{sock=Sock}=State) when Cmd =:= lset;
                                                                            Cmd =:= lrem ->
  case lists:nth(4, Command) of
    Sz when is_number(Sz) ->
      inet:setopts(Sock, [binary, {packet, raw}]),
      prim_inet:async_recv(Sock, Sz, -1),
      {noreply, State#state{command=chop(Command, 3), resp_spec=ResponseSpec}};
    false ->
      io:format("Oopsie: ~p~n", [Command])
  end;

handle_command(Command, ResponseSpec, #state{sock=Sock}=State) when length(Command) == 3 ->
  case lists:nth(3, Command) of
    Sz when is_number(Sz) ->
      inet:setopts(Sock, [binary, {packet, raw}]),
      prim_inet:async_recv(Sock, Sz, -1),
      {noreply, State#state{command=chop(Command), resp_spec=ResponseSpec}};
    false ->
      io:format("Oopsie: ~p~n", [Command])
  end;

handle_command({get, _Key}=Command, ResponseSpec, #state{db=Db, sock=Sock}=State) ->
  Response = gen_server:call(Db, Command),
  membox_response:send(Response, ResponseSpec, Sock),
  {noreply, reset_state(State)};

handle_command(Command, ResponseSpec, #state{db=Db, sock=Sock}=State) ->
  Response = gen_server:call(Db, list_to_tuple(Command)),
  membox_response:send(Response, ResponseSpec, Sock),
  {noreply, reset_state(State)}.

chop(Command) ->
  chop(Command, 2).

chop(Command, 2) ->
  [Cmd, Key|_] = Command,
  [Cmd, Key];
chop(Command, 3) ->
  [Cmd, Key, Index|_] = Command,
  [Cmd, Key, Index].

reset_state(#state{sock=Sock}=State) ->
  inet:setopts(Sock, [{packet, line}, {active, once}]),
  State#state{complete=false,
              command=undefined,
              resp_spec=undefined}.
