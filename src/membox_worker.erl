-module(membox_worker).

-export([start_link/1]).

start_link(Sock) ->
	Pid = proc_lib:spawn_link(fun() -> initialize_loop(Sock) end),
	receive
		{ack, _, {ok, Pid}} ->
			gen_tcp:controlling_process(Sock, Pid),
			Pid ! start,
			{ok, Pid};
		Error ->
			Error
	end.

initialize_loop(Sock) ->
	proc_lib:init_ack({ok, self()}),
	receive
		start ->
			inet:setopts(Sock, [{active, once}]),
			put(data_port, Sock),
			main_loop()
	end.

main_loop() ->
	receive
		{tcp, Sock, Command} ->
			inet:setopts(Sock, [{packet, raw}]),
			case send_reply(membox_parser:execute(Command), Sock) of
				ok ->
					inet:setopts(Sock, [{packet, line},
															{active, once}]),
					main_loop();
				stop ->
					ok
			end;
		{tcp_closed, _Sock} ->
			error_logger:info_msg("Client closed connection~n"),
			ok;
		{tcp_error, _Sock, Reason} ->
			error_logger:info_msg("TCP Error: ~p~n", [Reason]),
			ok
	end.

send_reply({ok, {set, ok}}, Sock) ->
	gen_tcp:send(Sock, "+OK\r\n"),
	ok;
send_reply({ok, {get, Value}}, Sock) ->
	Reply = io_lib:format("\$~p\r\n~s\r\n", [length(Value), Value]),
	gen_tcp:send(Sock, Reply),
	ok;

%% Lexer error
send_reply({error, _, _}, Sock) ->
	gen_tcp:close(Sock),
	stop;

send_reply(Reply, Sock) ->
	io:format("~p~n", [Reply]),
	ok.
