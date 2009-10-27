-module(membox_db).

-include("membox_db.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tid}).

start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name], [{fullsweep_after, 1000}]).

init([TableName]) ->
  Tid = ets:new(TableName, [set]),
  {ok, #state{tid=Tid}}.

handle_call({set, Key, Value}, _From, #state{tid=Tid}=State) ->
  ets:insert(Tid, {Key, #membox_entry{type=string,
                                      value=Value}}),
  {reply, ok, State};

handle_call({get, Key}, _From, #state{tid=Tid}=State) ->
  Reply = case lookup(Tid, Key, string) of
            Entry when is_record(Entry, membox_entry) ->
              Entry#membox_entry.value;
            V ->
              V
          end,
  {reply, Reply, State};

handle_call({getset, Key, Value}, _From, #state{tid=Tid}=State) ->
  Reply = case lookup(Tid, Key, string) of
            error ->
              error;
            not_found ->
              ets:insert(Tid, {Key, #membox_entry{type=string,
                                                  value=Value}}),
              not_found;
            Entry ->
              ets:insert(Tid, {Key, #membox_entry{type=string,
                                                  value=Value}}),
              Entry#membox_entry.value
          end,
  {reply, Reply, State};

handle_call({mget, Keys}, _From, #state{tid=Tid}=State) ->
  Values = lists:map(fun(Key) ->
                         case lookup(Tid, Key, string) of
                           E when E =:= not_found orelse E =:= error ->
                             E;
                           E ->
                             E#membox_entry.value
                         end end, Keys),
  {reply, Values, State};

handle_call({setnx, Key, Value}, _From, #state{tid=Tid}=State) ->
  Reply = case lookup(Tid, Key, string) of
            not_found ->
              ets:insert(Tid, {Key, #membox_entry{type=string,
                                                  value=Value}}),
              true;
            _ ->
              false
          end,
  {reply, Reply, State};

handle_call({incr, Key}, _From, #state{tid=Tid}=State) ->
  Reply = case lookup(Tid, Key, string) of
            not_found ->
              ets:insert(Tid, {Key, #membox_entry{type=string,
                                                  value= <<"0">>}}),
              0;
            error ->
              error;
          Entry ->
              NewValue = binary_to_integer(Entry#membox_entry.value) + 1,
              ets:insert(Tid, {Key, Entry#membox_entry{value=integer_to_binary(NewValue)}}),
              NewValue
          end,
  {reply, Reply, State};

handle_call({incrby, Key, Amount}, _From, #state{tid=Tid}=State) ->
  Reply = case lookup(Tid, Key, string) of
            not_found ->
              ets:insert(Tid, {Key, #membox_entry{type=string,
                                                  value= <<"0">>}}),
              0;
            error ->
              error;
          Entry ->
              NewValue = binary_to_integer(Entry#membox_entry.value) + Amount,
              ets:insert(Tid, {Key, Entry#membox_entry{value=integer_to_binary(NewValue)}}),
              NewValue
          end,
  {reply, Reply, State};

handle_call({decr, Key}, _From, #state{tid=Tid}=State) ->
  Reply = case lookup(Tid, Key, string) of
            not_found ->
              ets:insert(Tid, {Key, #membox_entry{type=string,
                                                  value= <<"0">>}}),
              0;
            error ->
              error;
          Entry ->
              NewValue = binary_to_integer(Entry#membox_entry.value) - 1,
              ets:insert(Tid, {Key, Entry#membox_entry{value=integer_to_binary(NewValue)}}),
              NewValue
          end,
  {reply, Reply, State};

handle_call({decrby, Key, Amount}, _From, #state{tid=Tid}=State) ->
  Reply = case lookup(Tid, Key, string) of
            not_found ->
              ets:insert(Tid, {Key, #membox_entry{type=string,
                                                  value= <<"0">>}}),
              0;
            error ->
              error;
          Entry ->
              NewValue = binary_to_integer(Entry#membox_entry.value) - Amount,
              ets:insert(Tid, {Key, Entry#membox_entry{value=integer_to_binary(NewValue)}}),
              NewValue
          end,
  {reply, Reply, State};

handle_call({exists, Key}, _From, #state{tid=Tid}=State) ->
  Reply = case lookup(Tid, Key, any) of
            not_found ->
              false;
            _ ->
              true
          end,
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  {reply, ignore, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
lookup(Tid, Key, Type) ->
  Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  try
    begin
      Entry = ets:lookup_element(Tid, Key, 2),
      case has_expired(Entry, Now) of
        true ->
          ets:delete(Tid, Key),
          not_found;
        false ->
          case is_correct_type(Entry, Type) of
            true ->
              Entry;
            false ->
              error
          end
      end
    end
  catch
    error:badarg ->
      not_found
  end.

is_correct_type(_Entry, all) ->
  true;
is_correct_type(Entry, Type) ->
  Entry#membox_entry.type =:= Type.

has_expired(Entry, Now) when is_record(Entry, membox_entry) ->
  if
    Entry#membox_entry.expiry > -1 andalso
    Entry#membox_entry.expiry < Now ->
      true;
    true ->
      false
  end;
has_expired(_Entry, _Now) ->
  false.

binary_to_integer(B) ->
  try
    list_to_integer(binary_to_list(B))
  catch
    error:badarg ->
      0
  end.

integer_to_binary(N) ->
  list_to_binary(integer_to_list(N)).
