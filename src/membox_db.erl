-module(membox_db).

-include("membox_db.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {data_tid,
                keys_tid}).

start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name], [{fullsweep_after, 1000}]).

init([TableName]) ->
  DataTid = ets:new(TableName, [set]),
  KeysTid = ets:new(list_to_atom(atom_to_list(TableName) ++ "_keys"), [set]),
  ets:insert(KeysTid, {<<"keys">>, 0}),
  {ok, #state{data_tid=DataTid,
              keys_tid=KeysTid}}.

handle_call({set, Key, Value}, _From, State) ->
  insert(State, Key, #membox_entry{type=string,
                                   value=Value}),
  {reply, ok, State};

handle_call({get, Key}, _From, State) ->
  Reply = case lookup(State, Key, string) of
            Entry when is_record(Entry, membox_entry) ->
              Entry#membox_entry.value;
            V ->
              V
          end,
  {reply, Reply, State};

handle_call({getset, Key, Value}, _From, State) ->
  Reply = case lookup(State, Key, string) of
            error ->
              error;
            not_found ->
              insert(State, Key, #membox_entry{type=string,
                                               value=Value}),
              not_found;
            Entry ->
              insert(State, Key, #membox_entry{type=string,
                                               value=Value}),
              Entry#membox_entry.value
          end,
  {reply, Reply, State};

handle_call({mget, Keys}, _From, State) ->
  Values = lists:map(fun(Key) ->
                         case lookup(State, Key, string) of
                           E when E =:= not_found orelse E =:= error ->
                             E;
                           E ->
                             E#membox_entry.value
                         end end, Keys),
  {reply, Values, State};

handle_call({setnx, Key, Value}, _From, State) ->
  Reply = case lookup(State, Key, string) of
            not_found ->
              insert(State, Key, #membox_entry{type=string,
                                               value=Value}),
              true;
            _ ->
              false
          end,
  {reply, Reply, State};

handle_call({incr, Key}, _From, State) ->
  Reply = increment_key(State, Key, 1),
  {reply, Reply, State};

handle_call({incrby, Key, Amount}, _From, State) ->
  Reply = increment_key(State, Key, Amount),
  {reply, Reply, State};

handle_call({decr, Key}, _From, State) ->
  Reply = increment_key(State, Key, -1),
  {reply, Reply, State};

handle_call({decrby, Key, Amount}, _From, State) ->
  Reply = increment_key(State, Key, Amount * -1),
  {reply, Reply, State};

handle_call({type, Key}, _From, State) ->
  Reply = case lookup(State, Key, all) of
            not_found ->
              none;
            Entry ->
              Entry#membox_entry.type
          end,
  {reply, Reply, State};

handle_call({del, Key}, _From, State) ->
  {reply, delete(State, Key), State};

handle_call({exists, Key}, _From, #state{data_tid=DataTid}=State) ->
  {reply, ets:member(DataTid, Key), State};

handle_call({keys, Pattern}, _From, #state{data_tid=DataTid}=State) ->
  Reply = case re:compile(Pattern, [extended, anchored]) of
            {ok, CPattern} ->
              ets:safe_fixtable(DataTid, true),
              find_keys(ets:first(DataTid), DataTid, CPattern, []);
            _ ->
              error
          end,
  {reply, Reply, State};

handle_call(randomkey, _From, #state{keys_tid=KTid}=State) ->
  KeySize = ets:lookup_element(KTid, <<"keys">>, 2),
  KeyId = crypto:rand_uniform(1, KeySize),
  Key = ets:lookup_element(KTid, KeyId, 2),
  {reply, Key, State};

handle_call({rename, OldKey, NewKey}, _From, State) ->
  Reply = case lookup(State, OldKey, all) of
            not_found ->
              not_found;
            OldEntry ->
              case lookup(State, NewKey, all) of
                not_found ->
                  delete(State, OldKey),
                  insert(State, NewKey, OldEntry),
                  ok;
                NewEntry ->
                  delete(State, OldKey),
                  ets:insert(State#state.data_tid, {NewKey, OldEntry#membox_entry{keyid=NewEntry#membox_entry.keyid}}),
                  ok
              end
          end,
  {reply, Reply, State};

handle_call({renamenx, OldKey, NewKey}, _From, State) ->
  Reply = case lookup(State, OldKey, all) of
            not_found ->
              false;
            OldEntry ->
              case lookup(State, NewKey, all) of
                not_found ->
                  delete(State, OldKey),
                  insert(State, NewKey, OldEntry),
                  true;
                _NewEntry ->
                  false
              end
          end,
  {reply, Reply, State};

handle_call(dbsize, _From, #state{keys_tid=KTid}=State) ->
  %% Have to deduct counter entry
  {reply, ets:info(KTid, size) - 1, State};

handle_call({expire, Key, Seconds}, _From, State) ->
  Reply = case lookup(State, Key, all) of
            not_found ->
              false;
            Entry ->
              case Entry#membox_entry.expiry of
                -1 ->
                  insert(State, Key, Entry#membox_entry{expiry=now_to_secs() + Seconds}),
                  true;
                _ ->
                  false
              end
          end,
  {reply, Reply, State};

handle_call({ttl, Key}, _From, State) ->
  Reply = case lookup(State, Key, all) of
            not_found ->
              -1;
            Entry ->
              Entry#membox_entry.expiry
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
find_keys('$end_of_table', Tid, _, Accum) ->
  ets:safe_fixtable(Tid, false),
  Accum;
find_keys(Key, Tid, Pattern, Accum) ->
  NewAccum = case re:run(Key, Pattern) of
               {match, _} ->
                 [Key|Accum];
               _ ->
                 Accum
             end,
  find_keys(ets:next(Tid, Key), Tid, Pattern, NewAccum).

increment_key(State, Key, Amount) ->
 case lookup(State, Key, string) of
   not_found ->
     insert(State, Key, #membox_entry{type=string,
                                      value= integer_to_binary(Amount)}),
     Amount;
   error ->
     error;
   Entry ->
     NewValue = binary_to_integer(Entry#membox_entry.value) + Amount,
     insert(State, Key, Entry#membox_entry{value=integer_to_binary(NewValue)}),
     NewValue
 end.

lookup(#state{data_tid=DTid, keys_tid=KTid}, Key, Type) ->
  Now = now_to_secs(),
  try
    begin
      Entry = ets:lookup_element(DTid, Key, 2),
      case has_expired(Entry, Now) of
        true ->
          ets:delete(DTid, Key),
          ets:delete(KTid, Entry#membox_entry.keyid),
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

delete(#state{data_tid=DTid, keys_tid=KTid}=State, Key) ->
  case lookup(State, Key, all) of
    not_found ->
      ok;
    Entry ->
      ets:delete(KTid, Entry#membox_entry.keyid),
      ets:delete(DTid, Key),
      ok
  end.

insert(#state{data_tid=DTid, keys_tid=KTid}=State, Key, Entry) ->
  case lookup(State, Key, all) of
    not_found ->
      KeyId = ets:update_counter(KTid, <<"keys">>, {2, 1}),
      ets:insert(KTid, {KeyId, Key}),
      ets:insert(DTid, {Key, Entry#membox_entry{keyid=KeyId}});
    OldEntry ->
      ets:insert(DTid, {Key, Entry#membox_entry{keyid=OldEntry#membox_entry.keyid}})
  end.

now_to_secs() ->
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
