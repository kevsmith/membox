-module(membox_db).

-include("membox_db.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2, bgsave/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {data_tid,
                keys_tid,
                bgsave_worker,
                dump_dir,
                save_ref}).

start_link(Name, Config) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, Config], [{fullsweep_after, 1000}]).

bgsave(Target) ->
  gen_server:call(Target, {bgsave}).

init([TableName, Config]) ->
  DataTid = ets:new(TableName, [set]),
  KeysTid = ets:new(list_to_atom(atom_to_list(TableName) ++ "_keys"), [set]),
  ets:insert(KeysTid, {<<"keys">>, 0}),
  {ok, #state{data_tid=DataTid,
              keys_tid=KeysTid,
              dump_dir=proplists:get_value(save_dir, Config, "/tmp"),
              save_ref=start_save_timer(proplists:get_value(save_interval, Config, 0))}}.

handle_call({set, Key, Value}, _From, State) ->
  insert(State, Key, #membox_entry{type=string,
                                   value=Value}),
  {reply, ok, State};

handle_call({get, Key}, _From, State) ->
  Handlers = [{found, fun(Entry) -> Entry#membox_entry.value end}],
  {reply, with_query(State, Key, string, Handlers), State};

handle_call({getset, Key, Value}, _From, State) ->
  Handlers = [{found, fun(Entry) -> insert(State, Key, #membox_entry{type=string,
                                                                     value=Value}),
                                    Entry#membox_entry.value end},
              {not_found, fun(_) -> insert(State, Key, #membox_entry{type=string,
                                                                     value=Value}),
                                    not_found end}],
  {reply, with_query(State, Key, string, Handlers), State};

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
  Handlers = [{not_found, fun(_) -> insert(State, Key, #membox_entry{type=string,
                                                                     value=Value}),
                                    true end},
              {found, fun(_) -> false end}],
  {reply, with_query(State, Key, string, Handlers), State};

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
  Handlers = [{not_found, fun(_) -> none end},
              {found, fun(Entry) -> Entry#membox_entry.type end}],
  {reply, with_query(State, Key, all, Handlers), State};

handle_call({del, Key}, _From, State) ->
  {reply, ok =:= delete(State, Key), State};

handle_call({exists, Key}, _From, #state{data_tid=DataTid}=State) ->
  {reply, ets:member(DataTid, Key), State};

handle_call({keys, Pattern}, _From, #state{data_tid=DataTid}=State) ->
  Results = case re:compile(Pattern, [extended, anchored]) of
              {ok, CPattern} ->
                ets:safe_fixtable(DataTid, true),
                find_keys(ets:first(DataTid), DataTid, CPattern, []);
              _ ->
                error
            end,
  Reply = case Results of
            [] ->
              not_found;
            Matches ->
              Matches
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
  Handlers = [{not_found, fun(_) -> false end},
              {found, fun(Entry) -> case Entry#membox_entry.expiry of
                                      -1 ->
                                        insert(State, Key, Entry#membox_entry{expiry=now_to_secs() + Seconds}),
                                        true;
                                      _ ->
                                        false
                                    end end}],
  {reply, with_query(State, Key, all, Handlers), State};

handle_call({ttl, Key}, _From, State) ->
  Handlers = [{not_found, fun(_) -> -1 end},
              {found, fun(Entry) -> Entry#membox_entry.expiry - now_to_secs() end}],
  {reply, with_query(State, Key, all, Handlers), State};

handle_call({rpush, Key, Value}, _From, State) ->
  Handlers = [{not_found, fun(_) -> insert(State, Key, #membox_entry{type=list, value=[Value]}),
                                    ok end},
              {found, fun(Entry) -> insert(State, Key, Entry#membox_entry{value=Entry#membox_entry.value ++ [Value]}),
                                    ok end}],
  {reply, with_query(State, Key, list, Handlers), State};

handle_call({lpush, Key, Value}, _From, State) ->
  Handlers = [{not_found, fun(_) -> insert(State, Key, #membox_entry{type=list, value=[Value]}),
                                    ok end},
              {found, fun(Entry) -> insert(State, Key, Entry#membox_entry{value=[Value|Entry#membox_entry.value]}),
                                    ok end}],
  {reply, with_query(State, Key, list, Handlers), State};

handle_call({llen, Key}, _From, State) ->
  Handlers = [{found, fun(Entry) -> length(Entry#membox_entry.value) end}],
  {reply, with_query(State, Key, list, Handlers), State};

handle_call({lrange, Key, Start, End}, _From, State) ->
  Handlers = [{found, fun(Entry) -> lists:sublist(Entry#membox_entry.value, Start + 1, End + 1) end}],
  {reply, with_query(State, Key, list, Handlers), State};

handle_call({ltrim, Key, Start, End}, _From, State) ->
  Handlers = [{found, fun(Entry) -> NewValue = lists:sublist(Entry#membox_entry.value, Start + 1, End + 1),
                                    insert(State, Key, Entry#membox_entry{value=NewValue}),
                                    ok end}],
  {reply, with_query(State, Key, list, Handlers), State};

handle_call({lindex, Key, Index}, _From, State) ->
  Handlers = [{found, fun(Entry) -> lists:nth(Index + 1, Entry#membox_entry.value) end}],
  {reply, with_query(State, Key, list, Handlers), State};

handle_call({lset, Key, Index, Value}, _From, State) ->
  Handlers = [{found, fun(Entry) -> #membox_entry{value=Values}=Entry,
                                    case set_member(Values, Index + 1, Value) of
                                      error ->
                                        error;
                                      NewValues ->
                                        insert(State, Key, Entry#membox_entry{value=NewValues}),
                                        ok
                                    end end}],
  {reply, with_query(State, Key, list, Handlers), State};

handle_call({lrem, Key, Count, Value}, _From, State) ->
  Handlers = [{found, fun(Entry) -> #membox_entry{value=Values}=Entry,
                                    NewValues = if
                                                  Count == 0 ->
                                                    lists:filter(fun(E) -> E =:= Value end, Values);
                                                  true ->
                                                    filter_members(Values, Value, Count)
                                                end,
                                    insert(State, Key, Entry#membox_entry{value=NewValues}),
                                    length(Values) - length(NewValues) end}],
  {reply, with_query(State, Key, list, Handlers), State};

handle_call({lpop, Key}, _From, State) ->
  Handlers = [{found, fun(Entry) -> #membox_entry{value=Values}=Entry,
                                    if
                                      length(Values) == 0 ->
                                        nil;
                                      true ->
                                        [H|T] = Values,
                                        insert(State, Key, Entry#membox_entry{value=T}),
                                        H
                                    end end}],
  {reply, with_query(State, Key, list, Handlers), State};

handle_call({rpop, Key}, _From, State) ->
  Handlers = [{found, fun(Entry) -> #membox_entry{value=Values}=Entry,
                                    if
                                      length(Values) == 0 ->
                                        nil;
                                      true ->
                                        [H|T] = lists:reverse(Values),
                                        insert(State, Key, Entry#membox_entry{value=lists:reverse(T)}),
                                        H
                                    end end}],
  {reply, with_query(State, Key, list, Handlers), State};

%% Persistence control commands
handle_call({save}, _From, #state{dump_dir=DumpDir, keys_tid=Keys, data_tid=Data}=State) ->
  ets:tab2file(Keys, filename:join([DumpDir, "keys.rdb"])),
  ets:tab2file(Data, filename:join([DumpDir, "data.rdb"])),
  {reply, ok, State};

handle_call({bgsave}, _From, #state{bgsave_worker=Worker}=State) when is_pid(Worker) ->
  {reply, ok, State};
handle_call({bgsave}, _From, #state{dump_dir=DumpDir, keys_tid=Keys, data_tid=Data}=State) ->
  Pid = proc_lib:spawn(fun() ->
                           ets:safe_fixtable(Keys, true),
                           ets:safe_fixtable(Data, true),
                           try
                             dump_table(filename:join([DumpDir, "keys.rdb"]), Keys),
                             dump_table(filename:join([DumpDir, "data.rdb"]), Data)
                           catch
                             Type:Exception ->
                               throw({Type, Exception})
                           after
                             ets:safe_fixtable(Data, false),
                             ets:safe_fixtable(Keys, false)
                           end end),
  erlang:monitor(process, Pid),
  {reply, ok, State#state{bgsave_worker=Pid}};
handle_call({lastsave}, _From, #state{dump_dir=DumpDir}=State) ->
  DataFile = filename:join([DumpDir, "data.rdb"]),
  Reply = case file:read_file_info(DataFile) of
            {error, _Error} ->
              0;
            {ok, Info} ->
              membox_util:datetime_to_unix_ts(Info#file_info.mtime)
          end,
  {reply, Reply, State};

handle_call({flush}, _From, #state{keys_tid=Keys, data_tid=Data}=State) ->
  ets:delete_all_objects(Keys),
  ets:insert(Keys, {<<"keys">>, 0}),
  ets:delete_all_objects(Data),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ignore, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _, _, WorkerPid, _}, #state{bgsave_worker=WorkerPid}=State) ->
  {noreply, State#state{bgsave_worker=undefined}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{save_ref=SaveTimer}) ->
  case SaveTimer of
    undefined ->
      ok;
    _ ->
      timer:cancel(SaveTimer)
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
dump_table(FileName, Tid) ->
  file:delete(FileName),
  {ok, DumpTid} = dets:open_file(FileName, [{type, set}]),
  dets:delete_all_objects(DumpTid),
  dump_data(DumpTid, ets:first(Tid), Tid).

dump_data(DumpTid, '$end_of_table', _DataTid) ->
  dets:close(DumpTid);
dump_data(DumpTid, Key, DataTid) ->
  [Value] = ets:lookup(DataTid, Key),
  dets:insert(DumpTid, Value),
  dump_data(DumpTid, ets:next(DataTid, Key), DataTid).

filter_members(Data, FilterVal, Count) ->
  if
    Count < 0 ->
      lists:reverse(filter_members(lists:reverse(Data), FilterVal, Count * -1, []));
    true ->
      filter_members(Data, FilterVal, Count, [])
  end.

filter_members(Data, _FilterVal, 0, Accum) ->
  lists:reverse(Accum) ++ Data;
filter_members([], _FilterVal, Count, Accum) ->
  Count, lists:reverse(Accum);
filter_members([FilterVal|T], FilterVal, Count, Accum) ->
  filter_members(T, FilterVal, Count - 1, Accum);
filter_members([H|T], FilterVal, Count, Accum) ->
  filter_members(T, FilterVal, Count, [H|Accum]).

set_member(Data, Position, Item) ->
  try
    {Before, A} = lists:split(Position - 1, Data),
    {_, After} = lists:split(1, A),
    lists:flatten([Before, Item, After])
  catch
    error:badarg ->
      error
  end.

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

with_query(State, Key, Type, Funs) ->
  exec_handler(lookup(State, Key, Type), Funs).

exec_handler(Result, Funs) when Result =:= not_found;
                                    Result =:= error ->
  F = proplists:get_value(Result, Funs, ?IDENTITY_FUN),
  F(Result);
exec_handler(Result, Funs) ->
  F = proplists:get_value(found, Funs, ?IDENTITY_FUN),
  F(Result).

start_save_timer(0) ->
  undefined;
start_save_timer(N) ->
  {ok, Ref} = timer:apply_interval(N, ?MODULE, bgsave, [self()]),
  Ref.
