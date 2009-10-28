-module(membox_db_tests).

-include_lib("eunit/include/eunit.hrl").

get_set_test_() ->
  {setup, fun() -> membox_db:start_link(db1) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [?_assertMatch(ok, gen_server:call(db1, {set, <<"foo">>, <<"abc">>})),
    ?_assertMatch(<<"abc">>, gen_server:call(db1, {get, <<"foo">>})),
    ?_assertMatch(not_found, gen_server:call(db1, {get, <<"bar">>})),
    ?_assertMatch(<<"abc">>, gen_server:call(db1, {getset, <<"foo">>, <<"def">>})),
    ?_assertMatch(not_found, gen_server:call(db1, {getset, <<"wubba">>, <<"testing">>})),
    ?_assertMatch([<<"def">>, <<"testing">>], gen_server:call(db1, {mget, [<<"foo">>, <<"wubba">>]})),
    ?_assertMatch(false, gen_server:call(db1, {setnx, <<"foo">>, <<"new_value">>}))]}.

incr_decr_test_() ->
  {setup, fun() -> membox_db:start_link(db1) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [?_assertMatch(1, gen_server:call(db1, {incr, <<"foo">>})),
    ?_assertMatch(-1, gen_server:call(db1, {decr, <<"bar">>})),
    ?_assertMatch(2, gen_server:call(db1, {incr, <<"foo">>})),
    ?_assertMatch(-2, gen_server:call(db1, {decr, <<"bar">>})),
    ?_assertMatch(4, gen_server:call(db1, {incrby, <<"foo">>, 2})),
    ?_assertMatch(-4, gen_server:call(db1, {decrby, <<"bar">>, 2}))]}.

exists_test_() ->
  {setup, fun() -> membox_db:start_link(db1) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [?_assertMatch(false, gen_server:call(db1, {exists, <<"foo">>})),
    fun() ->
        gen_server:call(db1, {set, <<"foo">>, <<"abc">>}),
        ?assertMatch(true, gen_server:call(db1, {exists, <<"foo">>})) end]}.

type_test_() ->
  {setup, fun() -> membox_db:start_link(db1) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [?_assertMatch(none, gen_server:call(db1, {type, <<"foo">>})),
    fun() ->
        gen_server:call(db1, {set, <<"foo">>, <<"0">>}),
        ?assertMatch(string, gen_server:call(db1, {type, <<"foo">>})) end]}.

del_test_() ->
  {setup, fun() -> membox_db:start_link(db1) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [fun() ->
        ok = gen_server:call(db1, {set, <<"foo">>, <<"bar">>}),
        ok = gen_server:call(db1, {del, <<"foo">>}),
        ?assertMatch(not_found, gen_server:call(db1, {get, <<"foo">>})),
        ?assertMatch([], gen_server:call(db1, {keys, <<"foo">>})) end]}.

keys_test_() ->
  {setup, fun() -> membox_db:start_link(db1) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [?_assertMatch([], gen_server:call(db1, {keys, <<"f?o">>})),
    fun() ->
        ok = gen_server:call(db1, {set, <<"foo">>, <<"bar">>}),
        ?assertMatch([<<"foo">>], gen_server:call(db1, {keys, <<"f?o">>})) end,
    fun() ->
       ok = gen_server:call(db1, {set, <<"fark">>, <<"rocked">>}),
       ?assertMatch(true, members([<<"foo">>, <<"fark">>], gen_server:call(db1, {keys, <<"f?.*">>}))) end,
    ?_assertMatch(true, is_binary(gen_server:call(db1, randomkey))),
    fun() ->
        ok = gen_server:call(db1, {set, <<"a">>, <<"a">>}),
        true = gen_server:call(db1, {renamenx, <<"a">>, <<"b">>}),
        <<"a">> = gen_server:call(db1, {get, <<"b">>}),
        ok = gen_server:call(db1, {set, <<"c">>, <<"d">>}),
        false = gen_server:call(db1, {renamenx, <<"c">>, <<"b">>}) end,
    fun() ->
        ok = gen_server:call(db1, {set, <<"1">>, <<"1">>}),
        ok = gen_server:call(db1, {set, <<"2">>, <<"2">>}),
        ok = gen_server:call(db1, {rename, <<"1">>, <<"3">>}),
        <<"1">> = gen_server:call(db1, {get, <<"3">>}),
        ok = gen_server:call(db1, {rename, <<"2">>, <<"3">>}),
        <<"2">> = gen_server:call(db1, {get, <<"3">>}) end,
    fun() ->
       ok = gen_server:call(db1, {set, <<"1">>, <<"12345">>}),
       true = gen_server:call(db1, {expire, <<"1">>, 60}) end]}.

size_test_() ->
  {setup, fun() -> membox_db:start_link(db1) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [fun() ->
        lists:foreach(fun(I) ->
                          V = list_to_binary(integer_to_list(I)),
                          gen_server:call(db1, {set, V, V}) end,
                      lists:seq(1, 1000)),
        1000 = gen_server:call(db1, dbsize) end]}.

teardown(Pid) ->
  unlink(Pid),
  exit(Pid, shutdown),
  timer:sleep(100).

members([], _Data) ->
  true;
members([H|T], Data) ->
  case lists:member(H, Data) of
    true ->
      members(T, Data);
    false ->
      false
  end.
