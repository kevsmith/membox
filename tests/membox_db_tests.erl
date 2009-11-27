-module(membox_db_tests).

-include_lib("eunit/include/eunit.hrl").

get_set_test_() ->
  {setup, fun() -> membox_db:start_link(db1, []) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [?_assertMatch(ok, gen_server:call(db1, {set, <<"foo">>, <<"abc">>})),
    ?_assertMatch(<<"abc">>, gen_server:call(db1, {get, <<"foo">>})),
    ?_assertMatch(not_found, gen_server:call(db1, {get, <<"bar">>})),
    ?_assertMatch(<<"abc">>, gen_server:call(db1, {getset, <<"foo">>, <<"def">>})),
    ?_assertMatch(not_found, gen_server:call(db1, {getset, <<"wubba">>, <<"testing">>})),
    ?_assertMatch([<<"def">>, <<"testing">>], gen_server:call(db1, {mget, [<<"foo">>, <<"wubba">>]})),
    ?_assertMatch(false, gen_server:call(db1, {setnx, <<"foo">>, <<"new_value">>}))]}.

incr_decr_test_() ->
  {setup, fun() -> membox_db:start_link(db1, []) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [?_assertMatch(1, gen_server:call(db1, {incr, <<"foo">>})),
    ?_assertMatch(-1, gen_server:call(db1, {decr, <<"bar">>})),
    ?_assertMatch(2, gen_server:call(db1, {incr, <<"foo">>})),
    ?_assertMatch(-2, gen_server:call(db1, {decr, <<"bar">>})),
    ?_assertMatch(4, gen_server:call(db1, {incrby, <<"foo">>, 2})),
    ?_assertMatch(-4, gen_server:call(db1, {decrby, <<"bar">>, 2}))]}.

exists_test_() ->
  {setup, fun() -> membox_db:start_link(db1, []) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [?_assertMatch(false, gen_server:call(db1, {exists, <<"foo">>})),
    fun() ->
        gen_server:call(db1, {set, <<"foo">>, <<"abc">>}),
        ?assertMatch(true, gen_server:call(db1, {exists, <<"foo">>})) end]}.

type_test_() ->
  {setup, fun() -> membox_db:start_link(db1, []) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [?_assertMatch(none, gen_server:call(db1, {type, <<"foo">>})),
    fun() ->
        gen_server:call(db1, {set, <<"foo">>, <<"0">>}),
        ?assertMatch(string, gen_server:call(db1, {type, <<"foo">>})) end]}.

del_test_() ->
  {setup, fun() -> membox_db:start_link(db1, []) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [fun() ->
        ok = gen_server:call(db1, {set, <<"foo">>, <<"bar">>}),
        true = gen_server:call(db1, {del, <<"foo">>}),
        ?assertMatch(not_found, gen_server:call(db1, {get, <<"foo">>})),
        ?assertMatch(not_found, gen_server:call(db1, {keys, <<"foo">>})) end]}.

keys_test_() ->
  {setup, fun() -> membox_db:start_link(db1, []) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [?_assertMatch(not_found, gen_server:call(db1, {keys, <<"f?o">>})),
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
  {setup, fun() -> membox_db:start_link(db1, []) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [fun() ->
        lists:foreach(fun(I) ->
                          V = list_to_binary(integer_to_list(I)),
                          gen_server:call(db1, {set, V, V}) end,
                      lists:seq(1, 1000)),
        1000 = gen_server:call(db1, dbsize) end]}.

ttl_test_() ->
  {setup, fun() -> membox_db:start_link(db1, []) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [fun() ->
        ok = gen_server:call(db1, {set, <<"ab">>, <<"def">>}),
        true = gen_server:call(db1, {expire, <<"ab">>, 30}),
        30 = gen_server:call(db1, {ttl, <<"ab">>}) end]}.

list_test_() ->
  {setup, fun() -> membox_db:start_link(db1, []) end,
   fun({ok, Pid}) -> teardown(Pid) end,
   [?_assertMatch(ok, gen_server:call(db1, {lpush, <<"foo">>, <<"abc">>})),
    ?_assertMatch(ok, gen_server:call(db1, {rpush, <<"foo">>, <<"def">>})),
    ?_assertMatch([<<"abc">>, <<"def">>], gen_server:call(db1, {lrange, <<"foo">>, 0, 1})),
    ?_assertMatch([<<"abc">>], gen_server:call(db1, {lrange, <<"foo">>, 0, 0})),
    ?_assertMatch(2, gen_server:call(db1, {llen, <<"foo">>})),
    ?_assertMatch(ok, gen_server:call(db1, {ltrim, <<"foo">>, 0, 0})),
    ?_assertMatch([<<"abc">>], gen_server:call(db1, {lrange, <<"foo">>, 0, 0})),
    ?_assertMatch(<<"abc">>, gen_server:call(db1, {lindex, <<"foo">>, 0})),
    ?_assertMatch(ok, gen_server:call(db1, {lset, <<"foo">>, 0, <<"qqq">>})),
    ?_assertMatch([<<"qqq">>], gen_server:call(db1, {lrange, <<"foo">>, 0, 0})),
    fun() ->
        ok = gen_server:call(db1, {lpush, <<"bar">>, <<"1">>}),
        ok = gen_server:call(db1, {lpush, <<"bar">>, <<"2">>}),
        ok = gen_server:call(db1, {lpush, <<"bar">>, <<"1">>}),
        ok = gen_server:call(db1, {lpush, <<"bar">>, <<"2">>}),
        2 = gen_server:call(db1, {lrem, <<"bar">>, 2, <<"1">>}) end,
    fun() ->
       ok = gen_server:call(db1, {lpush, <<"quux">>, <<"c">>}),
       ok = gen_server:call(db1, {lpush, <<"quux">>, <<"b">>}),
       ok = gen_server:call(db1, {lpush, <<"quux">>, <<"a">>}),
       <<"a">> = gen_server:call(db1, {lpop, <<"quux">>}),
       <<"c">> = gen_server:call(db1, {rpop, <<"quux">>}),
       [<<"b">>] = gen_server:call(db1, {lrange, <<"quux">>, 0, 0}) end,
    fun() ->
        ok = gen_server:call(db1, {flush}),
        setup_list(db1, <<"quux">>),
        ok = gen_server:call(db1, {ltrim, <<"quux">>, 1, 2}),
        [<<"b">>, <<"c">>] = gen_server:call(db1, {lrange, <<"quux">>, 0, 1}) end,
    fun() ->
        ok = gen_server:call(db1, {flush}),
        ok = gen_server:call(db1, {lpush, <<"foo">>, <<"a">>}),
        ok = gen_server:call(db1, {lpush, <<"foo">>, <<"a">>}),
        ok = gen_server:call(db1, {lpush, <<"foo">>, <<"b">>}),
        ?assertEqual(2, gen_server:call(db1, {lrem, <<"foo">>, 2, <<"a">>})) end,
    fun() ->
        ok = gen_server:call(db1, {flush}),
        setup_list2(db1, <<"foo">>),
        ?assertEqual(2, gen_server:call(db1, {lrem, <<"foo">>, -2, <<"a">>})),
        ?assertMatch([<<"a">>, <<"a">>, <<"b">>, <<"b">>], gen_server:call(db1, {lrange, <<"foo">>, 0, 3})) end,
    fun() ->
        ok = gen_server:call(db1, {flush}),
        setup_list(db1, <<"foo">>),
        ?assertMatch(ok, gen_server:call(db1, {lset, <<"foo">>, 1, <<"e">>})),
        ?assertMatch([<<"a">>, <<"e">>, <<"c">>, <<"d">>], gen_server:call(db1, {lrange, <<"foo">>, 0, 3})) end,
    fun() ->
        ok = gen_server:call(db1, {flush}),
        setup_list(db1, <<"foo">>),
        ?assertMatch(error, gen_server:call(db1, {lset, <<"foo">>, 5, <<"e">>})) end]}.

teardown(Pid) ->
  unlink(Pid),
  exit(Pid, shutdown),
  timer:sleep(100).

setup_list(Database, Key) ->
  ok = gen_server:call(Database, {lpush, Key, <<"d">>}),
  ok = gen_server:call(Database, {lpush, Key, <<"c">>}),
  ok = gen_server:call(Database, {lpush, Key, <<"b">>}),
  ok = gen_server:call(Database, {lpush, Key, <<"a">>}).

setup_list2(Database, Key) ->
  ok = gen_server:call(Database, {rpush, Key, <<"a">>}),
  ok = gen_server:call(Database, {rpush, Key, <<"a">>}),
  ok = gen_server:call(Database, {rpush, Key, <<"b">>}),
  ok = gen_server:call(Database, {rpush, Key, <<"a">>}),
  ok = gen_server:call(Database, {rpush, Key, <<"a">>}),
  ok = gen_server:call(Database, {rpush, Key, <<"b">>}).

members([], _Data) ->
  true;
members([H|T], Data) ->
  case lists:member(H, Data) of
    true ->
      members(T, Data);
    false ->
      false
  end.
