-module(membox_parser_tests).

-define(parse(Txt), simple_parse(Txt ++ "\r\n")).

-include_lib("eunit/include/eunit.hrl").

string_test() ->
  [?assertMatch({{set, "foo", 7}, status_ok}, ?parse("set foo 5")),
   ?assertMatch({{set, "100", 7}, status_ok}, ?parse("set 100 5")),
   ?assertMatch({{get, "wubba"}, bulk}, ?parse("get wubba")),
   ?assertMatch({{get, "wubbà"}, bulk}, ?parse("get wubbà")),
   ?assertMatch({{getset, "test", 7}, bulk}, ?parse("getset test 5")),
   ?assertMatch({{mget, ["foo", "bar", "baz"]}, multi_bulk}, ?parse("mget foo bar baz")),
   ?assertMatch({{setnx, "test", 12}, integer}, ?parse("setnx test 10")),
   ?assertMatch({{incr, "foo"}, integer}, ?parse("incr foo")),
   ?assertMatch({{incrby, "foo", 5}, integer}, ?parse("incrby foo 5")),
   ?assertMatch({{decr, "foo"}, integer}, ?parse("decr foo")),
   ?assertMatch({{decrby, "foo", 5}, integer}, ?parse("decrby foo 5")),
   ?assertMatch({{exists, "testing"}, integer}, ?parse("exists testing")),
   ?assertMatch({{del, "bar"}, integer}, ?parse("del bar")),
   ?assertMatch({{type, "t"}, status_type}, ?parse("type t")),
   ?assertMatch({{set, "foo", 7}, status_ok}, ?parse("SET foo 5")),
   ?assertMatch({{set, "100", 7}, status_ok}, ?parse("SET 100 5")),
   ?assertMatch({{get, "wubba"}, bulk}, ?parse("GET wubba")),
   ?assertMatch({{get, "wubbà"}, bulk}, ?parse("GET wubbà")),
   ?assertMatch({{getset, "test", 7}, bulk}, ?parse("GETSET test 5")),
   ?assertMatch({{mget, ["foo", "bar", "baz"]}, multi_bulk}, ?parse("MGET foo bar baz")),
   ?assertMatch({{setnx, "test", 12}, integer}, ?parse("SETNX test 10")),
   ?assertMatch({{incr, "foo"}, integer}, ?parse("INCR foo")),
   ?assertMatch({{incrby, "foo", 5}, integer}, ?parse("INCRBY foo 5")),
   ?assertMatch({{decr, "foo"}, integer}, ?parse("DECR foo")),
   ?assertMatch({{decrby, "foo", 5}, integer}, ?parse("DECRBY foo 5")),
   ?assertMatch({{exists, "testing"}, integer}, ?parse("EXISTS testing")),
   ?assertMatch({{del, "bar"}, integer}, ?parse("DEL bar")),
   ?assertMatch({{type, "t"}, status_type}, ?parse("TYPE t"))].

keyspace_test() ->
  [?assertMatch({{keys, "f?o"}, bulk}, ?parse("keys f?o")),
   ?assertMatch({randomkey, single_line}, ?parse("randomkey")),
   ?assertMatch({{rename, "foo", "bar"}, status_ok}, ?parse("rename foo bar")),
   ?assertMatch({{renamenx, "foo", "bar"}, integer}, ?parse("renamenx foo bar")),
   ?assertMatch({dbsize, integer}, ?parse("dbsize")),
   ?assertMatch({{expire, "test"}, integer}, ?parse("expire test")),
   ?assertMatch({{ttl, "foo"}, integer}, ?parse("ttl foo")),
   ?assertMatch({{keys, "f?o"}, bulk}, ?parse("KEYS f?o")),
   ?assertMatch({randomkey, single_line}, ?parse("RANDOMKEY")),
   ?assertMatch({{rename, "foo", "bar"}, status_ok}, ?parse("RENAME foo bar")),
   ?assertMatch({{renamenx, "foo", "bar"}, integer}, ?parse("RENAMENX foo bar")),
   ?assertMatch({dbsize, integer}, ?parse("DBSIZE")),
   ?assertMatch({{expire, "test"}, integer}, ?parse("EXPIRE test")),
   ?assertMatch({{ttl, "foo"}, integer}, ?parse("TTL foo"))].

list_test() ->
  [?assertMatch({{rpush, "foo", 5}, status_ok}, ?parse("rpush foo 3")),
   ?assertMatch({{lpush, "bar", 12}, status_ok}, ?parse("lpush bar 10")),
   ?assertMatch({{llen, "test"}, integer}, ?parse("llen test")),
   ?assertMatch({{lrange, "bar", 1, 36}, multi_bulk}, ?parse("lrange bar 1 36")),
   ?assertMatch({{ltrim, "test", 0, 18}, status_ok}, ?parse("ltrim test 0 18")),
   ?assertMatch({{lindex, "foo", 51283}, bulk}, ?parse("lindex foo 51283")),
   ?assertMatch({{lset, "bar", 10, 12}, status_ok}, ?parse("lset bar 10 10")),
   ?assertMatch({{lrem, "test", 3, 7}, integer}, ?parse("lrem test 3 5")),
   ?assertMatch({{lrem, "test", -1, 7}, integer}, ?parse("lrem test -1 5")),
   ?assertMatch({{lpop, "foo"}, bulk}, ?parse("lpop foo")),
   ?assertMatch({{rpop, "bar"}, bulk}, ?parse("rpop bar"))].

simple_parse(Txt) ->
  case membox_parser:parse_string(Txt) of
    {ok, Result} ->
      Result;
    Error ->
      Error
  end.
