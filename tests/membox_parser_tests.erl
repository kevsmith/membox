-module(membox_parser_tests).

-define(parse(Txt), simple_parse(Txt ++ "\r\n")).

-include_lib("eunit/include/eunit.hrl").

list_test() ->
  [?assertMatch([{set, "foo", 7}, status_ok], ?parse("set foo 5")),
   ?assertMatch([{set, "100", 7}, status_ok], ?parse("set 100 5")),
   ?assertMatch([{get, "wubba"}, bulk], ?parse("get wubba")),
   ?assertMatch([{get, "wubbà"}, bulk], ?parse("get wubbà")),
   ?assertMatch([{getset, "test", 7}, bulk], ?parse("getset test 5")),
   ?assertMatch([{mget, ["foo", "bar", "baz"]}, multi_bulk], ?parse("mget foo bar baz")),
   ?assertMatch([{setnx, "test", 12}, integer], ?parse("setnx test 10")),
   ?assertMatch([{incr, "foo"}, integer], ?parse("incr foo")),
   ?assertMatch([{incrby, "foo", 5}, integer], ?parse("incrby foo 5")),
   ?assertMatch([{decr, "foo"}, integer], ?parse("decr foo")),
   ?assertMatch([{decrby, "foo", 5}, integer], ?parse("decrby foo 5")),
   ?assertMatch([{exists, "testing"}, integer], ?parse("exists testing")),
   ?assertMatch([{del, "bar"}, integer], ?parse("del bar")),
   ?assertMatch([{type, "t"}, status_type], ?parse("type t")),
   ?assertMatch([{set, "foo", 7}, status_ok], ?parse("SET foo 5")),
   ?assertMatch([{set, "100", 7}, status_ok], ?parse("SET 100 5")),
   ?assertMatch([{get, "wubba"}, bulk], ?parse("GET wubba")),
   ?assertMatch([{get, "wubbà"}, bulk], ?parse("GET wubbà")),
   ?assertMatch([{getset, "test", 7}, bulk], ?parse("GETSET test 5")),
   ?assertMatch([{mget, ["foo", "bar", "baz"]}, multi_bulk], ?parse("MGET foo bar baz")),
   ?assertMatch([{setnx, "test", 12}, integer], ?parse("SETNX test 10")),
   ?assertMatch([{incr, "foo"}, integer], ?parse("INCR foo")),
   ?assertMatch([{incrby, "foo", 5}, integer], ?parse("INCRBY foo 5")),
   ?assertMatch([{decr, "foo"}, integer], ?parse("DECR foo")),
   ?assertMatch([{decrby, "foo", 5}, integer], ?parse("DECRBY foo 5")),
   ?assertMatch([{exists, "testing"}, integer], ?parse("EXISTS testing")),
   ?assertMatch([{del, "bar"}, integer], ?parse("DEL bar")),
   ?assertMatch([{type, "t"}, status_type], ?parse("TYPE t"))].

simple_parse(Txt) ->
  case membox_parser:parse_string(Txt) of
    {ok, Result} ->
      Result;
    Error ->
      Error
  end.
