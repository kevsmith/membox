-module(membox_parser_tests).

-define(parse(Txt), simple_parse(Txt ++ "\r\n")).

-include_lib("eunit/include/eunit.hrl").

string_test() ->
  [?assertMatch({[set, "foo", 7], status_ok}, ?parse("set foo 5")),
   ?assertMatch({[set, "100", 7], status_ok}, ?parse("set 100 5")),
   ?assertMatch({[get, "wubba"], bulk}, ?parse("get wubba")),
   ?assertMatch({[get, "wubbà"], bulk}, ?parse("get wubbà")),
   ?assertMatch({[getset, "test", 7], bulk}, ?parse("getset test 5")),
   ?assertMatch({[mget, ["foo", "bar", "baz"]], multi_bulk}, ?parse("mget foo bar baz")),
   ?assertMatch({[setnx, "test", 12], integer}, ?parse("setnx test 10")),
   ?assertMatch({[incr, "foo"], integer}, ?parse("incr foo")),
   ?assertMatch({[incrby, "foo", 5], integer}, ?parse("incrby foo 5")),
   ?assertMatch({[decr, "foo"], integer}, ?parse("decr foo")),
   ?assertMatch({[decrby, "foo", 5], integer}, ?parse("decrby foo 5")),
   ?assertMatch({[exists, "testing"], integer}, ?parse("exists testing")),
   ?assertMatch({[del, "bar"], integer}, ?parse("del bar")),
   ?assertMatch({[type, "t"], status_type}, ?parse("type t")),
   ?assertMatch({[set, "foo", 7], status_ok}, ?parse("SET foo 5")),
   ?assertMatch({[set, "100", 7], status_ok}, ?parse("SET 100 5")),
   ?assertMatch({[get, "wubba"], bulk}, ?parse("GET wubba")),
   ?assertMatch({[get, "wubbà"], bulk}, ?parse("GET wubbà")),
   ?assertMatch({[getset, "test", 7], bulk}, ?parse("GETSET test 5")),
   ?assertMatch({[mget, ["foo", "bar", "baz"]], multi_bulk}, ?parse("MGET foo bar baz")),
   ?assertMatch({[setnx, "test", 12], integer}, ?parse("SETNX test 10")),
   ?assertMatch({[incr, "foo"], integer}, ?parse("INCR foo")),
   ?assertMatch({[incrby, "foo", 5], integer}, ?parse("INCRBY foo 5")),
   ?assertMatch({[decr, "foo"], integer}, ?parse("DECR foo")),
   ?assertMatch({[decrby, "foo", 5], integer}, ?parse("DECRBY foo 5")),
   ?assertMatch({[exists, "testing"], integer}, ?parse("EXISTS testing")),
   ?assertMatch({[del, "bar"], integer}, ?parse("DEL bar")),
   ?assertMatch({[type, "t"], status_type}, ?parse("TYPE t"))].

keyspace_test() ->
  [?assertMatch({[keys, "f?o"], bulk_key_string}, ?parse("keys f?o")),
   ?assertMatch({randomkey, single_line}, ?parse("randomkey")),
   ?assertMatch({[rename, "foo", "bar"], status_ok}, ?parse("rename foo bar")),
   ?assertMatch({[renamenx, "foo", "bar"], integer}, ?parse("renamenx foo bar")),
   ?assertMatch({dbsize, integer}, ?parse("dbsize")),
   ?assertMatch({[expire, "test", 300], integer}, ?parse("expire test 300")),
   ?assertMatch({[ttl, "foo"], integer}, ?parse("ttl foo")),
   ?assertMatch({[keys, "f?o"], bulk_key_string}, ?parse("KEYS f?o")),
   ?assertMatch({randomkey, single_line}, ?parse("RANDOMKEY")),
   ?assertMatch({[rename, "foo", "bar"], status_ok}, ?parse("RENAME foo bar")),
   ?assertMatch({[renamenx, "foo", "bar"], integer}, ?parse("RENAMENX foo bar")),
   ?assertMatch({dbsize, integer}, ?parse("DBSIZE")),
   ?assertMatch({[expire, "test", 300], integer}, ?parse("EXPIRE test 300")),
   ?assertMatch({[ttl, "foo"], integer}, ?parse("TTL foo"))].

list_test() ->
  [?assertMatch({[rpush, "foo", 5], status_ok}, ?parse("rpush foo 3")),
   ?assertMatch({[lpush, "bar", 12], status_ok}, ?parse("lpush bar 10")),
   ?assertMatch({[llen, "test"], integer}, ?parse("llen test")),
   ?assertMatch({[lrange, "bar", 1, 36], multi_bulk}, ?parse("lrange bar 1 36")),
   ?assertMatch({[ltrim, "test", 0, 18], status_ok}, ?parse("ltrim test 0 18")),
   ?assertMatch({[lindex, "foo", 51283], bulk}, ?parse("lindex foo 51283")),
   ?assertMatch({[lset, "bar", 10, 12], status_ok}, ?parse("lset bar 10 10")),
   ?assertMatch({[lrem, "test", 3, 7], integer}, ?parse("lrem test 3 5")),
   ?assertMatch({[lrem, "test", -1, 7], integer}, ?parse("lrem test -1 5")),
   ?assertMatch({[lpop, "foo"], bulk}, ?parse("lpop foo")),
   ?assertMatch({[rpop, "bar"], bulk}, ?parse("rpop bar")),
   ?assertMatch({[rpush, "foo", 5], status_ok}, ?parse("RPUSH foo 3")),
   ?assertMatch({[lpush, "bar", 12], status_ok}, ?parse("LPUSH bar 10")),
   ?assertMatch({[llen, "test"], integer}, ?parse("LLEN test")),
   ?assertMatch({[lrange, "bar", 1, 36], multi_bulk}, ?parse("LRANGE bar 1 36")),
   ?assertMatch({[ltrim, "test", 0, 18], status_ok}, ?parse("LTRIM test 0 18")),
   ?assertMatch({[lindex, "foo", 51283], bulk}, ?parse("LINDEX foo 51283")),
   ?assertMatch({[lset, "bar", 10, 12], status_ok}, ?parse("LSET bar 10 10")),
   ?assertMatch({[lrem, "test", 3, 7], integer}, ?parse("LREM test 3 5")),
   ?assertMatch({[lrem, "test", -1, 7], integer}, ?parse("LREM test -1 5")),
   ?assertMatch({[lpop, "foo"], bulk}, ?parse("LPOP foo")),
   ?assertMatch({[rpop, "bar"], bulk}, ?parse("RPOP bar"))].

set_test() ->
  [?assertMatch({[sadd, "foo", 5], integer}, ?parse("sadd foo 3")),
   ?assertMatch({[srem, "foo", 12], integer}, ?parse("srem foo 10")),
   ?assertMatch({[spop, "bar"], bulk}, ?parse("spop bar")),
   ?assertMatch({[smove, "foo", "bar", 12], integer}, ?parse("smove foo bar 10")),
   ?assertMatch({[scard, "test"], integer}, ?parse("scard test")),
   ?assertMatch({[sismember, "test", 17], integer}, ?parse("sismember test 15")),
   ?assertMatch({[sinter, ["foo", "bar", "baz"]], multi_bulk}, ?parse("sinter foo bar baz")),
   ?assertMatch({[sinterstore, "quux", ["foo", "bar", "baz"]], status_ok}, ?parse("sinterstore quux foo bar baz")),
   ?assertMatch({[sunion, ["foo", "bar", "baz"]], multi_bulk}, ?parse("sunion foo bar baz")),
   ?assertMatch({[sunionstore, "quux", ["foo", "bar", "baz"]], status_ok}, ?parse("sunionstore quux foo bar baz")),
   ?assertMatch({[sdiff, ["foo", "bar", "baz"]], multi_bulk}, ?parse("sdiff foo bar baz")),
   ?assertMatch({[sdiffstore, "test", ["1", "2", "3"]], status_ok}, ?parse("sdiffstore test 1 2 3")),
   ?assertMatch({[smembers, "foo"], multi_bulk}, ?parse("smembers foo")),
   ?assertMatch({[sadd, "foo", 5], integer}, ?parse("SADD foo 3")),
   ?assertMatch({[srem, "foo", 12], integer}, ?parse("SREM foo 10")),
   ?assertMatch({[spop, "bar"], bulk}, ?parse("SPOP bar")),
   ?assertMatch({[smove, "foo", "bar", 12], integer}, ?parse("SMOVE foo bar 10")),
   ?assertMatch({[scard, "test"], integer}, ?parse("SCARD test")),
   ?assertMatch({[sismember, "test", 17], integer}, ?parse("SISMEMBER test 15")),
   ?assertMatch({[sinter, ["foo", "bar", "baz"]], multi_bulk}, ?parse("SINTER foo bar baz")),
   ?assertMatch({[sinterstore, "quux", ["foo", "bar", "baz"]], status_ok}, ?parse("SINTERSTORE quux foo bar baz")),
   ?assertMatch({[sunion, ["foo", "bar", "baz"]], multi_bulk}, ?parse("SUNION foo bar baz")),
   ?assertMatch({[sunionstore, "quux", ["foo", "bar", "baz"]], status_ok}, ?parse("SUNIONSTORE quux foo bar baz")),
   ?assertMatch({[sdiff, ["foo", "bar", "baz"]], multi_bulk}, ?parse("SDIFF foo bar baz")),
   ?assertMatch({[sdiffstore, "test", ["1", "2", "3"]], status_ok}, ?parse("SDIFFSTORE test 1 2 3")),
   ?assertMatch({[smembers, "foo"], multi_bulk}, ?parse("SMEMBERS foo"))].

db_test() ->
  [?assertMatch({[select, 1], status_ok}, ?parse("select 1")),
   ?assertMatch({[move, "foo", 2], integer}, ?parse("move foo 2")),
   ?assertMatch({flush, status_ok}, ?parse("flushdb")),
   ?assertMatch({flushall, status_ok}, ?parse("flushall")),
   ?assertMatch({[auth, "testing123"], status_ok}, ?parse("auth testing123")),
   ?assertMatch({quit, close}, ?parse("quit")),
   ?assertMatch({[save], status_ok}, ?parse("save")),
   ?assertMatch({[bgsave], status_ok}, ?parse("bgsave")),
   ?assertMatch({lastsave, integer}, ?parse("lastsave")),
   ?assertMatch({shutdown, close}, ?parse("shutdown")),
   ?assertMatch({[select, 1], status_ok}, ?parse("SELECT 1")),
   ?assertMatch({[move, "foo", 2], integer}, ?parse("MOVE foo 2")),
   ?assertMatch({flush, status_ok}, ?parse("FLUSHDB")),
   ?assertMatch({flushall, status_ok}, ?parse("FLUSHALL")),
   ?assertMatch({[auth, "testing123"], status_ok}, ?parse("AUTH testing123")),
   ?assertMatch({quit, close}, ?parse("QUIT")),
   ?assertMatch({[save], status_ok}, ?parse("SAVE")),
   ?assertMatch({[bgsave], status_ok}, ?parse("BGSAVE")),
   ?assertMatch({lastsave, integer}, ?parse("LASTSAVE")),
   ?assertMatch({shutdown, close}, ?parse("SHUTDOWN"))].

server_test() ->
  [?assertMatch({info, info_bulk}, ?parse("info")),
   ?assertMatch({monitor, pipe}, ?parse("monitor")),
   ?assertMatch({[slaveof, "test.foo.com", 6379], status_ok}, ?parse("slaveof test.foo.com 6379")),
   ?assertMatch({[slaveof, "test.foo.com", 6379], status_ok}, ?parse("slaveof test.foo.com")),
   ?assertMatch({info, info_bulk}, ?parse("INFO")),
   ?assertMatch({monitor, pipe}, ?parse("MONITOR")),
   ?assertMatch({[slaveof, "test.foo.com", 6379], status_ok}, ?parse("SLAVEOF test.foo.com 6379")),
   ?assertMatch({[slaveof, "test.foo.com", 6379], status_ok}, ?parse("SLAVEOF test.foo.com 6379"))].

simple_parse(Txt) ->
  case membox_parser:parse_string(Txt) of
    {ok, Result} ->
      Result;
    Error ->
      Error
  end.
