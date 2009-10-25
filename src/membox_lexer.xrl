Definitions.

Rules.

%% String commands
set|SET					: {token, {set, TokenLine, "set"}}.
get|GET					: {token, {get, TokenLine, "get"}}.
getset|GETSET				: {token, {getset, TokenLine, "getset"}}.
setnx|SETNX				: {token, {setnx, TokenLine, "setnx"}}.
mget|MGET				: {token, {mget, TokenLine, "mget"}}.
setnx|SETNX				: {token, {setnx, TokenLine, "setnx"}}.
incr|INCR				: {token, {incr, TokenLine, "incr"}}.
decr|DECR				: {token, {decr, TokenLine, "decr"}}.
incrby|INCRBY				: {token, {incrby, TokenLine, "incrby"}}.
decrby|DECRBY				: {token, {decrby, TokenLine, "decrby"}}.
exists|EXISTS				: {token, {exists, TokenLine, "exists"}}.
del|DEL					: {token, {del, TokenLine, "del"}}.
type|TYPE				: {token, {type, TokenLine, "type"}}.

%% Keyspace commands
keys|KEYS				: {token, {keys, TokenLine, "keys"}}.
randomkey|RANDOMKEY			: {token, {randomkey, TokenLine, "randomkeys"}}.
rename|RENAME				: {token, {rename, TokenLine, "rename"}}.
renamenx|RENAMENX			: {token, {renamenx, TokenLine, "renamenx"}}.
dbsize|DBSIZE				: {token, {dbsize, TokenLine, "dbsize"}}.
expire|EXPIRE				: {token, {expire, TokenLine, "expire"}}.
ttl|TTL					: {token, {ttl, TokenLine, "ttl"}}.

%% List commands
rpush|RPUSH				: {token, {rpush, TokenLine, "rpush"}}.
lpush|LPUSH				: {token, {lpush, TokenLine, "lpush"}}.
llen|LLEN				: {token, {llen, TokenLine, "llen"}}.
lrange|LRANGE				: {token, {lrange, TokenLine, "lrange"}}.
ltrim|LTRIM				: {token, {ltrim, TokenLine, "ltrim"}}.
lindex|LINDEX				: {token, {lindex, TokenLine, "lindex"}}.
lset|LSET				: {token, {lset, TokenLine, "lset"}}.
lrem|LREM				: {token, {lrem, TokenLine, "lrem"}}.
lpop|LPOP				: {token, {lpop, TokenLine, "lpop"}}.
rpop|RPOP				: {token, {rpop, TokenLine, "rpop"}}.

%% Set commands
sadd|SADD				: {token, {sadd, TokenLine, "sadd"}}.
srem|SREM				: {token, {srem, TokenLine, "srem"}}.
spop|SPOP				: {token, {spop, TokenLine, "spop"}}.
smove|SMOVE				: {token, {smove, TokenLine, "smove"}}.
scard|SCARD				: {token, {scard, TokenLine, "scard"}}.
sismember|SISMEMBER			: {token, {sismember, TokenLine, "sismember"}}.
sinter|SINTER				: {token, {sinter, TokenLine, "sinter"}}.
sinterstore|SINTERSTORE			: {token, {sinterstore, TokenLine, "sinterstore"}}.
sunion|SUNION				: {token, {sunion, TokenLine, "sunion"}}.
sunionstore|SUNIONSTORE			: {token, {sunionstore, TokenLine, "sunionstore"}}.
sdiff|SDIFF				: {token, {sdiff, TokenLine, "sdiff"}}.
sdiffstore|SDIFFSTORE			: {token, {sdiffstore, TokenLine, "sdiffstore"}}.
smembers|SMEMBERS			: {token, {smembers, TokenLine, "smembers"}}.

%% DB commands
select|SELECT				: {token, {select, TokenLine, "select"}}.
move|MOVE				: {token, {move, TokenLine, "move"}}.
flushdb|FLUSHDB				: {token, {flushdb, TokenLine, "flushdb"}}.
flushall|FLUSHALL			: {token, {flushall, TokenLine, "flushall"}}.

%% Sort commands
sort|SORT				: {token, {sort, TokenLine, "sort"}}.

%% Peristence commands
save|SAVE				: {token, {save, TokenLine, "save"}}.
bgsave|BGSAVE				: {token, {bgsave, TokenLine, "bgsave"}}.
lastsave|LASTSAVE			: {token, {lastsave, TokenLine, "lastsave"}}.
shutdown|SHUTDOWN			: {token, {shutdown, TokenLine, "shutdown"}}.

%% Remote server commands
info|INFO				: {token, {info, TokenLine, "info"}}.
monitor|MONITOR				: {token, {monitor, TokenLine, "monitor"}}.
slaveof|SLAVEOF				: {token, {slaveof, TokenLine, "slaveof"}}.

%% Connection commands
quit|QUIT				: {token, {quit, TokenLine, "quit"}}.
auth|AUTH				: {token, {auth, TokenLine, "auth"}}.

\040(\\\^.|\\.|[^(\040|(\015\012))])*	: T = lists:sublist(TokenChars, 2, length(TokenChars)),
                                          {token, {datum, TokenLine, T}}.
\015|\012				: skip_token.

Erlang code.