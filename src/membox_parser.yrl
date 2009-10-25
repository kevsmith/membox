Nonterminals
data datum_list command set_expr get_expr getset_expr mget_expr setnx_expr
incr_expr incrby_expr decr_expr decrby_expr exists_expr del_expr type_expr
keys_expr randomkey_expr rename_expr renamenx_expr dbsize_expr expire_expr
ttl_expr rpush_expr lpush_expr llen_expr lrange_expr ltrim_expr lindex_expr
lset_expr lrem_expr lpop_expr rpop_expr.

Terminals
datum set get getset mget setnx incr incrby decr decrby exists del type
keys randomkey rename renamenx dbsize expire ttl rpush lpush llen lrange
ltrim lindex lset lrem lpop rpop.

Rootsymbol command.

%% Supporting productions
data -> datum_list: [D || {D} <- '$1'].
datum_list -> datum datum_list: lists:flatten([{ev('$1')}, '$2']).
datum_list -> datum: [{ev('$1')}].

%% Return 2 element tuple:
%% {ParsedCommand, ReplyType}

command -> set_expr: '$1'.
command -> get_expr: '$1'.
command -> getset_expr: '$1'.
command -> mget_expr: '$1'.
command -> setnx_expr: '$1'.
command -> incr_expr: '$1'.
command -> decr_expr: '$1'.
command -> incrby_expr: '$1'.
command -> decrby_expr: '$1'.
command -> exists_expr: '$1'.
command -> del_expr: '$1'.
command -> type_expr: '$1'.

command -> keys_expr: '$1'.
command -> randomkey_expr: '$1'.
command -> rename_expr: '$1'.
command -> renamenx_expr: '$1'.
command -> dbsize_expr: '$1'.
command -> expire_expr: '$1'.
command -> ttl_expr: '$1'.

command -> rpush_expr: '$1'.
command -> lpush_expr: '$1'.
command -> llen_expr: '$1'.
command -> lrange_expr: '$1'.
command -> ltrim_expr: '$1'.
command -> lindex_expr: '$1'.
command -> lset_expr: '$1'.
command -> lrem_expr: '$1'.
command -> lpop_expr: '$1'.
command -> rpop_expr: '$1'.

%% String commands
set_expr -> set datum datum: {{set, ev('$2'), ev_ds('$3')}, status_ok}.
get_expr -> get datum: {{get, ev('$2')}, bulk}.
getset_expr -> getset datum datum: {{getset, ev('$2'), ev_ds('$3')}, bulk}.
mget_expr -> mget data: {{mget, '$2'}, multi_bulk}.
setnx_expr -> setnx datum datum: {{setnx, ev('$2'), ev_ds('$3')}, integer}.
incr_expr -> incr datum: {{incr, ev('$2')}, integer}.
incrby_expr -> incrby datum datum: {{incrby, ev('$2'), ev_int('$3')}, integer}.
decr_expr -> decr datum: {{decr, ev('$2')}, integer}.
decrby_expr -> decrby datum datum: {{decrby, ev('$2'), ev_int('$3')}, integer}.
exists_expr -> exists datum: {{exists, ev('$2')}, integer}.
del_expr -> del datum: {{del, ev('$2')}, integer}.
type_expr -> type datum: {{type, ev('$2')}, status_type}.

%% Keyspace commands
keys_expr -> keys datum: {{keys, ev('$2')}, bulk}.
randomkey_expr -> randomkey: {randomkey, single_line}.
renamenx_expr -> renamenx datum datum: {{renamenx, ev('$2'), ev('$3')}, integer}.
rename_expr -> rename datum datum: {{rename, ev('$2'), ev('$3')}, status_ok}.
dbsize_expr -> dbsize: {dbsize, integer}.
expire_expr -> expire datum: {{expire, ev('$2')}, integer}.
ttl_expr -> ttl datum: {{ttl, ev('$2')}, integer}.

%% List commands
rpush_expr -> rpush datum datum: {{rpush, ev('$2'), ev_ds('$3')}, status_ok}.
lpush_expr -> lpush datum datum: {{lpush, ev('$2'), ev_ds('$3')}, status_ok}.
llen_expr -> llen datum: {{llen, ev('$2')}, integer}.
lrange_expr -> lrange datum datum datum: {{lrange, ev('$2'), ev_int('$3'), ev_int('$4')}, multi_bulk}.
ltrim_expr -> ltrim datum datum datum: {{ltrim, ev('$2'), ev_int('$3'), ev_int('$4')}, status_ok}.
lindex_expr -> lindex datum datum: {{lindex, ev('$2'), ev_int('$3')}, bulk}.
lset_expr -> lset datum datum datum: {{lset, ev('$2'), ev_int('$3'), ev_ds('$4')}, status_ok}.
lrem_expr -> lrem datum datum datum: {{lrem, ev('$2'), ev_int('$3'), ev_ds('$4')}, integer}.
lpop_expr -> lpop datum: {{lpop, ev('$2')}, bulk}.
rpop_expr -> rpop datum: {{rpop, ev('$2')}, bulk}.

Erlang code.
-export([parse_string/1]).

parse_string(Text) when is_binary(Text) ->
  parse_string(binary_to_list(Text));
parse_string(Text) when is_list(Text) ->
  {ok, Tokens, _} = membox_lexer:string(Text),
  membox_parser:parse(Tokens).

ev({datum, _TokenLine, T}) ->
  T.

ev_int({datum, _TokenLine, T}) ->
  list_to_integer(T).

ev_ds(Token) ->
  ev_int(Token) + 2.
