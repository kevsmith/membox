Nonterminals
data datum_list command set_expr get_expr getset_expr mget_expr setnx_expr incr_expr
incrby_expr decr_expr decrby_expr exists_expr del_expr type_expr.

Terminals
datum set get getset mget setnx incr incrby decr decrby exists del type.

Rootsymbol command.

%% Supporting productions
data -> datum_list: [D || {D} <- '$1'].
datum_list -> datum datum_list: lists:flatten([{ev('$1')}, '$2']).
datum_list -> datum: [{ev('$1')}].


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

%% List commands
set_expr -> set datum datum: [{set, ev('$2'), ev_ds('$3')}, status_ok].
get_expr -> get datum: [{get, ev('$2')}, bulk].
getset_expr -> getset datum datum: [{getset, ev('$2'), ev_ds('$3')}, bulk].
mget_expr -> mget data: [{mget, '$2'}, multi_bulk].
setnx_expr -> setnx datum datum: [{setnx, ev('$2'), ev_ds('$3')}, integer].
incr_expr -> incr datum: [{incr, ev('$2')}, integer].
incrby_expr -> incrby datum datum: [{incrby, ev('$2'), ev_int('$3')}, integer].
decr_expr -> decr datum: [{decr, ev('$2')}, integer].
decrby_expr -> decrby datum datum: [{decrby, ev('$2'), ev_int('$3')}, integer].
exists_expr -> exists datum: [{exists, ev('$2')}, integer].
del_expr -> del datum: [{del, ev('$2')}, integer].
type_expr -> type datum: [{type, ev('$2')}, status_type].


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
