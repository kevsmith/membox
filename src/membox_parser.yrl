Nonterminals
command datum_list set_expr get_expr
mget_expr setnx_expr incr_expr decr_expr
incrby_expr decrby_expr exists_expr
del_expr type_expr.

Terminals
set get setnx mget incr decr
incrby decrby exists del type datum
number.

Rootsymbol command.

command -> set_expr: '$1'.
command -> get_expr: '$1'.
command -> mget_expr: '$1'.
command -> setnx_expr: '$1'.
command -> incr_expr: '$1'.
command -> decr_expr: '$1'.
command -> incrby_expr: '$1'.
command -> decrby_expr: '$1'.
command -> exists_expr: '$1'.
command -> del_expr: '$1'.
command -> type_expr: '$1'.

datum_list -> datum datum_list: [ev('$1'), '$2'].
datum_list -> datum: ev('$1').

set_expr -> set datum number: {set, dispatch(set, ev('$2'), ev('$3'))}.
set_expr -> set number number: {set, dispatch(set, ev_as_datum('$2'), ev('$3'))}.
get_expr -> get datum: {get, dispatch(get, ev('$2'))}.
get_expr -> get number: {get, dispatch(get, ev_as_datum('$2'))}.
mget_expr-> mget datum_list: {mget, lists:flatten('$2')}.
setnx_expr -> setnx datum number: {setnx, ev('$2'), ev('$3')}.
incr_expr -> incr datum: {incr, ev('$2')}.
decr_expr -> decr datum: {decr, ev('$2')}.
incrby_expr -> incrby datum number: {incrby, ev('$2'), ev('$3')}.
decrby_expr -> decrby datum number: {decrby, ev('$2'), ev('$3')}.
exists_expr -> exists datum: {exists, ev('$2')}.
del_expr -> del datum: {delete, ev('$2')}.
type_expr -> type datum: {type, ev('$2')}.

Erlang code.
-compile(inline).

-export([execute/1]).

execute(Text) ->
  case membox_lexer:string(Text) of
    {ok, Tokens, _} ->
      parse(Tokens)
  end.

dispatch(Command, Key, Datum) ->
  membox_storage:send_command(Command, Key, read(Datum)).

dispatch(Command, Key) ->
  membox_storage:send_command(Command, Key).

%% Internal functions
read(Size) ->
  case get(data_port) of
    undefined ->
      throw({error, no_data_port});
    Port ->
      case gen_tcp:recv(Port, Size + 2, 10000) of
        {ok, Packet} ->
          lists:sublist(Packet, 1, length(Packet) - 2);
        {error, Reason} ->
          error_logger:error_msg("Error processing command: ~p~n", [Reason]),
          throw({error, Reason})
      end
  end.

ev_as_datum({_, _, Chars}) ->
  Chars.

ev({datum, _, Chars}) ->
  Chars;
ev({number, _, Chars}) ->
  list_to_integer(Chars).
