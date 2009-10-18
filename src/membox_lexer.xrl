Definitions.

Rules.

%% Commands
set|SET					: {token, {set, TokenLine, "set"}}.
get|GET					: {token, {get, TokenLine, "get"}}.
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

\040(\\\^.|\\.|[^(\040|(\015\012))])*	: T = lists:sublist(TokenChars, 2, length(TokenChars)),
                                          case is_numeric(T) of
                                            true ->
					      {token, {number, TokenLine, T}};
                                 	    false ->
					      {token, {datum, TokenLine, T}}
					  end.
\015|\012				: skip_token.

Erlang code.
-compile(inline).
is_numeric(N) ->
  try
    begin
      _ = list_to_integer(N),
      true
    end
  catch
    error:badarg ->
      false
  end.