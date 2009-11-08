-module(membox_response).

-export([send/3]).

send(none, status_type, Sock) ->
  gen_tcp:send(Sock, "none\r\n");
send(string, status_type, Sock) ->
  gen_tcp:send(Sock, "string\r\n");
send(list, status_type, Sock) ->
  gen_tcp:send(Sock, "list\r\n");
send(set, status_type, Sock) ->
  gen_tcp:send(Sock, "set\r\n");
send(N, integer, Sock) when is_number(N) ->
  gen_tcp:send(Sock, integer_to_list(N) ++ "\r\n");
send(true, integer, Sock) ->
  gen_tcp:send(Sock, "1\r\n");
send(false, integer, Sock) ->
  gen_tcp:send(Sock, "0\r\n");
send(Data, multi_bulk, Sock) ->
  Reply = lists:foldl(fun(Item, R) -> R ++ format_bulk(Item) end,
                      io_lib:format("*~p\r\n", [length(Data)]),
                      Data),
  gen_tcp:send(Sock, Reply);
send(not_found, bulk, Sock) ->
  gen_tcp:send(Sock, format_bulk(not_found));
send([], bulk, Sock) ->
  gen_tcp:send(Sock, "$0\r\n");
send(Response, bulk, Sock) when is_list(Response) ->
  send(list_to_binary(Response), bulk, Sock);
send(Response, bulk, Sock) when is_binary(Response) ->
  D = format_bulk(Response),
  gen_tcp:send(Sock, D);
send(ok, status_ok, Sock) ->
  gen_tcp:send(Sock, "+OK\r\n");
send(error, status_ok, Sock) ->
  gen_tcp:send(Sock, "-Err\r\n");
send(not_found, bulk_key_string, Sock) ->
  gen_tcp:send(Sock, format_bulk(not_found));
send(Data, bulk_key_string, Sock) ->
  KeyString = list_to_binary(generate_key_string(Data, [])),
  Resp = io_lib:format("$~p\r\n~s\r\n", [size(KeyString), KeyString]),
  gen_tcp:send(Sock, Resp).

format_bulk(nil) ->
  "nil\r\n";
format_bulk(not_found) ->
  "$-1\r\n";
format_bulk(Item) ->
  io_lib:format("$~p\r\n~s\r\n", [size(Item), Item]).

generate_key_string([H|T], Accum) when length(T) == 0 ->
  lists:reverse([H|Accum]);
generate_key_string([H|T], Accum) ->
  generate_key_string(T, [[H, <<" ">>]|Accum]).
