-module(membox).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() ->
  application:start(sasl),
  application:start(crypto),
  application:start(membox).

start(_StartType, _StartArgs) ->
  case read_config() of
    {ok, Config} ->
      membox_sup:start_link(Config);
    Error ->
      Error
  end.

stop(_State) ->
  ok.

%% Internal functions
read_config() ->
  case init:get_argument(membox_config) of
    error ->
      case application:get_env(membox_config) of
        undefined ->
          {error, missing_config};
        FileName ->
          file:consult(FileName)
      end;
    [[FileName]] ->
      file:consult(FileName)
  end.
