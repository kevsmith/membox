-module(membox).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() ->
  application:start(membox).

start(_StartType, _StartArgs) ->
  case read_config() of
    {ok, [{pid_file, Path}|Config]} ->
      write_pid_file(Path),
      {ok, Pid} = membox_sup:start_link(Config),
      {ok, Pid, Path};
    Error ->
      Error
  end.

stop(Path) ->
  file:delete(Path),
  ok.

%% Internal functions
write_pid_file(Path) ->
  file:write_file(Path, os:getpid()).
read_config() ->
  case init:get_argument(membox_config) of
    error ->
      case application:get_env(membox_config) of
        undefined ->
          {error, missing_config};
        FileName ->
          file:consult(FileName)
      end;
    {ok, [[FileName]]} ->
      file:consult(FileName)
  end.
