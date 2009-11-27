-module(membox_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Config) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [Config]).

init([Config]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Restart = permanent,
	Shutdown = 2000,
	Type = worker,

  Storage = build_storage_specs(Config),
	Listener = {listener, {membox_listener, start_link, []},
							Restart, Shutdown, Type, [membox_listener]},

	{ok, {SupFlags, Storage ++ [Listener]}}.

%% Internal functions
build_storage_specs(Config) ->
  build_storage_specs(Config, []).

build_storage_specs([], Accum) ->
  lists:reverse(Accum);

build_storage_specs([{Id, Config}|T], Accum) ->
  Child = {Id, {membox_db, start_link, [Id, Config]},
           permanent, 30000, worker, [membox_storage]},
  build_storage_specs(T, [Child|Accum]).
