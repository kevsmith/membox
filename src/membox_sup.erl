-module(membox_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Restart = permanent,
	Shutdown = 2000,
	Type = worker,

	Storage = {storage, {membox_db, start_link, [db1]},
						 Restart, Shutdown, Type, [membox_storage]},
	Listener = {listener, {membox_listener, start_link, []},
							Restart, Shutdown, Type, [membox_listener]},


	{ok, {SupFlags, [Storage, Listener]}}.
