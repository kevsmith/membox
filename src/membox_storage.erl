-module(membox_storage).
-compile(inline).

%% API
-export([start_link/0, stop/0, send_command/2, send_command/3]).

send_command(Command, Key, Value) ->
	?MODULE ! {self(), {Command, Key, Value}},
	receive
		Reply ->
			Reply
	end.

send_command(Command, Key) ->
	?MODULE ! {self(), {Command, Key}},
	receive
		Reply ->
			Reply
	end.

stop() ->
	?MODULE ! stop,
	unregister(?MODULE).

start_link() ->
	Pid = proc_lib:spawn_link(fun() ->
																Tid = ets:new(table_1, [bag]),
																proc_lib:init_ack({ok, self()}),
																command_loop(Tid) end),
	receive
		{ack, _, {ok, Pid}} ->
			register(?MODULE, Pid),
			{ok, Pid};
		Error ->
			Error
	end.

command_loop(Tid) ->
	receive
		{Caller, {get, Key}} ->
			Caller ! ets:lookup_element(Tid, Key, 3),
			command_loop(Tid);
		{Caller, {set, Key, Value}} ->
			ets:insert(Tid, {Key, single, Value}),
			Caller ! ok,
			command_loop(Tid);
		stop ->
			ets:delete(Tid)
	end.
