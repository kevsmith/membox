-module(membox_bench).

-compile([export_all]).

timeit() ->
	membox_storage:start_link(),
	Commands = ["set foo_" ++ integer_to_list(N) ++ " 3"|| N <- lists:seq(1, 100000)],
	{Time, _} = timer:tc(?MODULE, run, [Commands]),
	print_stats(Time),
	membox_storage:stop().

run(Commands) ->
	lists:foreach(fun(C) -> [membox_parser:parse_command(C)] end, Commands).

print_stats(Time) ->
	io:format("Took ~pus, ~pms, ~ps~n", [Time, Time / 1000, Time / 1000000]).
