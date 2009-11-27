-module(membox_util).

-export([datetime_to_unix_ts/1]).


datetime_to_unix_ts(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) -
                calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
