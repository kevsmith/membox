-module(membox_suite).

-include_lib("eunit/include/eunit.hrl").

suite_test_() ->
  [{module, membox_parser_tests}].
