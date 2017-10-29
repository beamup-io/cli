-module(beamup_util_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, beamup_util).

replace_tuple_test() ->
  Base = [{a, true}, {b, 2}],
  Expected = [{a, false}, {b, 2}],
  ?assertEqual(Expected, ?M:override(a, 1, Base, {a, false})).

append_list_test() ->
  Base = [{opts, [a, b]}],
  Expected = [{opts, [a, b, c]}],
  ?assertEqual(Expected, ?M:override(opts, 1, Base, {opts, [c]})).

keep_existing_list_test() ->
  Base = [{opts, [a]}],
  Expected = [{opts, [a]}],
  ?assertEqual(Expected, ?M:override(opts, 1, Base, {opts, [a]})).
