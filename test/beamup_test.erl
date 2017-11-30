-module(beamup_test).

-include_lib("eunit/include/eunit.hrl").

replace_tuple_test() ->
  Base = [{a, true}, {b, 2}],
  Expected = [{a, false}, {b, 2}],
  ?assertEqual(Expected, beamup_util:override(a, 1, Base, {a, false})).

append_list_test() ->
  Base = [{opts, [a, b]}],
  Expected = [{opts, [a, b, c]}],
  ?assertEqual(Expected, beamup_util:override(opts, 1, Base, {opts, [c]})).

keep_existing_list_test() ->
  Base = [{opts, [a]}],
  Expected = [{opts, [a]}],
  ?assertEqual(Expected, beamup_util:override(opts, 1, Base, {opts, [a]})).
