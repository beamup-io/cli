-module(beamup_version_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, beamup_version).

-define(A, "aaa").
-define(B, "bbb").
-define(C, "ccc").
-define(X, "xxx").
-define(Y, "yyy").
-define(Z, "zzz").

both_empty_test() ->
  ?assertEqual(false, ?M:previous([], [])).

local_empty_test() ->
  ?assertEqual(false, ?M:previous([], [?A])).

stored_empty_test() ->
  ?assertEqual(false, ?M:previous([?A], [])).

only_same_false_test() ->
  ?assertEqual(false, ?M:previous([?A], [?A])).

only_unknown_test() ->
  ?assertEqual(false, ?M:previous([?A, ?B], [?X])).

last_test() ->
  ?assertEqual(?B, ?M:previous([?A, ?B], [?A, ?B])).

last2_test() ->
  ?assertEqual(?B, ?M:previous([?A, ?B], [?B])).

last3_test() ->
  ?assertEqual(?B, ?M:previous([?A, ?B, ?C], [?B, ?C])).

skip_unknown_test() ->
  ?assertEqual(?B, ?M:previous([?A, ?B], [?X, ?B])).
