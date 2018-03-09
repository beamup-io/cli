-module(beamup_node).

-export([run/2]).

run(WantedBranch, _Store) ->
  Branch = default_branch(WantedBranch),
  io:format("Branch ~p~n", [Branch]).

% Private

default_branch(<<>>) ->
  io:format("Warning: Falling back to default branch `master`~n"),
  io:format("  To explicitly set the branch that this node should run~n"),
  io:format("  please pass the environment variable BEAMUP_NODE_BRANCH~n"),
  io:format("    export BEAMUP_NODE_BRANCH=develop~n"),
  <<"master">>;
default_branch(Branch) -> Branch.
