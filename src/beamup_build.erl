-module(beamup_build).

-export([run/1]).

run(Path) ->
  Project = beamup_project:new(Path),
  beamup_sanity_check:check(Project),
  io:format("Last commit hash: ~p~n", [maps:get(commit, Project)]),
  io:format("Fetching dependencies~n"),
  beamup_build_tool:deps(Project),
  io:format("Building full release~n"),
  beamup_build_tool:full_release(Project).
