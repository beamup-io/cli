-module(beamup).

-export([run/2]).

run(Path, StoreCredentials) ->
  Project = beamup_project:new(Path),
  Store = beamup_store:new(StoreCredentials),

  io:format("Project: ~p~n", [Project]),
  io:format("Current version: ~p~n", [maps:get(version, Project)]),
  io:format("Current branch: ~p~n", [maps:get(branch, Project)]),
  io:format("Local Branches: ~p~n", [beamup_git:local_branches(Path)]),
  io:format("Commits by Branch: ~p~n", [beamup_git:commit_hashes_by_branches(Path)]),
  io:format("Upgrade Release From Vsn: ~p~n", [beamup_version:previous(Project)]),

  io:format("Fetching dependencies~n"),
  beamup_build_tool:deps(Project),

  io:format("Building full release~n"),
  {full, TarPath} = beamup_build_tool:release(Project, full),

  io:format("Uploading full release to store~n"),
  beamup_store:put(Store, TarPath, full).
