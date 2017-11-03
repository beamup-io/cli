-module(beamup_build).

-export([run/1]).

run(Path) ->
  Project = beamup_project:new(Path),
  beamup_sanity_check:check(Project),
  io:format("Current commit hash: ~p~n", [maps:get(commit, Project)]),
  io:format("Current branch: ~p~n", [maps:get(branch, Project)]),
  io:format("Local Branches: ~p~n", [beamup_git:local_branches(Path)]),
  io:format("Commits by Branch: ~p~n", [beamup_git:commit_hashes_by_branches(Path)]),
  io:format("Upgrade Release From Vsn: ~p~n", [beamup_version:previous(Project)]),
  io:format("Fetching dependencies~n"),
  beamup_build_tool:deps(Project),

  io:format("Building full release~n"),
  {full, TarFull} = beamup_build_tool:release(Project, full),
  beamup_store:put(Project, TarFull, full),

  UpFromVsn = beamup_version:previous(Project),
  case UpFromVsn of
    false ->
      io:format("Not building an upgrade release.~n"),
      halt(1);
    _ ->
      io:format("Building upgrade release from ~p to ~p~n", [UpFromVsn, maps:get(commit, Project)]),
      {upgrade, UpFromVsn, TarUpgrade} = beamup_build_tool:release(Project, {upgrade, UpFromVsn}),
      beamup_store:put(Project, TarUpgrade, {upgrade, UpFromVsn})
  end.
