-module(beamup).

-export([run/3]).

run(Path, Url, Secret) ->
  Project = beamup_project:new(Path),
  Store = beamup_store:new(Url, Secret),

  beamup_sanity_check:check(Project),

  StoredVersions = beamup_store:versions(Store, Project),


  io:format("Project: ~p~n", [Project]),
  io:format("Current version: ~p~n", [maps:get(version, Project)]),
  io:format("Current branch: ~p~n", [maps:get(branch, Project)]),
  io:format("Local branches: ~p~n", [beamup_git:local_branches(Path)]),
  io:format("Commits by branch: ~p~n", [beamup_git:commit_hashes_by_branches(Path)]),
  io:format("Versions in store: ~p~n", [StoredVersions]),

  io:format("Fetching dependencies~n"),
  beamup_build_tool:deps(Project),

  io:format("Building full release~n"),
  {full, TarPath} = beamup_build_tool:release(Project, full),

  Download = fun(V) ->
    beamup_store:get(Store, Project, V)
  end,
  PreviousReleases = lists:map(Download, StoredVersions),

  io:format("Fetched previous releases: ~p", [PreviousReleases]),
  Log = fun(Bytes) -> io:put_chars(Bytes) end,
  beamup_shell:cmd(<<"ls /beamup/">>, [], Log),
  beamup_shell:cmd(<<"ls /beamup/project">>, [], Log),
  beamup_shell:cmd(<<"ls /beamup/project/myrel">>, [], Log),
  beamup_shell:cmd(<<"ls /beamup/project/myrel/_build">>, [], Log),
  beamup_shell:cmd(<<"ls /beamup/project/myrel/_build/default">>, [], Log),
  beamup_shell:cmd(<<"ls /beamup/project/myrel/_build/default/rel/myrel">>, [], Log),
  lists:map(fun beamup_build_tool:extract/1, PreviousReleases).

  % io:format("Uploading full release to store~n"),
  % beamup_store:put(Store, Project, TarPath, full).
