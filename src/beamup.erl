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

  beamup_build:full(Project),

  Download = fun(V) ->
    beamup_store:get(Store, Project, V)
  end,
  NeededVersions = lists:filter(fun(V) ->
    V /= maps:get(version, Project)
  end, StoredVersions),
  PreviousTars = lists:map(Download, NeededVersions),
  io:format("Fetched previous releases: ~p~n", [PreviousTars]),
  ExtractedReleases = lists:map(
    fun beamup_build_tool:extract/1,
  PreviousTars),
  io:format("Extracted previous releases: ~p~n~n", [ExtractedReleases]),

  ToMetadata = fun(PreviousPath) ->
    {ok, [[{release, Name, Version, _, _, _}]]} = file:consult(filename:join([PreviousPath, "releases", "RELEASES"])),
    #{path => PreviousPath,
    name => list_to_binary(Name),
    version => list_to_binary(Version)}
  end,
  PreviousReleases = lists:map(ToMetadata, ExtractedReleases),
  io:format("Previous releases: ~p~n~n", [PreviousReleases]),

  beamup_build:upgrade(Project, PreviousReleases),

  TarPath = beamup_build_tool:tar(Project),

  io:format("Uploading full release to store~n"),
  beamup_store:put(Store, Project, TarPath, full).
