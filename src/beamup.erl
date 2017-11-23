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
  io:format("~n~nDone fetching dependencies~n~n"),

  io:format("Compiling project~n"),
  beamup_build_tool:compile(Project),
  io:format("~n~nDone compiling project~n~n"),

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

  ToMetadata = fun(Path) ->
    {ok, [[{release, Name, Version, _, _, _}]]} = file:consult(filename:join([Path, "releases", "RELEASES"])),
    #{path => Path,
    name => list_to_binary(Name),
    version => list_to_binary(Version)}
  end,
  PreviousReleases = lists:map(ToMetadata, ExtractedReleases),
  io:format("Previous releases: ~p~n~n", [PreviousReleases]),

  % TODO: Generate appups and relups that know
  % how to upgrade from multiple releases.
  % For now, just build upgrades from a single release.
  case PreviousReleases of
    [SinglePreviousRelease|_] ->
      lists:map(fun (PreviousRelease) ->
        io:format("~nGenerating appup for ~p~n~n", [PreviousRelease]),
        beamup_build_tool:appup(Project, PreviousRelease),
        io:format("~n~nDone generating appup for ~p~n~n", [PreviousRelease])
      end, [SinglePreviousRelease]),

      io:format("~n~nGenerating relup~n~n"),
      beamup_build_tool:relup(Project, SinglePreviousRelease),
      io:format("~n~nDone generating relup~n~n");
    _ -> ok
  end,

  TarPath = beamup_build_tool:tar(Project),

  io:format("Uploading full release to store~n"),
  beamup_store:put(Store, Project, TarPath, full).
