-module(beamup_build).

-export([full/1, upgrade/2]).

full(Project) ->
  beamup_build_tool:deps(Project),
  beamup_build_tool:compile(Project).

upgrade(Project, PreviousReleases) ->
  generate_appups(Project, PreviousReleases),
  generate_relup(Project, PreviousReleases).

% Private

generate_appups(Project, PreviousReleases) ->
  lists:map(fun(PreviousRelease) ->
    io:format("~nGenerating appup for ~p~n~n", [PreviousRelease]),
    beamup_build_tool:appup(Project, PreviousRelease),
    % Merge appup
    io:format("~n~nDone generating appup for ~p~n~n", [PreviousRelease])
  end, [hd(PreviousReleases)]).

generate_relup(Project, PreviousReleases) ->
  lists:map(fun(PreviousRelease) ->
    io:format("~n~nGenerating relup~n~n"),
    beamup_build_tool:relup(Project, PreviousRelease),
    % Merge relup
    io:format("~n~nDone generating relup~n~n")
  end, [hd(PreviousReleases)]).
