-module(beamup_build).

-export([full/1, upgrade/2]).

full(Project) ->
  beamup_build_tool:deps(Project),
  beamup_build_tool:compile(Project).

upgrade(Project, PreviousReleases) ->
  ok = generate_appups(Project, PreviousReleases),
  ok = generate_relup(Project, PreviousReleases).

% Private

generate_appups(Project, PreviousReleases) ->
  map_releases(Project, PreviousReleases, fun beamup_build_tool:appups/2).

generate_relup(Project, PreviousReleases) ->
  map_releases(Project, PreviousReleases, fun beamup_build_tool:relup/2).

map_releases(Project, PreviousReleases, Fun) ->
  Raw = collect_instructions(Project, PreviousReleases, Fun, []),
  ByFile = by_keys(Raw),
  Merged = map_values(fun merge_upgrade_instructions/1, ByFile),
  io:format("~n~p~n", [Merged]),
  write_terms(beamup_project:path(Project), Merged).

collect_instructions(Project, [PreviousRelease|PreviousReleases], Fun, Acc) ->
  % Operate on a copy of the current project's working tree
  % because otherwise the relup provider gets confused
  TempCurrentProject = beamup_project:duplicate(Project),
  Path = Fun(TempCurrentProject, PreviousRelease),
  Terms = ingest_terms(beamup_project:path(TempCurrentProject), Path),
  Acc2 = Terms ++ Acc,
  beamup_project:remove(TempCurrentProject),
  collect_instructions(Project, PreviousReleases, Fun, Acc2);
collect_instructions(_Project, [], _Fun, Acc) -> Acc.

% Restructure a proplist with multiple identical keys so that
% all values of a key are grouped together under that key
by_keys(List) ->
  Keys = proplists:get_keys(List),
  by_key(Keys, List, []).
by_key([Key|T], List, Acc) ->
  Acc2 = [{Key, proplists:get_all_values(Key, List)}] ++ Acc,
  by_key(T, List, Acc2);
by_key([], _List, Acc) -> Acc.

% Map proplist values
map_values(Fun, [{Key, Value}|T]) ->
  [{Key, Fun(Value)}|map_values(Fun, T)];
map_values(_Fun, []) -> [].

% Merge two appup or relup instruction lists
merge_upgrade_instructions(List) ->
  lists:foldl(fun merge_upgrade_instructions/2, [], List).
merge_upgrade_instructions({Vsn, UpA, DownA}, {Vsn, UpB, DownB}) ->
  {Vsn, UpA ++ UpB, DownA ++ DownB};
merge_upgrade_instructions({Vsn, UpA, DownA}, _) -> {Vsn, UpA, DownA};
merge_upgrade_instructions(_, {Vsn, UpB, DownB}) -> {Vsn, UpB, DownB}.

% Read multiple terms from files at once
ingest_terms(RootPath, AbsolutePath) when is_binary(AbsolutePath) ->
  ingest_terms(RootPath, [AbsolutePath]);
ingest_terms(RootPath, AbsolutePaths) when is_list(AbsolutePaths) ->
  ingest_terms(RootPath, AbsolutePaths, []).
ingest_terms(RootPath, [AbsolutePath|T], Acc) ->
  {ok, [Term]} = file:consult(AbsolutePath),
  MaybeRelativePath = string:prefix(AbsolutePath, RootPath),
  RelativePath = string:prefix(MaybeRelativePath, <<$/>>),
  Acc2 = [{RelativePath, Term}] ++ Acc,
  io:format("Reading file ~p~n~p~n", [AbsolutePath, RelativePath]),
  ingest_terms(RootPath, T, Acc2);
ingest_terms(_, [], Acc) -> Acc.

% Write multiple terms to files at once
write_terms(RootPath, [{RelativePath, Term}|T]) ->
  Path = filename:join([RootPath, RelativePath]),
  Formatted = io_lib:format("~p.~n", [Term]),
  ok = file:write_file(Path, Formatted),
  io:format("Writing file ~p~n", [Path]),
  io:format(Formatted),
  write_terms(RootPath, T);
write_terms(_, []) -> ok.
