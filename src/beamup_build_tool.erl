-module(beamup_build_tool).

-export([supported_tools/0,
         supported_tools_names/0,
         detect/1,
         deps/1,
         release/2]).

supported_tools() -> [beamup_build_tool_rebar3].
supported_tools_names() -> lists:map(fun ({_, Name}) -> Name end, with_tools(name)).

with_tools(F) -> with_tools(F, []).
with_tools(F, A) ->
  lists:map(fun (M) -> {M, erlang:apply(M, F, A)} end, supported_tools()).

detect(Path) ->
  {ok, Filenames} = file:list_dir(Path),
  case lists:keyfind(true, 2, with_tools(detect, [Filenames])) of
    false -> false;
    { Module, true } -> Module
end.

deps(#{ tool := Tool, path := Path }) ->
  apply(Tool, deps, [Path]).

release(#{ tool := Tool, path := Path } = Project, full) ->
  [app_config(Project, AppName) || AppName <- Tool:app_names(Path)],
  full_release_config(Project),
  Tar = Tool:release(Project, full),
  {full, Tar};
release(#{ name := Name, tool := Tool, path := Path } = Project, {upgrade, UpFromVsn}) ->
  PreviousTar = beamup_store:get(Project, {full, UpFromVsn}),
  % PreviousPath = filename:join(Path, ["_build/default/rel/", Name]),
  PreviousPath = filename:join(Path, ["_build/", Name]),
  file:make_dir(PreviousPath),
  {0, _} = beamup_shell:cmd("tar xvfz " ++ PreviousTar, [{cd, PreviousPath}]),
  [app_config(Project, AppName) || AppName <- Tool:app_names(Path)],
  full_release_config(Project),
  Tar = Tool:release(Project, {upgrade, UpFromVsn, PreviousPath}),
  io:format("Built upgrade tarball: ~p~n", [Tar]),
  {upgrade, UpFromVsn, Tar}.

full_release_config(#{ tool := Tool, path := Path, commit := Vsn }) ->
  Filename = filename:join(Path, Tool:release_config_filename()),
  {ok, Config} = file:consult(Filename),
  Config2 = Tool:release_config(Vsn, Config),
  Config3 = lists:flatten([io_lib:fwrite("~p.~n", [Term]) || Term <- Config2]),
  io:format("Release Config: ~p~n", [Config2]),
  file:write_file(Filename, Config3).

app_config(#{ tool := Tool, path := Path }, AppName) ->
  AppPath = filename:join(Path, Tool:app_path(AppName)),
  Filename = filename:join(AppPath, Tool:app_config_filename(AppName)),
  {ok, Config} = file:consult(Filename),
  AppVsn = beamup_git:commit_hash(AppPath),
  Config2 = Tool:app_config(AppVsn, Config),
  file:write_file(Filename, io_lib:fwrite("~p.~n", [Config2])).
