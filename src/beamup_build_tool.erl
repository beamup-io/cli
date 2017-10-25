-module(beamup_build_tool).

-export([supported_tools/0,
         supported_tools_names/0,
         detect/1,
         deps/1,
         full_release/1]).

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

full_release(#{ tool := Tool, path := Path } = Project) ->
  [app_config(Project, AppName) || AppName <- Tool:app_names(Path)],
  full_release_config(Project),
  Tool:full_release(Project).

full_release_config(#{ tool := Tool, path := Path, commit := Vsn }) ->
  Filename = filename:join(Path, Tool:release_config_filename()),
  {ok, Config} = file:consult(Filename),
  Config2 = Tool:release_config(Vsn, Config),
  Config3 = lists:flatten([io_lib:fwrite("~p.~n", [Term]) || Term <- Config2]),
  file:write_file(Filename, Config3).

app_config(#{ tool := Tool, path := Path }, AppName) ->
  AppPath = filename:join(Path, Tool:app_path(AppName)),
  Filename = filename:join(AppPath, Tool:app_config_filename(AppName)),
  {ok, Config} = file:consult(Filename),
  AppVsn = beamup_git:commit_hash(AppPath),
  Config2 = Tool:app_config(AppVsn, Config),
  file:write_file(Filename, io_lib:fwrite("~p.~n", [Config2])).
