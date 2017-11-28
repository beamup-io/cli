-module(beamup_build_tool).

-export([supported_tools/0,
         supported_tools_names/0,
         detect/1,
         deps/1,
         compile/1,
         appups/2,
         relup/2,
         tar/1,
         extract/1,
         update_paths/2]).

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

compile(#{ tool := Tool, path := Path } = Project) ->
  [app_config(Project, AppName) || AppName <- Tool:app_names(Path)],
  release_config(Project),
  Tool:compile(Project).

appups(Project = #{tool := Tool}, PreviousRelease) ->
  Tool:appups(Project, PreviousRelease).

relup(#{ tool := Tool } = Project, PreviousRelease) ->
  Tool:relup(Project, PreviousRelease).

tar(#{ tool := Tool } = Project) ->
  Tool:tar(Project).

release_config(#{ tool := Tool, path := Path, version := Version }) ->
  Filename = filename:join(Path, Tool:release_config_filename()),
  {ok, Config} = file:consult(Filename),
  Config2 = Tool:release_config(Version, Config),
  Config3 = lists:flatten([io_lib:fwrite("~p.~n", [Term]) || Term <- Config2]),
  io:format("Release Config: ~p~n", [Config2]),
  file:write_file(Filename, Config3).

app_config(#{ tool := Tool, path := ProjectPath }, AppName) ->
  AppPath = filename:join(ProjectPath, Tool:app_path(AppName)),
  Filename = filename:join(AppPath, Tool:app_config_filename(AppName)),
  {ok, Config} = file:consult(Filename),
  AppVersion = beamup_git:commit_hash(AppPath, ProjectPath),
  Config2 = Tool:app_config(AppVersion, Config),
  io:format("App ~p config:~n~p~n", [AppName, Config2]),
  file:write_file(Filename, io_lib:fwrite("~p.~n", [Config2])).

extract(TarPath) ->
  Version = filename:basename(TarPath, ".tar.gz"),
  ParentDir = filename:dirname(TarPath),
  TargetDir = filename:join([ParentDir, Version]),
  io:format("TargetDir: ~p~n", [TargetDir]),
  ok = filelib:ensure_dir(<<TargetDir/binary, $/>>),
  {0, _} = beamup_shell:cmd(<<"tar xvfz ", TarPath/binary>>, [{cd, TargetDir}]),
  TargetDir.

update_paths(#{tool := Tool} = Project, OldPath) ->
  Tool:update_paths(Project, OldPath).
