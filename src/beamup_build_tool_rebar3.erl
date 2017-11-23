-module(beamup_build_tool_rebar3).

-export([name/0,
         detect/1,
         deps/1,
         compile/1,
         appup/2,
         relup/2,
         tar/1,
         release_config_filename/0,
         release_config/2,
         app_names/1,
         app_path/1,
         app_config_filename/1,
         app_config/2]).

name() -> rebar3.

detect(Filenames) ->
  lists:any(fun(Filename) -> Filename == "rebar.config" end, Filenames).

deps(Path) ->
  rebar3(<<"get-deps">>, Path).

compile(#{ path := Path }) ->
  rebar3(<<"release">>, Path).

appup(#{path := CurrentPath}, #{path := PreviousPath}) ->
  rebar3(<<"appup generate",
           " --previous ", PreviousPath/binary>>,
         CurrentPath, verbose).

relup(#{ path := CurrentPath, name := Name }, #{path := PreviousPath, version := PreviousVersion}) ->
  % the relup provider expects the previous release in
  % /beamup/project/myrel/_build/default/rel/myrel/lib/myrel-Vsn/ebin
  % So copy the previous releases' lib dir over there
  beamup_shell:cmd(<<"cp -vR ",
                     PreviousPath/binary, "/lib/*",
                     " ",
                     CurrentPath/binary, "/_build/default/rel/", Name/binary, "/lib/">>,
                   [], fun(B) -> io:put_chars(B) end),

  rebar3(<<"relup",
           " --upfrom ", PreviousVersion/binary,
           " --lib-dir ", PreviousPath/binary>>,
         CurrentPath, verbose).

tar(#{ name := Name, path := Path, version := Version }) ->
  rebar3(<<"appup tar">>, Path),
  filename:join(Path, <<"_build/default/rel/",
                        Name/binary, $/,
                        Name/binary, $-,
                        Version/binary, ".tar.gz">>).

release_config_filename() ->
  <<"rebar.config">>.

release_config(Version, Config) ->
  Config2 = beamup_util:override(erl_opts, 1, Config, {erl_opts, [debug_info]}),
  Config3 = beamup_util:override(plugins, 1, Config2, {plugins, [rebar3_appup_plugin]}),
  Config4 = beamup_util:override(relx, 1, dev_mode, 1, Config3, {dev_mode, false}),
  Config5 = beamup_util:override(relx, 1, include_erts, 1, Config4, {include_erts, true}),
  Config6 = beamup_util:override(relx, 1, include_erts, 1, Config5, {include_source, false}),
  Config7 = beamup_util:override(relx, 1, generate_start_script, 1, Config6, {generate_start_script, true}),
  Config8 = beamup_util:override(relx, 1, extended_start_script, 1, Config7, {extended_start_script, true}),
  ensure_relx_version(Version, Config8).

ensure_relx_version(Version, [{relx, H}|T]) ->
  [{relx, ensure_relx_version(Version, H)}|T];
ensure_relx_version(Version, [{release, {Name, _}, Deps}|T]) ->
  [{release, {Name, binary_to_list(Version)}, Deps}|T];
ensure_relx_version(Version, [H|T]) ->
  [H|ensure_relx_version(Version, T)];
ensure_relx_version(_Version, []) ->
  [].

app_names(Path) ->
  {ok, Names} = file:list_dir(filename:join(Path, <<"apps">>)),
  [list_to_binary(N) || N <- Names].

app_path(AppName) ->
  filename:join(<<"apps">>, AppName).

app_config_filename(AppName) ->
  filename:join(<<"src">>, <<AppName/binary, ".app.src">>).

app_config(Version, [{application, Name, Opts}]) ->
  {application, Name, app_config(Version, Opts)};
app_config(Version, [{vsn, _}|T]) ->
  [{vsn, binary_to_list(Version)}|T];
app_config(Version, [H|T]) ->
  [H|app_config(Version, T)];
app_config(_Version, []) ->
  [].

% Private

rebar3(Args, Path) ->
  rebar3(Args, Path, []).
rebar3(Args, Path, verbose) ->
  rebar3(Args, Path, [{env, [{"DEBUG", "1"}]}]);
rebar3(Args, Path, Opts) ->
  io:format("Running rebar3 ~p~n", [Args]),
  {ExitCode, _} = beamup_shell:cmd(<<"rebar3 ", Args/binary>>,
    [{cd, Path}] ++ Opts,
    fun(Bytes) -> io:put_chars(Bytes) end),
  case ExitCode of
    0 -> ok;
    _ -> throw(rebar3_error)
  end.
