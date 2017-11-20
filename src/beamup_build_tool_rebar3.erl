-module(beamup_build_tool_rebar3).

-export([name/0,
         detect/1,
         deps/1,
         release/2,
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

release(#{ name := Name, path := Path, version := Version }, full) ->
  rebar3(<<"tar">>, Path),
  filename:join(Path, <<"_build/default/rel/",
                        Name/binary, $/,
                        Name/binary, $-,
                        Version/binary, ".tar.gz">>);

release(#{ name := Name, path := Path, version := Version },
        {upgrade, UpFromVersion, PreviousPath}) ->
  rebar3(<<"appup generate --previous ",
           PreviousPath/binary,
           " --previous_version ",
           UpFromVersion/binary>>, Path),
  rebar3(<<"appup compile">>, Path),
  rebar3(<<"relup --name ", Name/binary,
           " --upfrom ", UpFromVersion/binary,
           " --relVersion ", Version/binary>>, Path),
  rebar3(<<"appup tar">>, Path),
  filename:join(Path, <<"_build/default/rel/",
                        Name/binary, $/,
                        Name/binary, $-, Version/binary, ".tar.gz">>).

rebar3(Args, Path) ->
  io:format("Running rebar3 ~p~n", [Args]),
    [{cd, Path}],
  {ExitCode, _} = beamup_shell:cmd(<<"rebar3 ", Args/binary>>,
    fun(Bytes) -> io:put_chars(Bytes) end),
  case ExitCode of
    0 -> ok;
    _ -> throw(rebar3_error)
  end.

release_config_filename() ->
  <<"rebar.config">>.

release_config(Vsn, Config) ->
  Config2 = beamup_util:override(erl_opts, 1, Config, {erl_opts, [debug_info]}),
  Config3 = beamup_util:override(plugins, 1, Config2, {plugins, [rebar3_appup_plugin]}),
  Config4 = beamup_util:override(relx, 1, dev_mode, 1, Config3, {dev_mode, false}),
  Config5 = beamup_util:override(relx, 1, include_erts, 1, Config4, {include_erts, true}),
  Config6 = beamup_util:override(relx, 1, generate_start_script, 1, Config5, {generate_start_script, true}),
  Config7 = beamup_util:override(relx, 1, extended_start_script, 1, Config6, {extended_start_script, true}),
  ensure_relx_version(Vsn, Config7).

ensure_relx_version(Vsn, [{relx, H}|T]) ->
  [{relx, ensure_relx_version(Vsn, H)}|T];
ensure_relx_version(Vsn, [{release, {Name, _}, Deps}|T]) ->
  [{release, {Name, Vsn}, Deps}|T];
ensure_relx_version(Vsn, [H|T]) ->
  [H|ensure_relx_version(Vsn, T)];
ensure_relx_version(_Vsn, []) ->
  [].

app_names(Path) ->
  {ok, Names} = file:list_dir(filename:join(Path, <<"apps">>)),
  [list_to_binary(N) || N <- Names].

app_path(AppName) ->
  filename:join(<<"apps">>, AppName).

app_config_filename(AppName) ->
  filename:join(<<"src">>, <<AppName/binary, ".app.src">>).

app_config(Vsn, [{application, Name, Opts}]) ->
  {application, Name, app_config(Vsn, Opts)};
app_config(Vsn, [{vsn, _}|T]) ->
  [{vsn, Vsn}|T];
app_config(Vsn, [H|T]) ->
  [H|app_config(Vsn, T)];
app_config(_Vsn, []) ->
  [].
