-module(beamup_build_tool_rebar3).

-export([name/0,
         detect/1,
         deps/1,
         compile/1,
         appups/2,
         relup/2,
         tar/1,
         release_config_filename/0,
         release_config/2,
         app_names/1,
         app_path/1,
         app_config_filename/1,
         app_config/2,
         update_paths/2]).

name() -> rebar3.

detect(Filenames) ->
  lists:any(fun(Filename) -> Filename == "rebar.config" end, Filenames).

deps(Path) ->
  rebar3(<<"get-deps">>, Path).

compile(#{ path := Path }) ->
  rebar3(<<"release">>, Path).

appups(#{path := CurrentPath, name := Name}, #{path := PreviousPath}) ->
  rebar3(<<"appup generate",
           " --previous ", PreviousPath/binary>>,
         CurrentPath, verbose),
  % Move the *.appups from lib into the release directory
  Apps = filename:join([CurrentPath, "_build", "default", "lib"]),
  Appups = filelib:fold_files(Apps, "\\.appup$", true, fun (SourceAppupPath, Acc) ->
    AppName = filename:basename(SourceAppupPath, ".appup"),
    {ok, [{AppVersion, _, _}]} = file:consult(SourceAppupPath),
    AppVersionBin = list_to_binary(AppVersion),
    TargetApp = <<AppName/binary, $-, AppVersionBin/binary>>,
    TargetAppupPath = filename:join([CurrentPath, "_build", "default", "rel",
                   Name, "lib", TargetApp, "ebin", filename:basename(SourceAppupPath)]),
    {ok, _} = file:copy(SourceAppupPath, TargetAppupPath),
    [TargetAppupPath] ++ Acc
  end, []).

relup(#{path := CurrentPath, name := Name, version := CurrentVersion},
      #{path := PreviousPath, version := PreviousVersion}) ->
  io:format("Generating relup: ~p<>~p~n", [CurrentVersion, PreviousVersion]),
  % Hack: Copy all apps of the previous releases' lib dir over
  % to the current dir so that the relup provider finds them
  % This is also the reason why we have all that project tree copying
  % going on because having multiple apps in the current releases' lib dir
  % confuses the relup generator.
  LocalLibPath = <<CurrentPath/binary, "/_build/default/rel/", Name/binary, "/lib/">>,
  beamup_shell:cmd(<<"cp -vR ",
                     PreviousPath/binary, "/lib/*",
                     " ",
                     LocalLibPath/binary>>),

  rebar3(<<"relup",
           " --relname ", Name/binary,
           " --relvsn ", CurrentVersion/binary,
           " --upfrom ", PreviousVersion/binary,
           " --lib-dir ", PreviousPath/binary>>,
         CurrentPath, verbose),
  io:format("Generated relup: ~p<>~p~n", [CurrentVersion, PreviousVersion]),
  filename:join([CurrentPath, "_build", "default", "rel", Name, "releases", CurrentVersion, "relup"]).

tar(#{ name := Name, path := Path, version := Version }) ->
  rebar3(<<"tar">>, Path),
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
  Config6 = beamup_util:override(relx, 1, include_src, 1, Config5, {include_src, false}),
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

% Rebar3 keeps a small file called "erlcinfo" in "_build/default/lib/myrel/.rebar3/erlcinfo"
% which caches the last modified timestamp of the *.erl files, and their absolute path.
% To avoid recompiling the whole project, we have to
%   (i) keep timestamps intact when copying the project folder
%   (ii) and re-write the absolute path to point to the copy
update_paths(#{path := NewRootPath}, OldRootPath) ->
  filelib:fold_files(NewRootPath, "erlcinfo$", true, fun(ErlcinfoFilename, _) ->
    case file:read_file(ErlcinfoFilename) of
      {ok, Bytes} ->
        Term = binary_to_term(Bytes),
        Term2 = update_erlcinfo_term(NewRootPath, OldRootPath, Term),
        Bytes2 = term_to_binary(Term2, [{compressed, 2}]),
        ok = file:write_file(ErlcinfoFilename, Bytes2);
      _ -> ok
    end
  end, []).

% Private

update_erlcinfo_term(NewRootPath, OldRootPath, Term) ->
  {erlcinfo, ErlcinfoVsn, {Vs, Es, InclDirs}} = Term,
  UpdatePath = fun(Path) ->
    update_path(binary_to_list(NewRootPath),
                binary_to_list(OldRootPath),
                Path)
  end,
  Vs2 = update_erlcinfo_vs(Vs, UpdatePath),
  Es2 = update_erlcinfo_es(Es, UpdatePath),
  InclDirs2 = lists:map(UpdatePath, InclDirs),
  {erlcinfo, ErlcinfoVsn, {Vs2, Es2, InclDirs2}}.

update_path(NewRootPath, OldRootPath, Path) ->
  MaybeRelative = string:prefix(Path, OldRootPath),
  case MaybeRelative of
    % Path may be /usr/local/lib...
    nomatch -> Path;
    MaybeRelative ->
      case string:prefix(MaybeRelative, "/") of
        nomatch -> MaybeRelative;
        Relative -> filename:join([NewRootPath, Relative])
    end
  end.

update_erlcinfo_vs([{Path, Date}|T], UpdatePath) ->
  [{UpdatePath(Path), Date}|update_erlcinfo_vs(T, UpdatePath)];
update_erlcinfo_vs([Other|T], UpdatePath) ->
  [Other|update_erlcinfo_vs(T, UpdatePath)];
update_erlcinfo_vs([], _) -> [].

update_erlcinfo_es([{X, PathA, PathB, Y}|T], UpdatePath) ->
  [{X, UpdatePath(PathA), UpdatePath(PathB), Y}|update_erlcinfo_es(T, UpdatePath)];
update_erlcinfo_es([Other|T], UpdatePath) ->
  [Other|update_erlcinfo_es(T, UpdatePath)];
update_erlcinfo_es([], _) -> [].

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
