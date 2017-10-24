-module(beamup_build_tool_rebar3).

-export([name/0,
         detect/1,
         deps/1,
         full_release/1,
         release_config_filename/0,
         release_config/2]).

name() -> rebar3.

detect(Filenames) ->
  lists:any(fun(Filename) -> Filename == "rebar.config" end, Filenames).

deps(Path) ->
  rebar3("get-deps", Path).

full_release(Path) ->
  rebar3("tar", Path).

rebar3(Args, Path) ->
  {ExitCode, _} = beamup_shell:cmd("rebar3 " ++ Args,
    [{cd, Path},
    {env, [{"REBAR_CACHE_DIR", "/host/cache/rebar3"},
           {"TERM", "dumb"}]}],
    fun(Bytes) -> io:put_chars(Bytes) end),
  case ExitCode of
    0 -> ok;
    _ -> throw(rebar3_error)
  end.

release_config_filename() ->
  "rebar.config".

release_config(Vsn, [{relx, H}|T]) ->
  [{relx, release_config(Vsn, H)}|T];
release_config(Vsn, [{release, {Name, _}, Deps}|T]) ->
  [{release, {Name, Vsn}, Deps}|release_config(Vsn, T)];
release_config(Vsn, [{dev_mode, _}|T]) ->
  [{dev_mode, false}|release_config(Vsn, T)];
release_config(Vsn, [{include_erts, _}|T]) ->
  [{include_erts, true}|release_config(Vsn, T)];
release_config(Vsn, [H|T]) ->
  [H|release_config(Vsn, T)];
release_config(_Vsn, []) ->
  [].

