-module(beamup_build_tool_rebar3).

-export([name/0, detect/1, deps/1]).

name() -> rebar3.

detect(Filenames) ->
  lists:any(fun(Filename) -> Filename == "rebar.config" end, Filenames).

deps(Path) ->
  rebar3("get-deps", Path).

rebar3(Args, Path) ->
  beamup_shell:cmd("rebar3 " ++ Args,
    [{cd, Path},
    {env, [{"REBAR_CACHE_DIR", "/host/cache/rebar3"},
           {"TERM", "dumb"}]}],
    fun(Bytes) -> io:put_chars(Bytes) end).

% replace_version(Version, Term) ->
