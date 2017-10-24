-module(beamup_build_tool_rebar3).

-export([name/0, detect/1, deps/1]).

name() -> rebar3.

detect(Filenames) ->
  lists:any(fun(Filename) -> Filename == "rebar.config" end, Filenames).

deps(Path) ->
  beamup_shell:cmd("rebar3 get-deps", [{cd, Path}], fun(Bytes) ->
    io:put_chars(Bytes) end).
