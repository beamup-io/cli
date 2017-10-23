-module(beamup_build_tool_rebar3).

-export([name/0, detect/1]).

name() -> rebar3.

detect(Filenames) ->
  lists:any(fun(Filename) -> Filename == "rebar.config" end, Filenames).
