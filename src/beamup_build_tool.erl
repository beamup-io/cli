-module(beamup_build_tool).

-export([supported_tools/0, supported_tools_names/0, detect/1, deps/1]).

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
