-module(beamup_project).

-export([new/1]).

new(Path) ->
  #{path => Path,
    name => name_from_path(Path),
    architecture => architecture(),
    branch => beamup_git:branch(Path),
    version => beamup_git:commit_hash(Path),
    tool => beamup_build_tool:detect(Path)}.

name_from_path(Path) ->
  lists:last(string:lexemes(Path,"/")).

architecture() ->
  list_to_binary(erlang:system_info(system_architecture)).
