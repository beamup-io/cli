-module(beamup_project).

-export([new/1]).

new(Path) ->
  #{path => Path,
    name => path_to_name(Path),
    commit => beamup_git:commit_hash(Path),
    tool => beamup_build_tool:detect(Path),
    branch => beamup_git:branch(Path)}.

path_to_name(Path) ->
  lists:last(string:tokens(Path,"/")).
