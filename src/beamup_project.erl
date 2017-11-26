-module(beamup_project).

-export([new/1,
         path/1,
         duplicate/1,
         remove/1]).

new(Path) ->
  Project = #{path => Path,
    name => name_from_path(Path),
    architecture => architecture(),
    branch => beamup_git:branch(Path),
    version => beamup_git:commit_hash(Path),
    tool => beamup_build_tool:detect(Path)},
  duplicate(Project).

path(Project) ->
  maps:get(path, Project).

duplicate(Project) ->
  OldPath = maps:get(path, Project),
  NewPath = beamup_fileutil:temp_dir(maps:get(name, Project)),
  beamup_fileutil:copy(OldPath, NewPath),
  NewProject = maps:update(path, NewPath, Project),
  beamup_build_tool:update_paths(NewProject, OldPath),
  NewProject.

remove(Project) ->
  Dir = maps:get(path, Project),
  beamup_fileutil:remove(Dir).

% Private

name_from_path(Path) ->
  lists:last(string:lexemes(Path,"/")).

architecture() ->
  list_to_binary(erlang:system_info(system_architecture)).
