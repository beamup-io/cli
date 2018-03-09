-module(beamup_project).

-export([architecture/1,
         new/1,
         new_remote/2,
         path/1,
         branch/1,
         duplicate/1,
         remove/1]).

new(Path) ->
  Project = #{path => Path,
    name => name_from_path(Path),
    architecture => current_architecture(),
    branch => beamup_git:branch(Path),
    version => beamup_git:commit_hash(Path),
    tool => beamup_build_tool:detect(Path)},
  duplicate(Project).

new_remote(Name, Branch) ->
  #{name => Name,
    architecture => current_architecture(),
    branch => Branch}.

path(Project) ->
  maps:get(path, Project).

branch(Project) ->
  maps:get(branch, Project).

architecture(Project) ->
  maps:get(architecture, Project).

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

current_architecture() ->
  list_to_binary(erlang:system_info(system_architecture)).
