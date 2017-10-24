-module(beamup_git).

-export([commit_hash/1, untracked_files/1, is_dirty/1, status/1]).

commit_hash(Path) ->
  { ExitCode, Hash } = beamup_shell:cmd("git rev-list -1 HEAD -- " ++ Path),
  case ExitCode of
    0 -> Hash;
    _ -> error
  end.

untracked_files(Path) ->
  { _, List } = beamup_shell:cmd("git ls-files --exclude-standard --others", [{cd, Path}]),
  List.

is_dirty(Path) ->
  % Refresh index because files have been touched
  % See: https://stackoverflow.com/questions/34807971
  beamup_shell:cmd("git update-index --refresh", [{cd, Path}]),
  { ExitCode, _ } = beamup_shell:cmd("git diff-index --quiet HEAD --", [{cd, Path}]),
  case ExitCode of
    0 -> false;
    _ -> true
  end.

status(Path) ->
  { 0, Status } = beamup_shell:cmd("git status", [{cd, Path}]),
  Status ++ "~n".
