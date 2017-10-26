-module(beamup_git).

-export([
  commit_hash/1,
  branch/1,
  untracked_files/1,
  is_dirty/1,
  status/1,
  local_branches/1,
  commit_hashes/1, commit_hashes/2,
  commit_hashes_by_branches/1]).

commit_hash(Path) ->
  {_, Hash} = beamup_shell:cmd("git rev-list -1 HEAD -- " ++ Path),
  Hash.

commit_hashes(Path) ->
    commit_hashes(Path, "HEAD").
commit_hashes(Path, Branch) ->
  {_, Hashes} = beamup_shell:cmd("git rev-list " ++ Branch, [{cd, Path}]),
  lines_to_list(Hashes).

commit_hashes_by_branches(Path) ->
  lists:map(fun(Branch) ->
    {Branch, commit_hashes(Path, Branch)}
  end, local_branches(Path)).

branch(Path) ->
  {_, Branch} = beamup_shell:cmd("git rev-parse --abbrev-ref HEAD", [{cd, Path}]),
  Branch.

local_branches(Path) ->
  {_, Branches} = beamup_shell:cmd("git for-each-ref --format='%(refname:short)' refs/heads/", [{cd, Path}]),
  lines_to_list(Branches).

untracked_files(Path) ->
  {_, Untracked} = beamup_shell:cmd("git ls-files --exclude-standard --others", [{cd, Path}]),
  lines_to_list(Untracked).

is_dirty(Path) ->
  % Refresh index because files have been touched
  % See: https://stackoverflow.com/questions/34807971
  beamup_shell:cmd("git update-index --refresh", [{cd, Path}]),
  {ExitCode, _} = beamup_shell:cmd("git diff-index --quiet HEAD --", [{cd, Path}]),
  case ExitCode of
    0 -> false;
    _ -> true
  end.

status(Path) ->
  {_, Status} = beamup_shell:cmd("git status", [{cd, Path}]),
  Status ++ "~n".

lines_to_list(Text) ->
  string:lexemes(Text, [$\n]).
