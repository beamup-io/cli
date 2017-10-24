-module(beamup_sanity_check).

-export([check/1]).

check(Path) ->
  ok = ensure_clean_working_tree(Path),
  ensure_supported_build_tool(Path).

ensure_clean_working_tree(Path) ->
  Dirty = beamup_git:is_dirty(Path),
  UntrackedFiles = beamup_git:untracked_files(Path),
  case (not Dirty and is_empty(UntrackedFiles)) of
    true -> ok;
    false ->
      io:format("Dirty: ~p~n", [Dirty]),
      io:format("Untracked Files: ~p~n", [UntrackedFiles]),
      io:format(beamup_git:status(Path)),
      io:format(standard_error, "Please commit or stash all changes.~n", []),
      halt(1)
  end.

is_empty([]) -> true;
is_empty(_) -> false.

ensure_supported_build_tool(Path) ->
  Tool = beamup_build_tool:detect(Path),
  case Tool of
    false ->
      io:format("Could not detect bulid tool.~n"),
      io:format("Please make sure to use one of the supported tools: ~p~n",
        [beamup_build_tool:supported_tools_names()]),
      io:format("and that your project follows a standard (OTP) directory structure.~n"),
      halt(1);
    { Module, true } -> Module
  end.
