-module(beamup_version).

-export([previous/1, previous/2]).

previous(#{path := Path} = Project) ->
  StoredVersions = beamup_store:versions(Project),
  LocalVersions = beamup_git:commit_hashes(Path),
  previous(LocalVersions, StoredVersions).

previous([], _) -> false;
previous(_, []) -> false;
previous([_|Local], Stored) -> prev(Local, Stored, Local, Stored).

prev(_Local, _Stored, [Vsn|_], [Vsn|_]) -> Vsn;
prev(_Local, _Stored, [], _) -> false;
prev(L, S, [_|NextLocal], []) -> prev(L, S, NextLocal, S);
prev(L, S, Local, [_|NextStored]) -> prev(L, S, Local, NextStored).
