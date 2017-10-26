-module(beamup_store).

-export([put/2,
         versions/1]).

% NOTE: This is a mock implementation using the local fs
% TODO: Replace with real release store API calls

put(Project, Tar) ->
  Filename = to_filename(Project),
  file:copy(Tar, filename:join("/host/releases", Filename)).

versions(#{name := Name}) ->
  {ok, Filenames} = file:list_dir("/host/releases"),
  lists:filtermap(fun(Filename) ->
    case from_filename(Filename) of
      {true, {Name, Vsn}} -> {true, Vsn};
      _ -> false
    end
  end, Filenames).

to_filename(#{name := Name, commit := Commit}) ->
  Name ++ "-" ++ Commit ++ ".tar.gz".

from_filename(Filename) ->
  TotalLength = string:length(Filename),
  SuffixLength = string:length(".tar.gz"),
  HashLength = 40,
  case TotalLength > (HashLength + SuffixLength) of
    true ->
      PrefixLength = TotalLength - SuffixLength - HashLength - 1,
      Name = string:substr(Filename, 1, PrefixLength),
      Vsn = string:substr(Filename, PrefixLength + 2, HashLength),
      {true, {Name, Vsn}};
    false -> false
  end.
