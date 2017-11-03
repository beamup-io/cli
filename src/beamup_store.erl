-module(beamup_store).

-export([put/3,
         get/2,
         versions/1]).

% NOTE: This is a mock implementation using the local fs
% TODO: Replace with real release store API calls

put(Project, Tar, FullOrUpgrade) ->
  Filename = to_filename(Project, FullOrUpgrade),
  file:copy(Tar, filename:join("/host/releases", Filename)).

get(Project, FullOrUpgrade) ->
  Filename = to_filename(Project, FullOrUpgrade),
  filename:join("/host/releases", Filename).

versions(#{name := Name}) ->
  {ok, Filenames} = file:list_dir("/host/releases"),
  lists:filtermap(fun(Filename) ->
    case from_filename(Filename) of
      {true, {Name, Vsn}} -> {true, Vsn};
      _ -> false
    end
  end, Filenames).

to_filename(#{commit := Vsn} = Project, full) ->
  to_filename(Project, {full, Vsn});
to_filename(#{name := Name}, {full, Vsn}) ->
  Name ++ "-" ++ Vsn ++ "-full.tar.gz";
to_filename(#{name := Name, commit := Commit}, {upgrade, UpFromVsn}) ->
  Name ++ "-" ++ Commit ++ "-from-" ++ UpFromVsn ++ ".tar.gz".

from_filename(Filename) ->
  FullSuffix = "-full.tar.gz",
  TotalLength = string:length(Filename),
  SuffixLength = string:length(FullSuffix),
  HashLength = 40,
  case string:find(Filename, FullSuffix, trailing) of
    nomatch -> false;
    _ -> case TotalLength > (HashLength + SuffixLength) of
      true ->
        PrefixLength = TotalLength - SuffixLength - HashLength - 1,
        Name = string:substr(Filename, 1, PrefixLength),
        Vsn = string:substr(Filename, PrefixLength + 2, HashLength),
        {true, {Name, Vsn}};
      false -> false
    end
  end.
