-module(beamup_store_fs).

-export([new/2,
         put/3,
         get/3,
         versions/2]).

new(StorePath, _) ->
  ok = filelib:ensure_dir(<<StorePath/binary, "/">>),
  #{path => StorePath}.

put(#{path := StorePath}, Project, TarPath) ->
  Dest = to_filename(Project),
  Dest2 = filename:join([StorePath, Dest]),
  io:format("Storing at ~p~n", [Dest2]),
  {ok, _} = file:copy(TarPath, Dest2).

get(#{path := StorePath}, Project, Version) ->
  Project2 = override_version(Project, Version),
  Filename = to_filename(Project2),
  filename:join([StorePath, Filename]).

versions(#{path := StorePath}, #{name := Name}) ->
  {ok, Filenames} = file:list_dir(StorePath),
  lists:filtermap(fun(Filename) ->
    case from_filename(Filename) of
      {true, {Name, Vsn}} -> {true, Vsn};
      _ -> false
    end
  end, Filenames).

% Private

from_filename(Filename) ->
  FullSuffix = ".tar.gz",
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

to_filename(#{name := Name,
              architecture := Architecture,
              branch := Branch,
              version := Version}) ->
  <<Name/binary, $-, Architecture/binary, $-, Branch/binary, $-, Version/binary, ".tar.gz">>.

override_version(Project, Version) ->
  maps:update(version, Version, Project).
