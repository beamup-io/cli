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

versions(#{path := StorePath}, Project) ->
  {ok, Filenames} = file:list_dir(StorePath),
  lists:filtermap(fun(Filename) ->
    case from_filename(Project, Filename) of
      {true, Vsn} -> {true, Vsn};
      _ -> false
    end
  end, Filenames).

% Private

from_filename(#{name := Name,
                architecture := Architecture,
                branch := Branch}, Filename) ->
  Prefix = <<Name/binary, $-, Architecture/binary, $-,
             Branch/binary, $->>,
  Suffix = ".tar.gz",
  case string:find(Filename, Prefix) of
    nomatch -> false;
    _ -> WithoutPrefix = string:slice(Filename, string:length(Prefix)),
      VsnLength = string:length(WithoutPrefix) - string:length(Suffix),
      WithoutSuffix = string:substr(WithoutPrefix, 1, VsnLength),
      Vsn = list_to_binary(WithoutSuffix),
      {true, Vsn}
  end.

to_filename(#{name := Name,
              architecture := Architecture,
              branch := Branch,
              version := Version}) ->
  <<Name/binary, $-, Architecture/binary, $-, Branch/binary, $-, Version/binary, ".tar.gz">>.

override_version(Project, Version) ->
  maps:update(version, Version, Project).
