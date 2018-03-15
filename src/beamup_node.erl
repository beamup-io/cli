-module(beamup_node).

-export([run/4]).

run(Name, WantedBranch, Url, Secret) ->
  Branch = default_branch(WantedBranch),
  BaseDir = <<"/root/.beamup/node">>,
  ReleasesDir = <<BaseDir/binary, "/releases/">>,
  ActiveDir = <<BaseDir/binary, "/active/", Name/binary, $-, Branch/binary>>,
  ok = filelib:ensure_dir(ReleasesDir),
  ok = filelib:ensure_dir(ActiveDir),
  Store = beamup_store:new(Url, Secret),
  Project = beamup_project:new_remote(Name, Branch),
  StoredVersions = beamup_store:versions(Store, Project),
  LatestVersion = get_latest_version(Project, StoredVersions),
  io:format("Downloading~n"),
  TarPath = beamup_store:get(Store, Project, LatestVersion),
  beamup_fileutil:copy(TarPath, ReleasesDir),
  io:format("Extracting~n"),
  beamup_fileutil:extract(TarPath, ActiveDir),
  beamup_fileutil:remove(TarPath),
  ok = filelib:ensure_dir(<<ActiveDir/binary, "/bin">>),
  io:format("Booting~n"),

  Port = open_port({spawn_executable, os:find_executable(binary_to_list(<<ActiveDir/binary, "/bin/", Name/binary>>))},
                   [stream, eof, exit_status,
                    {cd, <<ActiveDir/binary, "/bin/">>},
                    {args, ["foreground"]}]),
  stream(Port, fun(Bytes) -> io:put_chars(Bytes) end).

% Private

stream(Port, Handler) ->
  receive
    {Port, {data, Bytes}} ->
      Binary = list_to_binary(Bytes),
      Handler(Binary),
      stream(Port, Handler);
    {Port, eof} ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          true
      end,
      receive
        {'EXIT', Port, _} ->
          ok
      after 1 ->
        ok
      end,
      ExitCode =
        receive
          {Port, {exit_status, Code}} ->
            Code
      end,
      {ExitCode}
  end.

default_branch(<<>>) ->
  io:format("Warning: Falling back to default branch `master`~n"),
  io:format("  To explicitly set the branch that this node should run~n"),
  io:format("  please pass the environment variable BEAMUP_NODE_BRANCH~n"),
  io:format("    export BEAMUP_NODE_BRANCH=develop~n"),
  <<"master">>;
default_branch(Branch) -> Branch.

get_latest_version(Project, []) ->
  io:format("Warning: No Releases in store for current combination of~n"),
  io:format("  project name, branch, and architecture (~p).~n",
            [beamup_project:architecture(Project)]),
  io:format("  Make sure your store has at lease one Release, and~n"),
  io:format("  double check this node's environment variables:~n"),
  io:format("    BEAMUP_NODE_PROJECT_NAME~n"),
  io:format("    BEAMUP_NODE_BRANCH~n"),
  io:format("    BEAMUP_STORE~n"),
  io:format("    BEAMUP_STORE_SECRET~n");
get_latest_version(_, [H|_]) ->
  H.
