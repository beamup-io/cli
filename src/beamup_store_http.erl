-module(beamup_store_http).

-export([new/2,
         put/3,
         get/3,
         versions/2,
         subscribe/2]).

new(Url, Secret) ->
  ConnPid = open_connection(Url),
  #{url => Url,
    secret => Secret,
    connection => ConnPid}.

put(Store, Project, TarPath) ->
  Version = maps:get(version, Project),
  Path = to_path(Project),
  Path2 = <<Path/binary, $/, Version/binary>>,
  Headers = [{<<"content-type">>, <<"application/gzip">>}] ++ headers(Store),
  ConnPid = maps:get(connection, Store),
  StreamRef = gun:put(ConnPid,
                      Path2,
                      Headers),
  {ok, IoDevice} = file:open(TarPath, [read, binary, raw]),
  upload_file(ConnPid, StreamRef, IoDevice),
  {response, fin, Status, _} = gun:await(ConnPid, StreamRef),
  ok = assert_success(Status).

get(Store, Project, Version) ->
  Path = to_path(Project),
  Path2 = <<Path/binary, $/, Version/binary>>,
  Headers = [{<<"accept">>, <<"application/gzip">>}] ++ headers(Store),
  ConnPid = maps:get(connection, Store),
  StreamRef = gun:get(ConnPid,
                      Path2,
                      Headers),
  File = download_file(ConnPid, StreamRef),
  TempPath = temp_tar_path(Version),
  ok = file:write_file(TempPath, File),
  TempPath.

versions(Store, Project) ->
  Path = to_path(Project),
  Headers = headers(Store),
  ConnPid = maps:get(connection, Store),
  StreamRef = gun:get(ConnPid,
                      Path,
                      Headers),
  {Status, Body} = case gun:await(ConnPid, StreamRef) of
    {response, fin, _Status, _ResHeaders} ->
      no_data;
    {response, nofin, ResStatus, _ResHeaders} ->
      {ok, Body2} = gun:await_body(ConnPid, StreamRef),
      io:format("~s~n", [Body2]),
      {ResStatus, Body2}
  end,
  assert_success(Status),
  List = from_etf(Body),
  case List of
    ok -> [];
    _ -> List
  end.

subscribe(_Store, _Project) ->
  {}.

% Private

download_file(ConnPid, StreamRef) ->
  receive
    {gun_response, ConnPid, StreamRef, fin, _Status, _Headers} ->
      no_data;
    {gun_response, ConnPid, StreamRef, nofin, _Status, _Headers} ->
      receive_data(ConnPid, StreamRef, <<>>);
    {'DOWN', _MRef, process, ConnPid, Reason} ->
      exit(Reason)
  after 1000 ->
    exit(timeout)
  end.

receive_data(ConnPid, StreamRef, BodyAcc) ->
  receive
    {gun_data, ConnPid, StreamRef, nofin, Body} ->
      receive_data(ConnPid, StreamRef, <<BodyAcc/binary, Body/binary>>);
    {gun_data, ConnPid, StreamRef, fin, Body} ->
      <<BodyAcc/binary, Body/binary>>;
    {'DOWN', _MRef, process, ConnPid, Reason} ->
      exit(Reason)
  after 1000 ->
    exit(timeout)
  end.

upload_file(ConnPid, StreamRef, IoDevice) ->
  case file:read(IoDevice, 8000) of
    eof ->
      gun:data(ConnPid, StreamRef, fin, <<>>),
      file:close(IoDevice);
    {ok, Bin} ->
      gun:data(ConnPid, StreamRef, nofin, Bin),
      upload_file(ConnPid, StreamRef, IoDevice)
  end.

temp_tar_path(Version) ->
  Dir = <<"/tmp/beamup/releases/">>,
  filelib:ensure_dir(Dir),
  <<Dir/binary, Version/binary, ".tar.gz">>.

from_etf(Body) ->
  case Body of
    <<>> -> ok;
    Etf ->
      Term = binary_to_term(Etf, [safe]),
      io:format("Response: ~p~n", [Term]),
      Term
  end.

to_path(Project) ->
  to_path(Project, <<"release">>).
to_path(Project, subscribe) ->
  to_path(Project, <<"subscribe">>);
to_path(#{name := Name,
          architecture := Architecture,
          branch := Branch}, Type) ->
  <<$/, Name/binary,
    $/, Type/binary, $/,
    Architecture/binary, $/,
    Branch/binary>>.

auth_header(#{secret := Secret}) ->
  Auth = base64:encode(<<"key", $:, Secret/binary>>),
  {<<"authorization">>, <<"Basic ", Auth/binary>>}.

headers(Store) ->
  [{<<"user-agent">>, <<"beamup/* gun/*">>},
   auth_header(Store)].

open_connection(Url) ->
  application:ensure_all_started(gun),
  {ok, {Scheme, _, HostBinary, Port, _, _}} = http_uri:parse(Url),
  Host = binary_to_list(HostBinary),
  {ok, ConnPid} = case Scheme of
    https -> gun:open(Host, Port, #{transport => ssl});
    _ -> gun:open(Host, Port)
  end,
  {ok, _Protocol} = gun:await_up(ConnPid),
  ConnPid.

assert_success(S) when 200 =< S, S =< 299 ->
  ok;
assert_success(_) ->
  error.
