-module(beamup_store_http).

-export([new/2,
         put/3,
         get/3,
         versions/2,
         subscribe/3]).

-export([handle_subscription/1]).

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
  {ok, File} = download_file(ConnPid, StreamRef),
  TempPath = temp_tar_path(Version),
  ok = file:write_file(TempPath, File),
  TempPath.

versions(Store, Project) ->
  Path = to_path(Project),
  Headers = [{<<"accept">>, <<"application/erlang">>}] ++ headers(Store),
  ConnPid = maps:get(connection, Store),
  StreamRef = gun:get(ConnPid,
                      Path,
                      Headers),
  case gun:await(ConnPid, StreamRef) of
    {response, _Fin, 404, _ResHeaders} ->
      [];
    {response, _Fin, 200, _ResHeaders} ->
      {ok, Body2} = gun:await_body(ConnPid, StreamRef),
      List = from_etf(Body2),
      case List of
        ok -> [];
        _ -> List
      end
  end.

subscribe(Store, Project, SubscriberPid) ->
  ok.
  % io:format("Subscribing ~p", [SubscriberPid]),
  % Path = to_path(Project, subscribe),
  % Headers = [{<<"accept">>, <<"text/event-stream">>}] ++ headers(Store),
  % ConnPid = maps:get(connection, Store),
  % HandlerPid = spawn(?MODULE, handle_subscription, [SubscriberPid]),
  % gun:get(ConnPid,
  %         Path,
  %         Headers,
  %         #{reply_to => HandlerPid}).

handle_subscription(SubscriberPid) ->
  receive
    {gun_response, _ConnPid, _StreamRef, nofin, Status, Headers} ->
      io:format("1 ~p ~p", [Status, Headers]),
      handle_subscription2(SubscriberPid);
    Msg ->
      io:format("Unknown response ~p~n", [Msg])
  after 5000 ->
    io:format("Timeout1~n"),
    exit(timeout)
  end.

handle_subscription2(SubscriberPid) ->
  receive
    {gun_sse, Pid, Ref, Event} ->
      io:format("event ~p", [Event]),
      handle_subscription2(SubscriberPid);
    {'DOWN', _MRef, process, _ConnPid, Reason} ->
      exit(Reason);
    Msg ->
      io:format("Msg ~p", [Msg]),
      handle_subscription2(SubscriberPid)
  after 5000 ->
    io:format("Timeout2~n"),
    handle_subscription2(SubscriberPid)
  end.


% Private

download_file(ConnPid, StreamRef) ->
  receive
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
      {ok, <<BodyAcc/binary, Body/binary>>};
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
    Etf -> binary_to_term(Etf, [safe])
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
    http -> gun:open(Host, Port)
  end,
  {ok, Protocol} = gun:await_up(ConnPid),
  io:format("Connected to store at ~p://~s:~p~n", [Protocol, Host, Port]),
  ConnPid.

assert_success(S) when 200 =< S, S =< 299 ->
  ok;
assert_success(S) ->
  io:format("HTTP ~p~n", [S]),
  error.
