-module(beamup_store).

-export([new/2,
         put/3,
         get/3,
         versions/2]).

new(Url, Secret) ->
  #{url => Url, secret => Secret}.

put(Store, Project, TarPath) ->
  Payload = {file, TarPath},
  Version = maps:get(version, Project),
  Path = to_path(Project),
  Path2 = <<Path/binary, $/, Version/binary>>,
  ReqHeaders = [{<<"Content-Type">>, <<"application/gzip">>}],
  request(Store, post, Path2, Payload, ReqHeaders).

get(Store, Project, Version) ->
  Path = to_path(Project),
  Path2 = <<Path/binary, $/, Version/binary>>,
  ReqHeaders = [{<<"Accept">>, <<"application/gzip">>}],
  Blob = request(Store, get, Path2, <<>>, ReqHeaders),
  TempPath = temp_tar_path(Version),
  ok = file:write_file(TempPath, Blob),
  TempPath.

versions(Store, Project) ->
  Path = to_path(Project),
  List = from_etf(request(Store, get, Path, <<>>, [])),
  case List of
    ok -> [];
    _ -> List
  end.

% Private

temp_tar_path(Version) ->
  Dir = <<"/tmp/beamup/releases/">>,
  filelib:ensure_dir(Dir),
  <<Dir/binary, Version/binary, ".tar.gz">>.

request(Store, Verb, Path, Payload, ReqHeaders) ->
  application:ensure_all_started(hackney),
  ReqHeaders2 = ReqHeaders ++
    [{<<"User-Agent">>, <<"beamup-builder/0.1 hackney/*">>}],
  Options = [{follow_redirect, true},
            {max_redirect, 5},
            {basic_auth, {<<"key">>, maps:get(secret, Store)}}],
  BaseUrl = maps:get(url, Store),
  Url = <<BaseUrl/binary, Path/binary>>,
  {ok, Status, ResHeaders, Client} = hackney:request(Verb, Url, ReqHeaders2, Payload, Options),
  {ok, Body} = hackney:body(Client),
  io:format("Status: ~p, ResHeaders: ~p~n", [Status, ResHeaders]),
  Body.

from_etf(Body) ->
  case Body of
    <<>> -> ok;
    Etf ->
      Term = binary_to_term(Etf, [safe]),
      io:format("Response: ~p~n", [Term]),
      Term
  end.

to_path(#{name := Name,
          architecture := Architecture,
          branch := Branch}) ->
  <<$/, Name/binary,
    "/release/",
    Architecture/binary, $/,
    Branch/binary>>.
