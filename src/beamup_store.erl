-module(beamup_store).

-export([new/2,
         put/3,
         get/3,
         versions/2,
         subscribe/3]).

new(UrlOrPath, Secret) ->
  Backend = select_backend(UrlOrPath),
  New = Backend:new(UrlOrPath, Secret),
  maps:put(backend, Backend, New).

put(#{backend := Backend} = Store, Project, TarPath) ->
  Backend:put(Store, Project, TarPath).

get(#{backend := Backend} = Store, Project, Version) ->
  Backend:get(Store, Project, Version).

versions(#{backend := Backend} = Store, Project) ->
  Backend:versions(Store, Project).

subscribe(#{backend := Backend} = Store, Project, SubscriberPid) ->
  Backend:subscribe(Store, Project, SubscriberPid).

% Private


select_backend(<<"http", _/binary>>) ->
  beamup_store_http;
select_backend(<<"/", _/binary>>) ->
  beamup_store_fs;
select_backend(UrlOrPath) ->
  io:format("Invalid store URL or path: ~p~n", [UrlOrPath]),
  io:format("Please check that the environment variable BEAMUP_STORE is set~n"),
  io:format("to either a valid URL, or an absolute file system path:~n"),
  io:format("  export BEAMUP_STORE=https://store.example.com~n"),
  io:format("  export BEAMUP_STORE=/tmp/store~n"),
  halt(1).
