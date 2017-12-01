-module(beamup_store).

-export([new/2,
         put/3,
         get/3,
         versions/2]).

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

% Private


select_backend(<<"http", _/binary>>) ->
  beamup_store_http;
select_backend(<<"/", _/binary>>) ->
  beamup_store_fs;
select_backend(UrlOrPath) ->
  io:format("Invalid Store URL or Path: ~p~n", [UrlOrPath]),
  halt(1).
