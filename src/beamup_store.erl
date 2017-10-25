-module(beamup_store).

-export([put/2]).

put(_Project, Tar) ->
  Filename = lists:last(filename:split(Tar)),
  file:copy(Tar, filename:join("/host/releases", Filename)).
