-module(beamup_project_name_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  beamup_project_name_sup:start_link().

stop(_State) ->
  ok.
