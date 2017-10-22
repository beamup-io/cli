-module(beamup_shell).

-export([cmd/2, cmd/1]).

cmd(Command) -> cmd(Command, []).
cmd(Command, Options) ->
  Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status] ++ Options),
  get_data(Port, []).

get_data(Port, Sofar) ->
  receive
    {Port, {data, Bytes}} ->
      get_data(Port, [Sofar|Bytes]);
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
      {ExitCode, string:trim(lists:flatten(Sofar), trailing)}
  end.
