-module(beamup_shell).

-export([cmd/3, cmd/2, cmd/1]).

cmd(Command) -> cmd(Command, []).
cmd(Command, Options) ->
  cmd(Command, Options, fun (_) -> ok end).
cmd(Command, Options, Handler) ->
  Port = open_port({spawn, Command},
    lists:flatten([stream, eof, exit_status, Options])),
  get_data(Port, [], Handler).

get_data(Port, Sofar, Handler) ->
  receive
    {Port, {data, Bytes}} ->
      Handler(Bytes),
      get_data(Port, [Sofar|Bytes], Handler);
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
