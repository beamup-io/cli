-module(beamup_fileutil).

-export([temp_dir/1, copy/2, remove/1]).

temp_dir(Name) ->
  Prefix = <<"/tmp/beamup">>,
  Id = erlang:unique_integer([positive]),
  Dir = filename:join([Prefix, integer_to_list(Id), Name]),
  ok = filelib:ensure_dir(Dir),
  Dir.

copy(From, To) ->
  cmd(<<"cp -pr ",
        From/binary,
        " ",
        To/binary>>).

remove(Path) ->
  cmd(<<"rm -rf ",
        Path/binary>>).

% Private

cmd(Cmd) ->
  {0, _} = beamup_shell:cmd(Cmd, [], fun(B) -> io:put_chars(B) end),
  ok.
