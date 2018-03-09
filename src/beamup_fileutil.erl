-module(beamup_fileutil).

-export([extract/2,
         temp_dir/1,
         copy/2,
         remove/1]).

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

extract(TarPath, TargetDir) ->
  ok = filelib:ensure_dir(<<TargetDir/binary, $/>>),
  {0, _} = beamup_shell:cmd(<<"tar xvfz ", TarPath/binary>>, [{cd, TargetDir}]),
  TargetDir.

% Private

cmd(Cmd) ->
  {0, _} = beamup_shell:cmd(Cmd, [], fun(B) -> io:put_chars(B) end),
  ok.
