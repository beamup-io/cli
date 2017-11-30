-module(beamup_elixir).

-export([ensure_installed/1]).

ensure_installed(ElixirVersion) ->
  case is_installed(ElixirVersion) of
    true -> ok;
    _ -> install_version(ElixirVersion)
  end.

% Private

is_installed(ElixirVersion) ->
  {_, Output} = beamup_shell:cmd(<<"elixir --version 2>&1">>),
  Needle = string:join(["Elixir", ElixirVersion], " "),
  case string:find(Output, Needle) of
    nomatch -> false;
    _ -> true
  end.

install_version(ElixirVersion) ->
  io:format("Installing Elixir ~p~n", [ElixirVersion]),
  Blob = download(url(ElixirVersion)),
  Dir = "/root/.beamup/elixir",
  io:format("Extracting to ~p~n", [Dir]),
  ok = filelib:ensure_dir(Dir),
  {ok, _} = zip:unzip(Blob, [{cwd, Dir}]),
  BinDir = filename:join([Dir, "bin"]),
  {0, _} = beamup_shell:cmd(<<"chmod +x *">>, [{cd, BinDir}]),
  ok.

url(ElixirVersion) ->
  OtpMajorVersion = erlang:system_info(otp_release),
  string:join(["https://repo.hex.pm/builds/elixir/v",
               ElixirVersion,
               "-otp-",
               OtpMajorVersion,
               ".zip"], "").

download(Url) ->
  io:format("Downloading ~p~n", [Url]),
  application:ensure_all_started(hackney),
  ReqHeaders = [{<<"User-Agent">>, <<"beamup-builder/0.1 hackney/*">>}],
  Options = [{follow_redirect, true},
            {max_redirect, 5}],
  {ok, _Status, _ResHeaders, Client} = hackney:request(get, Url, ReqHeaders, <<>>, Options),
  {ok, Body} = hackney:body(Client),
  Body.
