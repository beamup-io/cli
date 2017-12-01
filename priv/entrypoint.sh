#!/bin/bash

set -e

cli_dir=$(readlink -f "$(dirname "$(readlink -f "$0")")/../")
beamup_dir=$(readlink -f "$cli_dir/../")

source $cli_dir/priv/is_container.sh
if ! is_container; then
  echo "This script is NOT meant to be invoked directly on your machine,"
  echo "and should only be called inside an ephemeral container, either"
  echo "on your CI server with `beamup [build]`, or inside your CI container."
  exit 1
fi;

# Elixir needs UTF-8 locales
export LC_ALL=C.UTF-8
export LANG=C.UTF-8

# Enable shell history in OTP 20+
export ERL_AFLAGS="-kernel shell_history enabled"

# The CLI commands are executable escripts
export PATH="$cli_dir/commands:$PATH"

# Add Elixir executables (iex, elixirc...)
export PATH="$beamup_dir/elixir/bin:$PATH"

# Set global gitignore
if [ -n $GLOBAL_GITIGNORE ]; then
  echo "$GLOBAL_GITIGNORE" > ~/.beamup_gitignore
  git config --global core.excludesfile ~/.beamup_gitignore
fi

# Let Erlang know where to find modules
cp "$cli_dir/priv/.erlang" ~/.erlang

# Compile builder
tmp_cli_dir="/tmp/beamup/cli"
mkdir -p "$tmp_cli_dir"
cp -r "$cli_dir/." "$tmp_cli_dir"
cd "$tmp_cli_dir"

rebar3 compile

# Write paths of compiled dependencies
# to be read and loaded by ~/.erlang
rebar3 path > "$tmp_cli_dir/paths"

cd "$BEAMUP_PROJECT_DIR"

# Optionally install Elixir
if [ -n "$BEAMUP_ELIXIR_VERSION" ]; then
  echo "Using Elixir $BEAMUP_ELIXIR_VERSION"
  install_elixir $BEAMUP_ELIXIR_VERSION
fi

exec $@
