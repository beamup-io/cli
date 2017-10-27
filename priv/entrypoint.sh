#!/bin/bash

set -e

# Let Erlang know where to find modules
cp /host/priv/.erlang ~/.erlang

# Fetch CLI command scripts from the mounted volume
mkdir -p /beamup/commands/
cp -R /host/commands/* /beamup/commands/

# Fetch and compile modules
mkdir -p /beamup/src/
cp -R /host/src/* /beamup/src/
erlc -o /beamup/src/ /beamup/src/*.erl

if [[ ! "$@" = "selftest" ]]; then
  echo "Syncing working tree to scratch location"
  mkdir -p /beamup/project
  cp -R /host/project/${PROJECT_NAME} /beamup/project/

  cd /beamup/project/${PROJECT_NAME}
fi

# Enable shell history in OTP 20+
export ERL_AFLAGS="-kernel shell_history enabled"

# The CLI commands are executable escripts
export PATH="/beamup/commands:$PATH"

exec $@
