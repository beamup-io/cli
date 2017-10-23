#!/bin/bash

set -e

# Let Erlang know where to find modules
cp /host/lib/.erlang ~/.erlang

# Fetch CLI command scripts from the mounted volume
mkdir -p /beamup/commands/
cp -R /host/commands/* /beamup/commands/

# Fetch and compile modules
mkdir -p /beamup/src/
cp -R /host/src/* /beamup/src/
erlc -o /beamup/src/ /beamup/src/*.erl

if [[ "$@" = "selftest" ]]; then
  exec $@
else
  echo "Syncing working tree to scratch location"
  mkdir -p /beamup/project/${PROJECT_NAME}
  rsync -r /host/project/${PROJECT_NAME}/ /beamup/project/${PROJECT_NAME}/

  cd /beamup/project/${PROJECT_NAME}
fi
