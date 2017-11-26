#!/bin/bash

set -e

# Let Erlang know where to find modules
cp /host/builder/priv/.erlang ~/.erlang

# Fetch CLI command scripts from the mounted volume
mkdir -p /beamup/builder/commands/
cp -R /host/builder/commands/* /beamup/builder/commands/

# Fetch and compile modules
mkdir -p /beamup/builder/src/
cp -R /host/builder/src/* /beamup/builder/src/
erlc -o /beamup/builder/src/ /beamup/builder/src/*.erl

# Compile builder
cp /host/builder/rebar.config /beamup/builder/rebar.config
cd /beamup/builder
rebar3 compile

# Write paths of compiled dependencies
# to be read and loaded by ~/.erlang
rebar3 path > paths

cd /host/project/${BEAMUP_PROJECT_NAME}

# Enable shell history in OTP 20+
export ERL_AFLAGS="-kernel shell_history enabled"

# The CLI commands are executable escripts
export PATH="/beamup/builder/commands:$PATH"

exec $@
