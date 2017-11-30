#!/bin/bash

is_container () {
  cgroup_output=$(cat /proc/self/cgroup 2>&1)
  echo "$cgroup_output" | grep -E '/docker/|/lxc/' > /dev/null
  return $?
}
