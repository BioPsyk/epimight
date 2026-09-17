#!/usr/bin/env bash

set -euo pipefail

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
project_dir=$(dirname "$script_dir")

cd "${project_dir}"

version=$(cat "./VERSION")

echo ">> Publishing docker image ${version}"

echo "-- Building"
nix build .#dockerImage

echo "-- Loading image"
docker load < ./result

echo "-- Tagging latest"
docker image tag "biopsyk/epimight:${version}" "biopsyk/epimight:latest"

echo "-- Pushing"
docker image push "biopsyk/epimight:${version}"
docker image push "biopsyk/epimight:latest"
