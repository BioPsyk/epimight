#!/usr/bin/env bash

set -euo pipefail

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
project_dir=$(dirname "$script_dir")

cd "${project_dir}"

version=$(cat "./VERSION")

echo ">> Publishing docker image ${version}"

echo "-- Building"
nix build .#dockerImage

echo "-- Importing"
image_id=$(docker import ./result | sed 's/^sha256://')

echo "-- Tagging"
docker image tag "${image_id}" "biopsyk/epimight:${version}"
docker image tag "${image_id}" "biopsyk/epimight:latest"

echo "-- Pushing"
docker image push "biopsyk/epimight:${version}"
docker image push "biopsyk/epimight:latest"
