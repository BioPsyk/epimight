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

image_id=$(docker images -q "biopsyk/epimight:${version}")
git_branch=$(git rev-parse --abbrev-ref HEAD)

if [ "${git_branch}" != "master" ]; then
  version="${version}-${git_branch}"
fi

echo "-- Pushing ${version}"

docker image tag "${image_id}" "biopsyk/epimight:${version}"
docker image push "biopsyk/epimight:${version}"

if [ "${git_branch}" == "master" ]; then
  echo "-- Pushing latest"
  docker image tag "biopsyk/epimight:${version}" "biopsyk/epimight:latest"
  docker image push "biopsyk/epimight:latest"
fi
