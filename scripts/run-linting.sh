#!/usr/bin/env bash
set -euo pipefail

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
project_dir=$(dirname "$script_dir")

cd "${project_dir}"

echo ">> Linting R"
R -e "library(lintr); lintr::lint_dir(path = './R')"

echo ">> Linting guides"
R -e "library(lintr); lintr::lint_dir(path = './guides')"
