#!/usr/bin/env bash
set -euo pipefail

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
project_dir=$(dirname "$script_dir")

cd "${project_dir}"

echo ">> Building docs"

R -e "library(devtools); devtools::document(); pkgdown::build_site(new_process = FALSE)"
