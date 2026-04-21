#!/usr/bin/env bash
set -euo pipefail

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
project_dir=$(dirname "$script_dir")
tmp_dir="${project_dir}/tmp/benchmarks"
cache_dir="${tmp_dir}/cache"
plot_script="${script_dir}/plot-benchmark-sumstats.R"

cd "${project_dir}"

rm -rf "${tmp_dir}"
mkdir "${tmp_dir}"
mkdir "${cache_dir}"

echo ">> Running benchmarks"

samples=1000000
iterations=3

for f in ./tests/benchmarks/benchmark_*.R
do
  fname=$(basename ${f})
  echo "-- Running benchmark ${fname}"
  cd $(dirname ${f})

  output_path="${tmp_dir}/${fname}.png"

  Rscript "${fname}" ${samples} ${iterations} "${cache_dir}" "${output_path}"

  cd "${project_dir}"
done
