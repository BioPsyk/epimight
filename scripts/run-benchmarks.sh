#!/usr/bin/env bash
set -euo pipefail

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
project_dir=$(dirname "$script_dir")
tmp_dir="${project_dir}/tmp/benchmarks"
plot_script="${script_dir}/plot-benchmark-sumstats.R"

cd "${project_dir}"

rm -rf "${tmp_dir}"
mkdir "${tmp_dir}"

echo ">> Running benchmarks"

for f in ./quality-assurance/benchmarks/benchmark_*.R
do
  fname=$(basename ${f})
  echo "-- Running benchmark ${fname}"
  cd $(dirname ${f})

  output_path="${tmp_dir}/${fname}.csv"
  echo "expr,min,lq,mean,median,uq,max,neval,n" > "${output_path}"
  samples=(1000 10000)
  for s in ${samples[@]}
  do
    Rscript "${fname}" "${s}" 1 "${output_path}"
  done

  Rscript "${plot_script}" "${output_path}" "${fname}"

  cd "${project_dir}"
done
