#!/usr/bin/env bash
set -euo pipefail

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
project_dir=$(dirname "$script_dir")

cd "${project_dir}"

echo ">> Building guides"

out="${project_dir}/tmp/docs"
config="${project_dir}/scripts/init.el"

rm -rf "${out}"
mkdir "${out}"

for d in "${project_dir}/guides/"*; do
  if [ ! -d "$d" ]; then
    continue
  fi

  dir_name=$(basename $d)
  echo ">> Building $dir_name"
  pushd "$d"
  rm -f *.R

  export PLANTUML_LIMIT_SIZE=8192

  for f in ./*.org; do
    emacs "$f" --batch --kill -l "${config}" -f org-html-export-to-html
    emacs "$f" --batch --kill -l "${config}" -f org-latex-export-to-pdf
    emacs "$f" --batch --kill -l "${config}" -f org-babel-tangle
  done

  popd
done
