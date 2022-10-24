{ version, singularityImage, docs, stdenv, lib }:

let
  sifName    = "ibp-risk-estimations-${version}.sif";
  entrypoint = builtins.toFile "ibp-risk-estimations" ''
    #!/usr/bin/env bash

    set -euo pipefail

    script_dir="$( cd "$( dirname "''${BASH_SOURCE[0]}" )" && pwd )"

    sif="''${script_dir}/${sifName}"

    exec singularity run --contain --cleanenv --home "$(pwd)" $sif "$@"
  '';
in
stdenv.mkDerivation rec {
  inherit version;

  pname  = "ibp-risk-estimations-standalone";
  phases = "installPhase";

  installPhase = ''
    mkdir -p "$out"

    cp "${singularityImage}" "$out/${sifName}"
    cp "${docs}/docs" "$out/docs" -R
    cp "${entrypoint}" "$out/ibp-risk-estimations"

    chmod +x "$out/ibp-risk-estimations"
  '';
}
