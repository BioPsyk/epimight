{ version, singularityImage, guides, stdenv, lib }:

let
  sifName    = "epimight-${version}.sif";
  entrypoint = builtins.toFile "epimight" ''
    #!/usr/bin/env bash

    set -euo pipefail

    script_dir="$( cd "$( dirname "''${BASH_SOURCE[0]}" )" && pwd )"

    sif="''${script_dir}/${sifName}"

    exec singularity exec --contain --cleanenv --home "$(pwd)" $sif "$@"
  '';
in
stdenv.mkDerivation rec {
  inherit version;

  pname  = "epimight-standalone";
  phases = "installPhase";

  installPhase = ''
    mkdir -p "$out"

    cp "${singularityImage}" "$out/${sifName}"
    cp "${guides}/guides" "$out/guides" -R
    cp "${entrypoint}" "$out/epimight"

    chmod +x "$out/epimight"
  '';
}
