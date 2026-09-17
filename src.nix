{ stdenv, lib, version }:

stdenv.mkDerivation {
  inherit version;

  pname  = "epimight-src";
  phases = "installPhase";

  installPhase = ''
    mkdir -p $out

    cp ${./DESCRIPTION} $out/DESCRIPTION -R
    cp ${./NAMESPACE} $out/NAMESPACE -R
    cp ${./NEWS.md} $out/NEWS.md -R
    cp ${./README.md} $out/README.md
    cp ${./R} $out/R -R
    cp ${./VERSION} $out/VERSION -R
    cp ${./guides} $out/guides -R
    cp ${./inst} $out/inst -R
    cp ${./tests} $out/tests -R
  '';
}
