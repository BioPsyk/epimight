{ stdenv, lib, version }:

stdenv.mkDerivation {
  inherit version;

  pname  = "ibp-risk-estimations-src";
  phases = "installPhase";

  installPhase = ''
    mkdir -p $out

    cp ${./R} $out/R -R
    cp ${./docs} $out/docs -R
    cp ${./quality-assurance} $out/quality-assurance -R
    cp ${./DESCRIPTION} $out/DESCRIPTION -R
    cp ${./NAMESPACE} $out/NAMESPACE -R
    cp ${./NEWS.md} $out/NEWS.md -R
    cp ${./README.org} $out/README.org
    cp ${./VERSION} $out/VERSION -R
  '';
}
