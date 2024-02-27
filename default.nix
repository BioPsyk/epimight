{ rPackages, version, src }:

with rPackages; buildRPackage rec {
  name = "ibp-risk-estimations";

  inherit version;
  inherit src;

  propagatedBuildInputs = [
    dplyr dtplyr data_table cmprsk ggplot2 stringr readr tidyr rlang
    jinjar yaml rjson
  ];
}
