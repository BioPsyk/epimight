{ pkgs ? import <nixpkgs> { } }:

with pkgs; mkShell {
  buildInputs = [
    postgresql_13
    python3
    python3Packages.watchdog
    python3Packages.psycopg2
    python3Packages.jinja2
    python3Packages.behave
    python3Packages.tabulate
    R
  ] ++
  (with rPackages; [
    # Development
    devtools testthat lintr knitr rmarkdown pkgdown box microbenchmark progress
    waldo
    # Requirements
    dplyr dtplyr data_table cmprsk ggplot2 stringr readr tidyr rlang
    jinjar yaml rjson
  ]);
}
