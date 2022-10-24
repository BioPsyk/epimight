{
  description = "ibp-risk-estimations";

  nixConfig.bash-prompt = "\[dev\]$ ";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs    = nixpkgs.legacyPackages."${system}";
    version = pkgs.lib.removeSuffix "\n" (builtins.readFile ./VERSION);
  in
  {
    devShell = import ./shell.nix { inherit pkgs; };
    packages = rec {
      src = pkgs.callPackage ./src.nix {
        inherit (pkgs);
        inherit version;
      };
      default = pkgs.callPackage ./default.nix {
        inherit (pkgs);
        inherit src;
        inherit version;
      };
      wrappedR = with pkgs; rWrapper.override {
        packages = with rPackages; [
          dplyr dtplyr data_table cmprsk ggplot2 stringr readr
          tidyr rlang devtools testthat lintr microbenchmark
          default
        ];
      };
      docs = pkgs.callPackage ./docs.nix {
        inherit (pkgs);
        inherit src;
        inherit version;
        inherit wrappedR;
      };
      singularityImage = pkgs.singularity-tools.buildImage {
        name      = "ibp-risk-estimations";
        contents  = [ pkgs.coreutils wrappedR ];
        diskSize  = 10 * 1024;
        memSize   = 2048;
        runScript = "${wrappedR}/bin/Rscript $@";
        runAsRoot = with pkgs; ''
          #!${stdenv.shell}
          ${dockerTools.shadowSetup}
        '';
      };
      standalone = pkgs.callPackage ./standalone.nix {
        inherit (pkgs);
        inherit docs;
        inherit singularityImage;
        inherit version;
      };
    };
  });
}
