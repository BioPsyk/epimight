{
  description = "epimight";

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

    wrappedTexlive = with pkgs; texlive.combine {
      inherit (texlive)
      scheme-tetex wrapfig ulem capt-of parskip titlesec
      footmisc listings cm-super sectsty framed libertine tcolorbox environ
      trimspaces background everypage datetime fmtcount titling tabulary
      listingsutf8;
    };

    wrappedEmacs = with pkgs; (emacsPackagesFor emacs).emacsWithPackages (epkgs: with epkgs; [
      org
      htmlize
      use-package
      ess
    ]);

    wrappedR = with pkgs; rWrapper.override {
      packages = with rPackages; [
        # Development
        devtools testthat lintr knitr rmarkdown pkgdown box microbenchmark progress
        waldo
        # Requirements
        dplyr dtplyr data_table cmprsk ggplot2 stringr readr tidyr rlang
        jinjar yaml rjson
      ];
    };
  in
  {
    devShell = import ./shell.nix {
      inherit pkgs;
      inherit wrappedEmacs;
      inherit wrappedTexlive;
      inherit wrappedR;
    };
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
      guides = pkgs.callPackage ./guides.nix {
        inherit (pkgs);
        inherit src;
        inherit version;
        inherit wrappedEmacs;
        inherit wrappedTexlive;
        inherit wrappedR;
      };
      singularityImage = pkgs.singularity-tools.buildImage {
        name      = "epimigh t";
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
        inherit singularityImage;
        inherit guides;
        inherit version;
      };
    };
  });
}
