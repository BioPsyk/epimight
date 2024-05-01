{ wrappedR, src, version, stdenv, plantuml, emacs, emacsPackagesFor, texlive }:

let
  wrappedTexlive = texlive.combine {
    inherit (texlive)
    scheme-tetex wrapfig ulem capt-of parskip titlesec
    footmisc listings cm-super sectsty framed libertine tcolorbox environ
    trimspaces background everypage datetime fmtcount titling tabulary
    listingsutf8;
  };

  emacsP = (emacsPackagesFor emacs).emacsWithPackages (epkgs: with epkgs; [
    org
    htmlize
    use-package
    ess
  ]);

  buildScript = builtins.toFile "build-docs.el" ''
    ;; Turn this off since use-package takes care of enabling packages
    (setq package-enable-at-startup nil)

    ;; Turn this off since NixOS provides all packages
    (setq package-archives nil)

    (eval-when-compile
      (require 'use-package)
      (setq use-package-compute-statistics t))

    (defconst emacs-src-dir (file-name-directory load-file-name)
      "Absolute path to zplatform emacs directory.")

    ;;---- DEFUNS ------------------------------------------------------------------

    (defun resolve-org-file (file)
      "Gets the absolute path of the zplatform org-file FILE."
      (expand-file-name file emacs-src-dir))

    (defun tangle-org-file (file)
      "Loads the org-file FILE that exists inside the zplatform emacs dir."
      (find-file (resolve-org-file file))
      (org-babel-tangle)
      (kill-buffer))

    ;;------------------------------------------------------------------------------

    (require 'org)
    (require 'ob)
    (require 'ob-tangle)

    (use-package htmlize)

    (setq org-plantuml-jar-path
      (concat (getenv "PLANTUML_PATH") "/lib/plantuml.jar"))

    (setq org-src-fontify-natively t)

    (setq org-log-done 'note)
    (setq org-export-html-validation-link nil)
    (setq org-latex-listings t)
    (setq org-src-preserve-indentation t)
    (setq org-confirm-babel-evaluate nil)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((plantuml . t)))
  '';
in
stdenv.mkDerivation rec {
  inherit version;
  inherit src;

  pname = "ibp-risk-estimations-docs";

  phases = "installPhase";

  buildInputs = [
    emacsP plantuml wrappedR wrappedTexlive
  ];

  PLANTUML_PATH = plantuml;

  installPhase = ''
    mkdir -p $out/docs/pipelines

    cp $src/* ./ -R
    chmod +w ./docs/pipelines -R
    rm ./docs/pipelines/*.R

    pushd ./docs/pipelines

    export PLANTUML_LIMIT_SIZE=8192

    for f in ./*.org; do
      emacs "$f" --batch --kill -l ${buildScript} -f org-html-export-to-html
      emacs "$f" --batch --kill -l ${buildScript} -f org-latex-export-to-pdf
      emacs "$f" --batch --kill -l ${buildScript} -f org-babel-tangle

      cp $(basename $f .org).html $out/docs/pipelines/
      cp $(basename $f .org).pdf $out/docs/pipelines/
      cp $(basename $f .org).R $out/docs/pipelines/
    done

    popd

    cp ./docs/diagrams $out/docs/ -R
    cp ./docs/images $out/docs/ -R
  '';
}
