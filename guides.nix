{ src, version, wrappedEmacs, wrappedTexlive, wrappedR, stdenv, plantuml, jre }:

stdenv.mkDerivation rec {
  inherit version;
  inherit src;

  pname  = "epimight-guides";
  phases = "installPhase";

  buildInputs = [
    plantuml jre wrappedEmacs wrappedTexlive wrappedR
  ];

  PLANTUML_PATH = plantuml;

  installPhase = ''
    mkdir -p $out/guides/{cumulative-incidence,genetic-correlation,heritability,pipeline}

    cp $src/* ./ -R
    chmod +w ./guides/ -R

    rm ./guides/diagrams/*.png

    for d in ./guides/*; do
      if [ ! -d "$d" ]; then
        continue
      fi

      dir_name=$(basename $d)
      echo ">> Building $dir_name"
      pushd "$d"
      rm -f *.R

      export PLANTUML_LIMIT_SIZE=8192

      for f in ./*.org; do
        emacs "$f" --batch --kill -l ${./scripts/init.el} -f org-html-export-to-html
        emacs "$f" --batch --kill -l ${./scripts/init.el} -f org-latex-export-to-pdf
        emacs "$f" --batch --kill -l ${./scripts/init.el} -f org-babel-tangle

        mkdir -p "$out/guides/$dir_name"

        cp *.html "$out/guides/$dir_name/"
        cp *.pdf "$out/guides/$dir_name/"
        cp *.R "$out/guides/$dir_name/"
      done

      popd
    done

    cp ./guides/diagrams $out/guides/ -R
    cp ./guides/data $out/guides/ -R
  '';
}
