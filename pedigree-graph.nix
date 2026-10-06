{ fetchFromGitHub, rustPlatform, R, rPackages, cargo, rustc }:

let
  version = "v0.12.2";
  rev     = "ce93d5a5f3152d4d7c620eaf490489f19c9679c1";

  src = fetchFromGitHub {
    owner = "rwaples";
    repo  = "pedigree-graph";

    inherit rev;

    hash = "sha256-I8f6fdbp04b0UTq2nBv8GUNBGob5tGLbJWTKZtaf/Vc=";
  };

  rust_library = rustPlatform.buildRustPackage (finalAttrs: {
    pname = "pedigree-graph-r";

    inherit version;
    inherit src;

    cargoDeps = rustPlatform.importCargoLock {
      lockFile = "${src}/r/src/rust/Cargo.lock";
    };

    nativeBuildInputs = [
      R
    ];

    postPatch = ''
      cd r/src/rust
    '';
  });
in rPackages.buildRPackage {
  name = "pedigreegraph";

  inherit version;

  src = src + "/r";

  propagatedBuildInputs = with rPackages; [
    Matrix
  ];

  preBuild = ''
    rm -rf src/rust

    echo "PKG_LIBS = -L${rust_library}/lib -lpedigreegraph" > src/Makevars
  '';
}
