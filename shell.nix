{ pkgs, wrappedEmacs, wrappedTexlive, wrappedR }:

with pkgs; mkShell {
  buildInputs = [
    postgresql_13
    python3
    python3Packages.watchdog
    python3Packages.psycopg2
    python3Packages.jinja2
    python3Packages.behave
    python3Packages.tabulate
    python3Packages.numpy
    wrappedEmacs
    wrappedTexlive
    wrappedR
  ];
}
