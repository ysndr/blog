# This allows overriding pkgs by passing `--arg pkgs ...`
{ pkgs ? import <nixpkgs> {}, pinned ? false, enable-hie ? false }:
let
  project = import ./default.nix { inherit pkgs pinned; };
in with project.pkgs;

mkShell {
  buildInputs = [
    # put packages here.
    project.generator
    project.haskell-env
    (lib.optional (enable-hie) project.hies)
  ];
}
