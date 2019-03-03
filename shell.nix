# This allows overriding pkgs by passing `--arg pkgs ...`
{ pkgs ? import <nixpkgs> {}, pinned ? null, enable-hie ? false }:
let
  project = import ./default.nix 
      { inherit pinned; } //
      (if (isNull pinned) then { inherit pkgs; } else {});
in with project.pkgs;

mkShell {
  buildInputs = [
    # put packages here.
    project.generator
    project.haskell-env
    (lib.optional (enable-hie) project.hie)
  ];

   shellHook = ''
    export HIE_HOOGLE_DATABASE="${project.haskell-env}/share/doc/hoogle/index.html";
    export NIX_GHC="${project.haskell-env}/bin/ghc"
    export NIX_GHCPKG="${project.haskell-env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${project.haskell-env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
