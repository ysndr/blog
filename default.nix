let
  # Look here for information about how to generate `nixpkgs-version.json`.
  #  → https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  pinnedVersion = builtins.fromJSON (builtins.readFile ./.nixpkgs-version.json);
  hies-pkgs = import (builtins.fetchTarball {
    url = "https://github.com/domenkozar/hie-nix/tarball/master";
  });
  pinnedPkgs = import (builtins.fetchGit {
    inherit (pinnedVersion) url rev;

    ref = "nixos-unstable";
  }) {};
  pkgs' = pinned: (if pinned then pinnedPkgs else import <nixpkgs> {});
in

{ pkgs ? pkgs' pinned, pinned ? false, enable-hie ? false }:
with pkgs;
let

  hie = (hies-pkgs { }).hie86;

  haskell-env = (haskellPackages.ghcWithPackages (ps: with ps; [
    hakyll
    hakyll-sass
    stack
    hie
  ]));


  generator = callPackage ./generator {
    stdenv=pkgs.stdenv; 
    inherit haskell-env;
  };

  # --------------- Commands ----------------


in {
  inherit haskell-env hie generator pkgs;
} 
