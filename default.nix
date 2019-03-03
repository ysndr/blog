let
  # Look here for information about how to generate `nixpkgs-version.json`.
  #  → https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  pinnedVersion = pin: builtins.fromJSON (builtins.readFile pin);
  pinnedPkgs = pin:  import (builtins.fetchTarball {
    inherit (pinnedVersion pin) url sha256;
  }) {};
  pkgs' = pinned: (
    if (!isNull pinned) then pinnedPkgs pinned 
    else import <nixpkgs> {});

  hies-pkgs = import (builtins.fetchTarball {
    url = "https://github.com/domenkozar/hie-nix/tarball/master";
  });
in
{ pkgs ? pkgs' pinned, pinned ? null, enable-hie ? false }:
with pkgs;
let


  # ------------- Haskell ----------------
  # for build usage only
  hies = (hies-pkgs { inherit pkgs; }).hies;

  #
  hie = (hies-pkgs { inherit pkgs; }).hie84;


  haskellPackages' = haskellPackages.extend( self: super: {
    
  });
  haskell-env = (haskellPackages'.ghcWithHoogle (hp: with hp; [
    hakyll
    hakyll-sass
    cabal-install
  ]));


  # ------------ dist ---------------
  thirdparty = linkFarm "thirdparty" [
    {
      name = "sierra";
      path = (fetchTarball "https://github.com/sierra-library/sierra/archive/3.2.0.tar.gz") + "/src";
    }
  ];


  # ------------- generator -----------
  generator = callPackage ./generator {
    inherit (pkgs) stdenv; 
    inherit haskell-env thirdparty;
  };

  # --------------- Commands ----------------


in {
  #inherit (pkgs)  libyaml libiconv;
  inherit haskell-env hie hies generator pkgs;
} 
