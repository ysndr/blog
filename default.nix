let
  # Look here for information about how to generate `nixpkgs-version.json`.
  #  → https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  pinnedVersion = pin: builtins.fromJSON (builtins.readFile pin);
  pinnedPkgs = pin: let pin' = (pinnedVersion pin); in 
    import (builtins.fetchTarball {
      inherit (pin') url sha256;
      name = "nixpkgs-${pin'.date}";
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
      hakyll-sass-new = self.callCabal2nix "hakyll-sass" (builtins.fetchTarball {
        url = "http://hackage.haskell.org/package/hakyll-sass-0.2.4/hakyll-sass-0.2.4.tar.gz";
      }) {};
  });
  haskell-env = (haskellPackages'.ghcWithHoogle (hp: with hp; [
    text
    blaze-html
    stack
    hakyll
    hakyll-sass-new
    cabal-install
  ]));


  # ------------ dist ---------------
  thirdparty = linkFarm "thirdparty" [
    {
      name = "bulma";
      path = (fetchTarball "https://github.com/jgthms/bulma/archive/0.7.4.tar.gz") + "/sass";
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
