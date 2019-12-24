let
  all-hies = import (builtins.fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};

in
{pkgs ? import (if pin == false then <nixpkgs> else pin) {},
 pin ? ./nixpkgs.nix, ... }:
with pkgs;
let

  # -------------- Utils -------------
  nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      pkgs=pkgs;
  };
  script = {...} @ args: nur.repos.ysndr.lib.wrap ({
    shell = true;
  } // args);

  # ------------- Haskell ------------
  hie = all-hies.selection { selector = p: { inherit (p) ghc865; }; };
  haskellPackages' = haskell.packages.ghc865.extend( self: super: {
      hakyll-sass-new = self.callCabal2nix "hakyll-sass" (builtins.fetchTarball {
        url = "http://hackage.haskell.org/package/hakyll-sass-0.2.4/hakyll-sass-0.2.4.tar.gz";
      }) {};
  });

  # ------------ dist ---------------
  thirdparty = linkFarm "thirdparty" [
    {
      name = "bulma";
      path = (fetchTarball "https://github.com/jgthms/bulma/archive/0.7.4.tar.gz") + "/sass";
    }
    {
      name = "uikit";
      path = (fetchTarball "https://github.com/uikit/uikit/archive/v3.2.4.tar.gz") + "/src";
    }
  ];

  # ------------- generator -----------
  generator = haskell.lib.justStaticExecutables (haskellPackages'.callPackage ./generator {});
  
  generator-with-thirdparty = generator.overrideAttrs(old: {
    nativeBuildInputs = old.nativeBuildInputs or [] ++ [makeWrapper];
    installPhase = old.installPhase + "\n" + ''
      wrapProgram $out/bin/generator --set THIRDPARTY ${thirdparty}
    '';
  });

  # --------------- Commands ----------------

  generate-website = script {
    name = "generate-website"; 
    paths = [generator-with-thirdparty git];

    script = ''
      generator rebuild
    '';
  };

  # ---------------- Shell ------------------
  haskell-env = haskellPackages'.ghcWithHoogle (
    hp: with hp; [ cabal-install ] 
    ++ generator.buildInputs );
  
  shell = { enable-hie ? false }: mkShell {
    name = "blog-env";
    buildInputs = [
      # put packages here.
      generator
      haskell-env
      (lib.optional (enable-hie) hie)
    ];

    shellHook = ''
      export THIRDPARTY="${thirdparty}"

      export HIE_HOOGLE_DATABASE="${haskell-env}/share/doc/hoogle/default.hoo"
      export NIX_GHC="${haskell-env}/bin/ghc"
      export NIX_GHCPKG="${haskell-env}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${haskell-env}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    '';
  };
in {
  inherit shell website generator generate-website ;
  ci = {
    compile = generate-website;
  };
} 
