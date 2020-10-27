let
  all-hies = import (builtins.fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};

in
{ pkgs ? import <nixpkgs> {}
, thirdparty ? []
, nur ? import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
    pkgs = pkgs;
  }
}:
  with pkgs;
  let
    # -------------- Utils -------------
    script = { ... } @ args: nur.repos.ysndr.lib.wrap (
      {
        shell = true;
      } // args
    );

    # ------------- Haskell ------------
    # hie = all-hies.selection { selector = p: { inherit (p) ghc865; }; };
    haskellPackages' = haskellPackages.extend (
      self: super: with pkgs.haskell.lib;  {
        hakyll = appendPatch (dontCheck (super.callHackage "hakyll" "4.13.4.1" {})) ./hakyll.patch;
        hakyll-images = unmarkBroken super.hakyll-images;
        hakyll-sass = unmarkBroken super.hakyll-sass;
      }
    );

    # ------------ dist ---------------
    thirdparty' = linkFarm "thirdparty" thirdparty;

    # ------------- generator -----------
    generator = haskellPackages'.callCabal2nix "Site" "${./generator}" {};

    generator-with-thirdparty =
      generator.overrideAttrs (
        old: {
          nativeBuildInputs = old.nativeBuildInputs or [] ++ [ makeWrapper ];
          installPhase = old.installPhase + "\n" + ''
            wrapProgram $out/bin/generator --set  THIRDPARTY ${thirdparty'}
          '';
        }
      );

    # --------------- Commands ----------------

    generate-website = script {
      name = "generate-website";
      paths = [ generator-with-thirdparty git ];

      script = ''
        generator rebuild
      '';
    };

    # ---------------- Shell ------------------
    haskell-env = haskellPackages'.ghcWithHoogle (
      hp: with hp; [ haskell-language-server cabal-install ]
      ++ generator.buildInputs
    );

    shell = mkShell {
      name = "blog-env";
      buildInputs = [
        haskell-env generator
      ];

      shellHook = ''
        export THIRDPARTY="${thirdparty'}"
        export HAKYLL_ENV="development"

        export HIE_HOOGLE_DATABASE="${haskell-env}/share/doc/hoogle/default.hoo"
        export NIX_GHC="${haskell-env}/bin/ghc"
        export NIX_GHCPKG="${haskell-env}/bin/ghc-pkg"
        export NIX_GHC_DOCDIR="${haskell-env}/share/doc/ghc/html"
        export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
      '';
    };
  in
    {
      inherit shell generator generator-with-thirdparty generate-website;
      ci = {
        compile = generate-website;
      };
    }
