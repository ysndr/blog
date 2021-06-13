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
        # hakyll = appendPatch (dontCheck (super.callHackage "hakyll" "4.13.4.1" {})) ./hakyll.patch;


        # hakyll-images = unmarkBroken super.hakyll-images;
        JuicyPixels-extra = super.callHackage "JuicyPixels-extra" "0.4.1" {};
        # pandoc = super.callHackage "pandoc" "2.14.0.1" {};

        # hakyll-sass = unmarkBroken super.hakyll-sass;
      }
    );

    # --------------- JS --------------
    css-tools = (import ./css-tools { inherit pkgs; });

    # ------------ dist ---------------
    thirdparty' = linkFarm "thirdparty" thirdparty;

    # ------------- generator -----------
    generator = (haskellPackages'.callCabal2nix "Site" "${./generator}" {});

    generator-with-thirdparty =
      symlinkJoin {
        name = "generator-with-thirdparty";
        paths = [generator css-tools.shell.nodeDependencies css-tools.package ];
        buildInputs = [ makeWrapper ];
        postBuild = ''
            wrapProgram $out/bin/generator \
              --set THIRDPARTY "${thirdparty'}" \
              --set POSTCSS_MODULES "${css-tools.shell.nodeDependencies}/lib/node_modules" \
              --prefix NODE_PATH : ${css-tools.shell.nodeDependencies}/lib/node_modules \
              --prefix PATH : ${css-tools.shell.nodeDependencies}/bin
          '';
      };

    # --------------- Commands ----------------

    generate-website = script {
      name = "generate-website";
      paths = [ generator-with-thirdparty git css-tools.shell.nodeDependencies ];

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
        haskell-env
        css-tools.shell.nodeDependencies
      ];

      shellHook = ''

        export THIRDPARTY="${thirdparty'}"
        export HAKYLL_ENV="development"

        export POSTCSS_MODULES="${css-tools.shell.nodeDependencies}/lib/node_modules"
        export NODE_PATH="$POSTCSS_MODULES:$NODE_PATH"
        export NODE_ENV="$HAKYLL_ENV"

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
