{
  description = "Flake utils demo";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.nur.url = "github:nix-community/NUR";
  inputs.flake-utils.url = "github:numtide/flake-utils";


  inputs.uikit-src = {
    url = "https://github.com/uikit/uikit/archive/v3.5.8.tar.gz";
    flake = false;
  };


  outputs = { self, nixpkgs, nur, flake-utils, uikit-src }:
    let
    in
      flake-utils.lib.eachDefaultSystem (
        system:
          let
            pkgs' = import nixpkgs { inherit system; overlays = [ nur.overlay ]; };
            blog = pkgs'.callPackage ./blog.nix {
              pkgs = pkgs';
              nur = pkgs'.nur;
              thirdparty = [
                {
                  name = "uikit";
                  path = "${uikit-src}/src";
                }
              ];
            };
          in
            {
              packages = { inherit (blog) generator generator-with-thirdparty ci shell; };
              defaultPackage = blog.generator-with-thirdparty;
              apps.generate-website =
                flake-utils.lib.mkApp { drv = blog.ci.generate-website; };
              defaultApp = blog.generate-website;
              devShell = blog.shell;
            }
      );

}
