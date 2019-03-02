# Lucas' Yale website v. 2017-03-02

# Some code copied and/or modified from
# https://utdemir.com/posts/hakyll-on-nixos.html
{ stdenv, haskell-env }:

stdenv.mkDerivation {
  name = "blog-generator";
  src = ./.;
  phases = "unpackPhase buildPhase";
  buildInputs = [
    haskell-env
  ];
  buildPhase = ''
    mkdir -p $out/bin
    ghc -O2 -dynamic --make site.hs -o $out/bin/generate-site
  '';
}
