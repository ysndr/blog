{ mkDerivation, base, hakyll, hakyll-sass, hsass, stdenv, text }:
mkDerivation {
  pname = "Site";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base filepath process time hakyll hakyll-sass hsass text ];
  license = stdenv.lib.licenses.mit;
}
