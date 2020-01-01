{ mkDerivation, base, filepath, process, hakyll, hakyll-sass, hsass, stdenv, text, time, pandoc, hakyll-images}:
mkDerivation {
  pname = "Site";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base filepath process time hakyll hakyll-sass hsass text pandoc hakyll-images];
  license = stdenv.lib.licenses.mit;
}
