{ mkDerivation, base, filepath, process, hakyll, hakyll-sass, hsass, stdenv, text, time, pandoc, hakyll-images, blaze-html, blaze-markup}:
mkDerivation {
  pname = "Site";
  version = "0.1.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base filepath process time hakyll hakyll-sass hsass text pandoc hakyll-images blaze-html blaze-markup];
  license = stdenv.lib.licenses.mit;
}
