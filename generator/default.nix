{ mkDerivation, base, filepath, process, hakyll, hakyll-sass, hsass, stdenv, text, time, pandoc, hakyll-images, blaze-html, blaze-markup, data-default}:
mkDerivation {
  pname = "Site";
  version = "0.1.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base filepath process time hakyll hakyll-sass hsass text pandoc hakyll-images blaze-html blaze-markup data-default];
  license = stdenv.lib.licenses.mit;
}
