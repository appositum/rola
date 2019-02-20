{ mkDerivation, base, containers, hspec, megaparsec, stdenv }:
mkDerivation {
  pname = "rola";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers megaparsec ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base containers hspec megaparsec ];
  doHaddock = false;
  homepage = "https://github.com/appositum/rola#readme";
  license = stdenv.lib.licenses.asl20;
}
