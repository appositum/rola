{ lib, mkDerivation, base, containers, hspec, megaparsec, stdenv }:
mkDerivation {
  pname = "rola";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers megaparsec ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base containers hspec megaparsec ];
  doHaddock = false;
  homepage = "https://github.com/appositum/rola#readme";
  license = lib.licenses.asl20;
}
