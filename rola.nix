{ mkDerivation, base, hspec, megaparsec_7_0_4, stdenv }:
mkDerivation {
  pname = "rola";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec_7_0_4 ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec megaparsec_7_0_4 ];
  doHaddock = false;
  homepage = "https://github.com/appositum/rola#readme";
  license = stdenv.lib.licenses.asl20;
}
