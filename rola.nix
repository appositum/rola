{ mkDerivation, base, megaparsec_7_0_4, stdenv }:
mkDerivation {
  pname = "rola";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec_7_0_4 ];
  executableHaskellDepends = [ base megaparsec_7_0_4 ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  homepage = "https://github.com/appositum/rola#readme";
  license = stdenv.lib.licenses.asl20;
}
