{ mkDerivation, attoparsec, base, containers, criterion, deepseq
, filepath, pretty-show, QuickCheck, stdenv, text, vector
}:
mkDerivation {
  pname = "nix-derivation";
  version = "1.1.0";
  sha256 = "1fd69e47cc5d29e6778dd566bccf06705094d4715b63fd7e350a0d2451402127";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base containers deepseq filepath text vector
  ];
  executableHaskellDepends = [ attoparsec base pretty-show text ];
  testHaskellDepends = [
    attoparsec base filepath QuickCheck text vector
  ];
  benchmarkHaskellDepends = [ attoparsec base criterion text ];
  description = "Parse and render *.drv files";
  license = stdenv.lib.licenses.bsd3;
}
