{ mkDerivation, attoparsec, base, containers, criterion, deepseq
, filepath, pretty-show, QuickCheck, stdenv, text, vector, lib
}:
mkDerivation {
  pname = "nix-derivation";
  version = "1.1.3";
  sha256 = "11drhg3zjhwbvdw25k0icvbkcpqilx0m9qw60k7snfaz1iadfkdb";
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
  license = lib.licenses.bsd3;
}
