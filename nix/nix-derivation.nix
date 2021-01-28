{ mkDerivation, attoparsec, base, containers, criterion, deepseq
, filepath, pretty-show, QuickCheck, stdenv, text, vector
}:
mkDerivation {
  pname = "nix-derivation";
  version = "1.1.1";
  sha256 = "2d69ca33daa5f92b3601b263b1360b230ff193e144db815b1b039b0ed8c18fc9";
  revision = "1";
  editedCabalFile = "1ink37s91kbrq8p0sqyi1i90xp2jimyg1cnzy9ydjh3iv21f7pi5";
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
