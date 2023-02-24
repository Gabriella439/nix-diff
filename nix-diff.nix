{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, filepath, generic-arbitrary, lib, mtl, nix-derivation
, optparse-applicative, patience, process, QuickCheck
, quickcheck-instances, tasty, tasty-quickcheck, tasty-silver, text
, typed-process, uniplate, unix, vector
}:
mkDerivation {
  pname = "nix-diff";
  version = "1.0.19";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers directory filepath
    generic-arbitrary mtl nix-derivation optparse-applicative patience
    process QuickCheck quickcheck-instances text uniplate vector
  ];
  executableHaskellDepends = [
    aeson base bytestring containers mtl optparse-applicative text unix
  ];
  testHaskellDepends = [
    aeson base bytestring containers mtl tasty tasty-quickcheck
    tasty-silver text typed-process
  ];
  homepage = "https://github.com/Gabriella439/nix-diff";
  description = "Explain why two Nix derivations differ";
  license = lib.licenses.bsd3;
  mainProgram = "nix-diff";
}
