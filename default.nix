{ mkDerivation, attoparsec, base, containers, Diff, mtl
, nix-derivation, optparse-applicative, stdenv, system-filepath
, text, unix, vector
}:
mkDerivation {
  pname = "nix-diff";
  version = "1.0.4";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base containers Diff mtl nix-derivation
    optparse-applicative system-filepath text unix vector
  ];
  homepage = "https://github.com/Gabriel439/nix-diff";
  description = "Explain why two Nix derivations differ";
  license = stdenv.lib.licenses.bsd3;
}
