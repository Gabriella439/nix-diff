{ mkDerivation, attoparsec, base, containers, Diff, mtl
, nix-derivation, optparse-generic, stdenv, system-filepath, text
, unix
}:
mkDerivation {
  pname = "nix-diff";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base containers Diff mtl nix-derivation optparse-generic
    system-filepath text unix
  ];
  homepage = "https://github.com/Gabriel439/nix-diff";
  description = "Explain why two Nix derivations differ";
  license = stdenv.lib.licenses.bsd3;
}
