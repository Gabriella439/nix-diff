# You can build this repository using Nix by running:
#
#     $ nix-build
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell
#
# ... and then Nix will supply the correct Haskell development environment for
# you
{ compiler ? null
}:
let
  config = { };

  overlay = pkgsNew: pkgsOld: {
    haskellPackages =
      let
        packageSet =
          if compiler == null
          then pkgsOld.haskellPackages
          else pkgsOld.haskell.packages.${compiler};
      in packageSet.override (old: {
      overrides =
        let
          fromCabal2nix = self: super: {
            nix-diff =
              # see a note in flake.nix
              pkgsNew.haskell.lib.dontCheck
                (self.callCabal2nix "nix-diff" ./. { });
          };

          fromDirectory = pkgsNew.haskell.lib.packagesFromDirectory {
            directory = ./nix;
          };

          default = old.overrides or (_: _: {});

        in
          pkgsNew.lib.fold pkgsNew.lib.composeExtensions default [
            fromCabal2nix
            fromDirectory
          ];

    });
  };

  pkgs =
    import <nixpkgs> { inherit config; overlays = [ overlay ]; };

in
  { nix-diff = pkgs.haskellPackages.nix-diff;
  }
