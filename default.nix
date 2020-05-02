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
let
  config = { };

  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        let
          sourceOverrides = pkgsNew.haskell.lib.packageSourceOverrides {
            "nix-diff" = ./.;
          };

          fromDirectory = pkgsNew.haskell.lib.packagesFromDirectory {
            directory = ./nix;
          };

          default = old.overrides or (_: _: {});

        in
          pkgsNew.lib.fold pkgsNew.lib.composeExtensions default [
            sourceOverrides
            fromDirectory
          ];

    });
  };

  pkgs =
    import <nixpkgs> { inherit config; overlays = [ overlay ]; };

in
  { nix-diff = pkgs.haskellPackages.nix-diff;
  }
