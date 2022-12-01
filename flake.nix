{ inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-22.05;

    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlay = pkgsNew: pkgsOld: {
          nix-diff =
            pkgsNew.haskell.lib.justStaticExecutables
              pkgsNew.haskellPackages.nix-diff;

          haskellPackages = pkgsOld.haskellPackages.override (old: {
            overrides = pkgsNew.haskell.lib.packageSourceOverrides {
              nix-diff = ./.;
            };
          });
        };

        pkgs =
          import nixpkgs { inherit config system; overlays = [ overlay ]; };

      in
        rec {
          packages.default = pkgs.haskellPackages.nix-diff;

          apps.default = {
            type = "app";

            program = "${pkgs.nix-diff}/bin/nix-diff";
          };

          devShells.default = pkgs.haskellPackages.nix-diff.env;
        }
    );

  nixConfig = {
    extra-substituters = [ "https://cache.garnix.io" ];

    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };
}
