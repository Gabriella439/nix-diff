{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    let
      config = { };

      overlay = pkgsNew: pkgsOld: {
        nix-diff =
          pkgsNew.haskell.lib.justStaticExecutables
            pkgsNew.haskellPackages.nix-diff;

        haskellPackages = pkgsOld.haskellPackages.override (old: {
          overrides = self: super: {
            nix-diff =
              # There are quick check and golden tests.
              # Quick check tests require random source to work.
              # Golden tests require write access to the /nix/var
              # and read access to the test data into /nix/store
              # So we can't run these tests in build time
              pkgsNew.haskell.lib.dontCheck
                (self.callCabal2nix "nix-diff" ./. { });
          };
        });
      };
    in
    {
      overlays.default = overlay;
    } //
    (utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit config system; overlays = [ overlay ]; };
      in
      {
        packages.default = pkgs.haskellPackages.nix-diff;

        apps.default = {
          type = "app";

          program = "${pkgs.nix-diff}/bin/nix-diff";
        };

        devShells.default = pkgs.haskellPackages.nix-diff.env;
      }
    ));

  nixConfig = {
    extra-substituters = [ "https://cache.garnix.io" ];

    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };
}
