{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # must match golden-tests/derivations/complex/new/flake.lock
    nixpkgs-golden.url = "github:NixOS/nixpkgs/5ca8e2e9e1fa5e66a749b39261ad6bd0e07bc87f";

    utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ nixpkgs, utils, ... }:
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
              pkgsNew.haskell.lib.compose.overrideCabal
                (old: {
                  testToolDepends = old.testToolDepends or [] ++ [
                    pkgsNew.nix
                  ];
                  # For testing, we use a relocated Nix store, because the
                  # real /nix/store in the sandbox is only accessible as files;
                  # not a store objects (https://nixos.org/manual/nix/stable/glossary.html#gloss-store-object)
                  # A relocated store lets us use /nix/store as the logical
                  # storeDir, while using an arbitrary directory for storage.
                  preCheck = ''
                    ${old.preCheck or ""}
                    TESTROOT="$(mktemp --tmpdir -d test-state-XXXXXX)"
                    export NIX_BUILD_HOOK=
                    export NIX_CONF_DIR=$TEST_ROOT/etc
                    export NIX_LOCALSTATE_DIR=$TEST_ROOT/var
                    export NIX_LOG_DIR=$TEST_ROOT/var/log/nix
                    export NIX_STATE_DIR=$TEST_ROOT/var/nix
                    export NIX_REMOTE="$TESTROOT/store"
                    export HOME="$TESTROOT/home"
                    mkdir -p $HOME
                    echo "Copying pinned Nixpkgs for golden tests to relocated store $NIX_REMOTE"
                    nix eval --expr 'builtins.path { path = ${inputs.nixpkgs-golden}; name = "source"; }' \
                      --impure --extra-experimental-features nix-command --offline
                  '';
                })
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
}
