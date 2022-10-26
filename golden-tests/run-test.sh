#!/usr/bin/env bash

old_drv="$(nix-instantiate ./old-derivation/drv.nix)"
new_drv="$(nix-instantiate ./new-derivation/drv.nix)"
nix-diff $old_drv $new_drv --environment > /tmp/nix-diff-output
diff ./expected-output /tmp/nix-diff-output
code="$?"
rm /tmp/nix-diff-output
exit "$code"
