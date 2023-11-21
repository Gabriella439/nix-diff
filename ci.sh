#!/usr/bin/env bash
# Script by @fisx

set -eo pipefail
cd "$( dirname "${BASH_SOURCE[0]}" )"

echo "regenerating .github/workflows/ci.yaml"

which dhall-to-yaml || cabal install dhall-yaml
dhall-to-yaml-ng --generated-comment --file ci.dhall > .github/workflows/ci.yaml
