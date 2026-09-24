#!/usr/bin/env bash
set -euo pipefail

# cabal-scaffold monorepos keep each package in a direct child directory.
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." >/dev/null && pwd)
STATUS=0
for PACKAGE in "$ROOT"/*/*.cabal; do
  if ! (cd "$(dirname "$PACKAGE")" >/dev/null && cabal check); then
    STATUS=1
  fi
done
exit "$STATUS"
