#!/bin/sh
set -euxo pipefail
scripts/ormolu.sh -c && scripts/cabal-fmt.sh -c && exit 0
