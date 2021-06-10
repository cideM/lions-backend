#! /usr/bin/env bash
[[ -d public ]] || mkdir public
[[ -f "$LIONS_SQLITE_PATH" ]] || touch "$LIONS_SQLITE_PATH"
nix build .#allAssets -o public
parallel --ungroup ::: "litestream replicate $LIONS_SQLITE_PATH s3://lions-achern-litestream-replica-1/dev-db" "cabal v2-run run-lions-backend"
