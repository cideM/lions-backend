#! /usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Turtle

main :: IO ()
main = do
  need "LIONS_SQLITE_PATH" >>= \case
    Nothing -> die "LIONS_SQLITE_PATH missing"
    Just sqlitePath -> do
      sqliteExists <- testfile $ fromText sqlitePath
      unless sqliteExists $ touch $ fromText sqlitePath

      procs "nix" ["build", ".#allAssets", "-o", "public"] empty

      let litestream = proc "litestream" ["replicate", sqlitePath, "s3://lions-achern-litestream-replica-1/dev-db"] empty
          cabal = proc "cabal" ["v2-run", "--cabal-file", "./backend/lions-backend.cabal", "run-lions-backend"] empty

      view $ parallel [litestream, cabal]
