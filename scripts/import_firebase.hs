#! /usr/bin/env runghc
-- Check with:
-- $ ghc -Wall scripts/import_firebase.hs

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Database.SQLite.Simple as SQLite
import GHC.Generics
import Turtle
import Prelude hiding (FilePath)

data FirebaseUser = FirebaseUser
  { email :: Text,
    passwordHash :: Text,
    salt :: Text
  }
  deriving (Generic, Show)

instance ToJSON FirebaseUser where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FirebaseUser

newtype FirebaseUserFile = FirebaseUserFile
  { users :: [FirebaseUser]
  }
  deriving (Generic, Show)

instance ToJSON FirebaseUserFile where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FirebaseUserFile

parser :: Parser (FilePath, Text)
parser =
  (,) <$> argPath "users" "JSON file with exported Firebase user data"
    <*> argText "sqlite" "Path to SQLite DB File"

main :: IO ()
main = do
  (usersFile, sqlitePath) <- options "Import Firebase users into SQLite" parser
  SQLite.withConnection
    (T.unpack sqlitePath)
    ( \conn -> do
        content <- encodeUtf8 <$> readTextFile usersFile
        FirebaseUserFile {..} <- case (decodeStrict content :: Maybe FirebaseUserFile) of
          Nothing -> die "couldn't parse JSON"
          Just v -> return v
        forM_
          users
          ( \FirebaseUser {..} -> do
              print ([i|Inserting user: #{email}|] :: Text)
              SQLite.execute
                conn
                "insert or ignore into users (email,password_digest,salt) values (?,?,?)"
                (email, passwordHash, salt)
          )
    )
