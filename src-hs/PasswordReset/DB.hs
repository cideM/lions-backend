{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PasswordReset.DB
  ( getTokenForUser,
    updatePassword,
    insertToken,
    getTokenByValue,
    deleteToken,
  )
where

import Control.Exception.Safe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow (FromRow)
import PasswordReset.Domain (Hashed, Token (..), TokenCreate (..), TokenId (..), unhash)
import User.Domain (UserId (..))
import Prelude hiding (id)

newtype DBToken = DBToken Token deriving (Show)

instance FromRow DBToken where
  fromRow =
    DBToken
      <$> ( Token <$> SQLite.field
              <*> SQLite.field
              <*> (TokenId <$> SQLite.field)
              <*> (UserId <$> SQLite.field)
          )

updatePassword :: SQLite.Connection -> Hashed -> UserId -> IO ()
updatePassword conn hashed (UserId userid) = do
  let newPw = unhash hashed
  SQLite.execute conn "update users set password_digest = ? where id = ?" (newPw, show userid)

getTokenByValue :: SQLite.Connection -> Text -> IO (Maybe Token)
getTokenByValue conn t =
  handleAny (\e -> throwString $ "error getting users: " <> show e) $
    (SQLite.query conn "select token, expires, id, userid from reset_tokens where token = ?" [t])
      >>= \case
        [] -> return Nothing
        [(DBToken token) :: DBToken] -> return $ Just token
        _ -> throwString . Text.unpack $ "returned more than one token for value: " <> t

getTokenForUser :: SQLite.Connection -> UserId -> IO (Maybe Token)
getTokenForUser conn (UserId userid) =
  (SQLite.query conn "select token, expires, id, userid from reset_tokens where userid = ?" [userid])
    >>= \case
      [] -> return Nothing
      [(DBToken token) :: DBToken] -> return $ Just token
      s -> throwString $ "returned more than one token for user: '" <> show userid <> "', got: '" <> show s <> "'"

-- Run this and generate token in transaction
deleteToken :: SQLite.Connection -> UserId -> IO ()
deleteToken conn (UserId userid) = SQLite.execute conn "delete from reset_tokens where userid = ?" $ SQLite.Only userid

insertToken :: SQLite.Connection -> TokenCreate -> IO ()
insertToken conn TokenCreate {tokenCreateUserId = (UserId userid), ..} =
  SQLite.execute
    conn
    "insert into reset_tokens (token, expires, userid) values (?,?,?)"
    (tokenCreateValue, tokenCreateExpires, userid)
