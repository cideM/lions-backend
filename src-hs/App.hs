{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module App (Env (..), App, unApp) where

import Capability.Reader (Field (..), HasReader, MonadReader (..), Rename (..))
import Capability.Source (HasSource)
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..))
import qualified Database.SQLite.Simple as SQLite
import GHC.Generics
import Session.Domain (VaultKey)
import qualified System.Log.FastLogger as Log
import qualified Web.ClientSession as ClientSession

data Env = Env
  { envDbConn :: SQLite.Connection,
    envSessionKey :: ClientSession.Key,
    envLogger :: Log.FastLogger,
    envVaultKey :: VaultKey
  }
  deriving (Generic)

newtype App a = App {unApp :: Env -> IO a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch
    )
    via ReaderT Env IO
  deriving
    (HasReader "dbConn" SQLite.Connection, HasSource "dbConn" SQLite.Connection)
    via Rename "envDbConn" (Field "envDbConn" "env" (MonadReader (ReaderT Env IO)))
  deriving
    (HasReader "vaultKey" VaultKey, HasSource "vaultKey" VaultKey)
    via Rename "envVaultKey" (Field "envVaultKey" "env" (MonadReader (ReaderT Env IO)))
  deriving
    (HasReader "logger" Log.FastLogger, HasSource "logger" Log.FastLogger)
    via Rename "envLogger" (Field "envLogger" "env" (MonadReader (ReaderT Env IO)))
  deriving
    (HasReader "sessionKey" ClientSession.Key, HasSource "sessionKey" ClientSession.Key)
    via Rename "envSessionKey" (Field "envSessionKey" "env" (MonadReader (ReaderT Env IO)))
