{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module App (Env (..), LogEnv (..), App (..), Environment(..), parseEnv) where

import Capability.Reader (Field (..), local, ask, HasReader, MonadReader (..), Rename (..))
import Capability.Source (HasSource)
import Control.Exception.Safe
import RequestID.Middleware (RequestIdVaultKey)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..))
import qualified Database.SQLite.Simple as SQLite
import GHC.Generics
import qualified Katip as K
import Session.Domain (SessionDataVaultKey)
import qualified Web.ClientSession as ClientSession

data Environment = Production | Development deriving (Show, Eq)

parseEnv :: String -> IO Environment
parseEnv "production" = return Production
parseEnv "development" = return Development
parseEnv s = throwString $ "unknown environment: " <> show s

data LogEnv = LogEnv
  { logEnvContexts :: K.LogContexts,
    logEnvNamespace :: K.Namespace,
    logEnvLogEnv :: K.LogEnv
  }
  deriving (Generic)

data Env = Env
  { envDbConn :: SQLite.Connection,
    envSessionKey :: ClientSession.Key,
    envSessionDataVaultKey :: SessionDataVaultKey,
    envAppEnv :: Environment,
    envRequestIdVaultKey :: RequestIdVaultKey,
    envLogEnv :: LogEnv
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
    via Rename "envDbConn" (Field "envDbConn" () (MonadReader (ReaderT Env IO)))
  deriving
    (HasReader "sessionDataVaultKey" SessionDataVaultKey, HasSource "sessionDataVaultKey" SessionDataVaultKey)
    via Rename "envSessionDataVaultKey" (Field "envSessionDataVaultKey" () (MonadReader (ReaderT Env IO)))
  deriving
    (HasReader "requestIdVaultKey" RequestIdVaultKey, HasSource "requestIdVaultKey" RequestIdVaultKey)
    via Rename "envRequestIdVaultKey" (Field "envRequestIdVaultKey" () (MonadReader (ReaderT Env IO)))
  deriving
    (HasReader "sessionKey" ClientSession.Key, HasSource "sessionKey" ClientSession.Key)
    via Rename "envSessionKey" (Field "envSessionKey" () (MonadReader (ReaderT Env IO)))
  deriving
    (HasReader "logContexts" K.LogContexts, HasSource "logContexts" K.LogContexts)
    via Rename "logEnvContexts" (Field "logEnvContexts" "envLogEnv" (Field "envLogEnv" () (MonadReader (ReaderT Env IO))))
  deriving
    (HasReader "logEnv" K.LogEnv, HasSource "logEnv" K.LogEnv)
    via Rename "logEnvLogEnv" (Field "logEnvLogEnv" "envLogEnv" (Field "envLogEnv" () (MonadReader (ReaderT Env IO))))
  deriving
    (HasReader "logNamespace" K.Namespace, HasSource "logNamespace" K.Namespace)
    via Rename "logEnvNamespace" (Field "logEnvNamespace" "envLogEnv" (Field "envLogEnv" () (MonadReader (ReaderT Env IO))))
  deriving
    (HasReader "appEnv" Environment, HasSource "appEnv" Environment)
    via Rename "envAppEnv" (Field "envAppEnv" () (MonadReader (ReaderT Env IO)))

instance K.Katip App where
  getLogEnv = ask @"logEnv"
  localLogEnv f = local @"logEnv" f

instance K.KatipContext App where
  getKatipContext = ask @"logContexts"
  localKatipContext f = local @"logContexts" f
  getKatipNamespace = ask @"logNamespace"
  localKatipNamespace f = local @"logNamespace" f
