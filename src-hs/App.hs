{-# LANGUAGE DerivingVia #-}

module App where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Database.SQLite.Simple as SQLite
import Session.Domain (VaultKey)
import qualified System.Log.FastLogger as Log
import qualified Web.ClientSession as ClientSession

type Env = (SQLite.Connection, ClientSession.Key, Log.FastLogger, VaultKey)

newtype App a = App {unApp :: Env -> IO a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadReader Env
    )
    via ReaderT Env IO
