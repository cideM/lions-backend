module App.Test (Env (..)) where

import Data.ByteString (ByteString)
import qualified Database.SQLite.Simple as SQLite
import qualified Katip as K
import qualified Request.Types
import qualified App
import qualified Session.Types
import qualified Web.ClientSession as ClientSession

data Env = Env
  { envDatabaseConnection :: SQLite.Connection,
    envEnvironment :: App.Environment,
    envScryptSignerKey :: ByteString,
    envScryptSaltSeparator :: ByteString,
    envEventAttachmentStorageDir :: FilePath,
    envSessionDataVaultKey :: Session.Types.VaultKey,
    envRequestIdVaultKey :: Request.Types.IdVaultKey,
    envSessionEncryptionKey :: ClientSession.Key,
    envLogNamespace :: K.Namespace,
    envLogContext :: K.LogContexts,
    envLogEnv :: K.LogEnv
  }

