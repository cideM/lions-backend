module App
  ( Environment (..),
    parseEnv,
    App (..),
    Env (..),
    HasDb (..),
    HasEventStorage (..),
    HasAwsAccessKey (..),
    HasEnvironment (..),
    HasAwsSecretAccessKey (..),
    HasScryptSignerKey (..),
    HasScryptSaltSeparator (..),
    HasSessionDataVaultKey (..),
    HasRequestIdVaultKey (..),
    HasSessionEncryptionKey (..),
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.ByteString (ByteString)
import qualified Database.SQLite.Simple as SQLite
import qualified Katip as K
import qualified Network.AWS as AWS
import qualified Request.Types
import qualified Session.Types
import qualified UnliftIO
import qualified Web.ClientSession as ClientSession

newtype App env result = App {unApp :: env -> IO result}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      UnliftIO.MonadUnliftIO,
      MonadThrow,
      MonadReader env,
      MonadCatch
    )
    via ReaderT env IO

data Environment = Production | Development deriving (Show, Eq)

parseEnv :: String -> IO Environment
parseEnv "production" = return Production
parseEnv "development" = return Development
parseEnv s = throwString $ "unknown environment: " <> show s

data Env = Env
  { envDatabaseConnection :: SQLite.Connection,
    envEnvironment :: Environment,
    envAwsAccessKey :: AWS.AccessKey,
    envAwsSecretAccessKey :: AWS.SecretKey,
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

class HasEnvironment a where
  getEnv :: a -> Environment

instance HasEnvironment Environment where
  getEnv = id

instance HasEnvironment Env where
  getEnv = envEnvironment

class HasDb a where
  getDb :: a -> SQLite.Connection

instance HasDb SQLite.Connection where
  getDb = id

instance HasDb Env where
  getDb = envDatabaseConnection

class HasEventStorage a where
  getStorageDir :: a -> FilePath

instance HasEventStorage FilePath where
  getStorageDir = id

instance HasEventStorage Env where
  getStorageDir = envEventAttachmentStorageDir

class HasAwsAccessKey a where
  getAwsAccessKey :: a -> AWS.AccessKey

instance HasAwsAccessKey AWS.AccessKey where
  getAwsAccessKey = id

instance HasAwsAccessKey Env where
  getAwsAccessKey = envAwsAccessKey

class HasAwsSecretAccessKey a where
  getAwsSecretAccessKey :: a -> AWS.SecretKey

instance HasAwsSecretAccessKey AWS.SecretKey where
  getAwsSecretAccessKey = id

instance HasAwsSecretAccessKey Env where
  getAwsSecretAccessKey = envAwsSecretAccessKey

class HasScryptSignerKey a where
  getScryptSignerKey :: a -> ByteString

instance HasScryptSignerKey ByteString where
  getScryptSignerKey = id

instance HasScryptSignerKey Env where
  getScryptSignerKey = envScryptSignerKey

class HasScryptSaltSeparator a where
  getScryptSaltSeparator :: a -> ByteString

instance HasScryptSaltSeparator ByteString where
  getScryptSaltSeparator = id

instance HasScryptSaltSeparator Env where
  getScryptSaltSeparator = envScryptSaltSeparator

class HasSessionDataVaultKey a where
  getSessionDataVaultKey :: a -> Session.Types.VaultKey

instance HasSessionDataVaultKey Session.Types.VaultKey where
  getSessionDataVaultKey = id

instance HasSessionDataVaultKey Env where
  getSessionDataVaultKey = envSessionDataVaultKey

class HasRequestIdVaultKey a where
  getRequestIdVaultKey :: a -> Request.Types.IdVaultKey

instance HasRequestIdVaultKey Request.Types.IdVaultKey where
  getRequestIdVaultKey = id

instance HasRequestIdVaultKey Env where
  getRequestIdVaultKey = envRequestIdVaultKey

class HasSessionEncryptionKey a where
  getSessionEncryptionKey :: a -> ClientSession.Key

instance HasSessionEncryptionKey ClientSession.Key where
  getSessionEncryptionKey = id

instance HasSessionEncryptionKey Env where
  getSessionEncryptionKey = envSessionEncryptionKey

instance K.Katip (App Env) where
  getLogEnv = asks envLogEnv
  localLogEnv f (App m) = App (local (\s -> s {envLogEnv = f (envLogEnv s)}) m)

instance K.KatipContext (App Env) where
  getKatipContext = asks envLogContext
  localKatipContext f (App m) = App (local (\s -> s {envLogContext = f (envLogContext s)}) m)
  getKatipNamespace = asks envLogNamespace
  localKatipNamespace f (App m) = App (local (\s -> s {envLogNamespace = f (envLogNamespace s)}) m)
