module App
  ( Environment (..),
    parseEnv,
    App (..),
    Env (..),
    HasDb (..),
    HasEnvironment (..),
    HasMail (..),
    HasPort (..),
    HasScryptSignerKey (..),
    HasScryptSaltSeparator (..),
    HasSessionDataVaultKey (..),
    HasRequestIdVaultKey (..),
    HasInternalState (..),
    HasSessionEncryptionKey (..),
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans.Resource (InternalState)
import Data.ByteString (ByteString)
import qualified Database.SQLite.Simple as SQLite
import qualified Katip as K
import qualified Password.Reset.Mail as Mail
import qualified Request.Types
import qualified UnliftIO
import qualified User.Session
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

-- TODO: Get rid of this
data Environment
  = Production -- Uses real infrastructure and sends emails
  | Development -- Logs at Debug
  | Test -- Logs at Debug and doesn't send emails because it doesn't use real infrastructure
  deriving (Show, Eq)

parseEnv :: String -> IO Environment
parseEnv "production" = return Production
parseEnv "development" = return Development
parseEnv "test" = return Test
parseEnv s = throwString $ "unknown environment: " <> show s

data Env = Env
  { envDatabaseConnection :: SQLite.Connection,
    envEnvironment :: Environment,
    envMail :: Mail.SendMail IO,
    envPort :: Int,
    envScryptSignerKey :: ByteString,
    envScryptSaltSeparator :: ByteString,
    envSessionDataVaultKey :: User.Session.VaultKey,
    envRequestIdVaultKey :: Request.Types.IdVaultKey,
    envInternalState :: InternalState,
    envSessionEncryptionKey :: ClientSession.Key,
    envLogNamespace :: K.Namespace,
    envLogContext :: K.LogContexts,
    envLogEnv :: K.LogEnv
  }

class HasPort a where
  getPort :: a -> Int

instance HasPort Int where
  getPort = id

instance HasPort Env where
  getPort = envPort

class HasMail a where
  getMail :: a -> Mail.SendMail IO

instance HasMail (Mail.SendMail IO) where
  getMail = id

instance HasMail Env where
  getMail = envMail

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
  getSessionDataVaultKey :: a -> User.Session.VaultKey

instance HasSessionDataVaultKey User.Session.VaultKey where
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

class HasInternalState a where
  getInternalState :: a -> InternalState

instance HasInternalState InternalState where
  getInternalState = id

instance HasInternalState Env where
  getInternalState = envInternalState

instance K.KatipContext (App Env) where
  getKatipContext = asks envLogContext
  localKatipContext f (App m) = App (local (\s -> s {envLogContext = f (envLogContext s)}) m)
  getKatipNamespace = asks envLogNamespace
  localKatipNamespace f (App m) = App (local (\s -> s {envLogNamespace = f (envLogNamespace s)}) m)
