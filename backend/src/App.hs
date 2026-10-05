module App
  ( App (..),
    withAppEnv,
    Env (..),
    HasDb (..),
    HasAWS (..),
    HasScryptSignerKey (..),
    HasScryptSaltSeparator (..),
    HasSessionDataVaultKey (..),
    HasRequestIdVaultKey (..),
    HasSessionEncryptionKey (..),
    HasMaintenanceMode (..),
    HasEmailMode (..),
    EmailMode (..),
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.Auth as AWSAuth
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Reader.Class (MonadReader, asks, local)
import qualified DB
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Katip
import qualified Katip as K
import qualified Request.Types
import System.Environment (getEnv, lookupEnv)
import System.IO (stdout)
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

data Env = Env
  { envDatabaseConnection :: SQLite.Connection,
    envAWSEnv :: AWS.Env,
    envScryptSignerKey :: ByteString,
    envScryptSaltSeparator :: ByteString,
    envSessionDataVaultKey :: User.Session.VaultKey,
    envRequestIdVaultKey :: Request.Types.IdVaultKey,
    envSessionEncryptionKey :: ClientSession.Key,
    -- When set, every request is answered with a maintenance notice
    envMaintenanceMode :: Bool,
    -- Whether emails are sent through SES or only written to the log
    envEmailMode :: EmailMode,
    envLogNamespace :: K.Namespace,
    envLogContext :: K.LogContexts,
    envLogEnv :: K.LogEnv
  }

class HasAWS a where
  getAWSEnv :: a -> AWS.Env

instance HasAWS AWS.Env where
  getAWSEnv = id

instance HasAWS Env where
  getAWSEnv = envAWSEnv

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

class HasMaintenanceMode a where
  getMaintenanceMode :: a -> Bool

instance HasMaintenanceMode Bool where
  getMaintenanceMode = id

instance HasMaintenanceMode Env where
  getMaintenanceMode = envMaintenanceMode

-- | How outgoing email is delivered. 'EmailLog' is for local development and
-- the black box tests: the message is logged instead of sent, so no AWS
-- credentials are needed.
data EmailMode = EmailSes | EmailLog
  deriving (Show, Eq)

class HasEmailMode a where
  getEmailMode :: a -> EmailMode

instance HasEmailMode EmailMode where
  getEmailMode = id

instance HasEmailMode Env where
  getEmailMode = envEmailMode

instance K.Katip (App Env) where
  getLogEnv = asks envLogEnv
  localLogEnv f (App m) = App (local (\s -> s {envLogEnv = f (envLogEnv s)}) m)

instance K.KatipContext (App Env) where
  getKatipContext = asks envLogContext
  localKatipContext f (App m) = App (local (\s -> s {envLogContext = f (envLogContext s)}) m)
  getKatipNamespace = asks envLogNamespace
  localKatipNamespace f (App m) = App (local (\s -> s {envLogNamespace = f (envLogNamespace s)}) m)

withAppEnv :: (App.Env -> KatipContextT IO b) -> IO b
withAppEnv f = do
  lionsLogLevel <- Text.pack <$> getEnv "LIONS_LOG_LEVEL"
  let logLevel = fromMaybe InfoS $ textToSeverity lionsLogLevel

  sqlitePath <- getEnv "LIONS_SQLITE_PATH"
  sessionKeyFile <- getEnv "LIONS_SESSION_KEY_FILE"
  signerKey <- encodeUtf8 . Text.pack <$> getEnv "LIONS_SCRYPT_SIGNER_KEY"
  saltSep <- encodeUtf8 . Text.pack <$> getEnv "LIONS_SCRYPT_SALT_SEP"
  -- Optional. "log" writes emails to the log instead of sending them, in
  -- which case the SES credentials are not needed.
  emailMode <-
    maybe EmailSes (\v -> if Text.toLower (Text.strip (Text.pack v)) == "log" then EmailLog else EmailSes)
      <$> lookupEnv "LIONS_EMAIL_MODE"
  mailAwsAccessKey <- fromMaybe "" <$> lookupEnv "LIONS_AWS_SES_ACCESS_KEY"
  mailAwsSecretAccessKey <- fromMaybe "" <$> lookupEnv "LIONS_AWS_SES_SECRET_ACCESS_KEY"

  -- Optional. Any value other than empty, "0" or "false" enables maintenance
  -- mode, so that it can be toggled with "flyctl secrets set/unset".
  maintenanceMode <-
    maybe False (\v -> Text.toLower (Text.strip (Text.pack v)) `notElem` ["", "0", "false"])
      <$> lookupEnv "LIONS_MAINTENANCE"

  awsEnv <- do
    let aKey = AWSAuth.AccessKey (encodeUtf8 (Text.pack mailAwsAccessKey))
        sKey = AWSAuth.SecretKey (encodeUtf8 (Text.pack mailAwsSecretAccessKey))
    (awsEnv :: AWSAuth.Env) <- AWS.newEnv (pure . AWSAuth.fromKeys aKey sKey)
    return awsEnv

  sessionKey <- ClientSession.getKey sessionKeyFile
  sessionDataVaultKey <- Vault.newKey
  requestIdVaultKey <- Vault.newKey

  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem logLevel) V2

  rootLogEnv <- initLogEnv "main" (Environment "production")

  let rootLogEnvWithScribes =
        registerScribe
          "stdout"
          handleScribe
          defaultScribeSettings
          rootLogEnv

  bracket rootLogEnvWithScribes closeScribes $ \le -> do
    let initialContext = ()
    let initialNamespace = "server"

    DB.withConnection
      sqlitePath
      ( \conn -> do
          runKatipContextT le initialContext initialNamespace $ do
            ctx <- getKatipContext
            ns <- getKatipNamespace
            logEnv <- getLogEnv

            f
              ( App.Env
                  { envDatabaseConnection = conn,
                    envScryptSignerKey = signerKey,
                    envAWSEnv = awsEnv,
                    envScryptSaltSeparator = saltSep,
                    envSessionDataVaultKey = sessionDataVaultKey,
                    envRequestIdVaultKey = requestIdVaultKey,
                    envSessionEncryptionKey = sessionKey,
                    envMaintenanceMode = maintenanceMode,
                    envEmailMode = emailMode,
                    envLogNamespace = ns,
                    envLogContext = ctx,
                    envLogEnv = logEnv
                  }
              )
      )
