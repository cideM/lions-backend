module Env
  ( withAppEnv,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad.Trans.Resource (runResourceT, withInternalState)
import qualified DB
import Data.Maybe
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vault.Lazy as Vault
import Katip
import qualified Network.AWS as AWS
import System.Environment (getEnv)
import System.IO (stdout)
import qualified Web.ClientSession as ClientSession

withAppEnv :: (App.Env -> KatipContextT IO b) -> IO b
withAppEnv f = do
  lionsLogLevel <- Text.pack <$> getEnv "LIONS_LOG_LEVEL"
  let logLevel = fromMaybe InfoS $ textToSeverity lionsLogLevel

  sqlitePath <- getEnv "LIONS_SQLITE_PATH"
  sessionKeyFile <- getEnv "LIONS_SESSION_KEY_FILE"
  signerKey <- encodeUtf8 . Text.pack <$> getEnv "LIONS_SCRYPT_SIGNER_KEY"
  saltSep <- encodeUtf8 . Text.pack <$> getEnv "LIONS_SCRYPT_SALT_SEP"
  mailAwsAccessKey <- getEnv "LIONS_AWS_SES_ACCESS_KEY"
  mailAwsSecretAccessKey <- getEnv "LIONS_AWS_SES_SECRET_ACCESS_KEY"

  awsEnv <- do
    let aKey = AWS.AccessKey (encodeUtf8 (Text.pack mailAwsAccessKey))
        sKey = AWS.SecretKey (encodeUtf8 (Text.pack mailAwsSecretAccessKey))
    awsEnv <- AWS.newEnv (AWS.FromKeys aKey sKey)
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
          runResourceT $
            withInternalState
              ( \internalState -> do
                  runKatipContextT le initialContext initialNamespace $ do
                    ctx <- getKatipContext
                    ns <- getKatipNamespace
                    logEnv <- getLogEnv

                    let port = (3000 :: Int)

                    f
                      ( App.Env
                          { envDatabaseConnection = conn,
                            envScryptSignerKey = signerKey,
                            envInternalState = internalState,
                            envAWSEnv = awsEnv,
                            envScryptSaltSeparator = saltSep,
                            envPort = port,
                            envSessionDataVaultKey = sessionDataVaultKey,
                            envRequestIdVaultKey = requestIdVaultKey,
                            envSessionEncryptionKey = sessionKey,
                            envLogNamespace = ns,
                            envLogContext = ctx,
                            envLogEnv = logEnv
                          }
                      )
              )
      )
