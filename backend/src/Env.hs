module Env
  ( withAppEnv,
  )
where

import qualified App
import Control.Monad.Trans.Resource (runResourceT, withInternalState)
import qualified DB
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Katip as K
import qualified Logging
import qualified Network.AWS as AWS
import System.Environment (getEnv)
import qualified Web.ClientSession as ClientSession

withAppEnv :: (App.Env -> K.KatipContextT IO b) -> IO b
withAppEnv f = do
  sqlitePath <- getEnv "LIONS_SQLITE_PATH"
  appEnv <- getEnv "LIONS_ENV" >>= App.parseEnv
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

  let logLevel = case appEnv of
        App.Production -> K.InfoS
        _ -> K.DebugS

      verbosity = case appEnv of
        App.Production -> K.V2
        _ -> K.V2

  sessionKey <- ClientSession.getKey sessionKeyFile
  sessionDataVaultKey <- Vault.newKey
  requestIdVaultKey <- Vault.newKey

  DB.withConnection
    sqlitePath
    ( \conn -> do
        runResourceT $
          withInternalState
            ( \internalState -> do
                Logging.withKatip
                  verbosity
                  logLevel
                  "main"
                  (K.Environment . Text.pack $ show appEnv)
                  $ do
                    ctx <- K.getKatipContext
                    ns <- K.getKatipNamespace
                    logEnv <- K.getLogEnv

                    let port = (3000 :: Int)

                    f
                      ( App.Env
                          { envDatabaseConnection = conn,
                            envEnvironment = appEnv,
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
