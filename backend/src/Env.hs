module Env
  ( withAppEnv,
  )
where

import qualified App
import Control.Monad.Trans.Resource (runResourceT, withInternalState)
import qualified DB
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Katip as K
import qualified Logging
import qualified Network.AWS as AWS
import qualified Password.Reset.Mail as Mail
import qualified System.Directory
import System.Environment (getEnv)
import qualified Web.ClientSession as ClientSession

withAppEnv :: (App.Env -> K.KatipContextT IO b) -> IO b
withAppEnv f = do
  sqlitePath <- getEnv "LIONS_SQLITE_PATH"

  -- If this is test, do the fake mail thing
  appEnv <- getEnv "LIONS_ENV" >>= App.parseEnv
  sessionKeyFile <- getEnv "LIONS_SESSION_KEY_FILE"
  signerKey <- encodeUtf8 . Text.pack <$> getEnv "LIONS_SCRYPT_SIGNER_KEY"
  saltSep <- encodeUtf8 . Text.pack <$> getEnv "LIONS_SCRYPT_SALT_SEP"
  storageDir <- getEnv "LIONS_STORAGE_DIR"

  -- Determine if we should send actual email using AWS or if we just fake it.
  sendMail <-
    if appEnv == App.Test
      then do
        mailRef <- IORef.newIORef (Nothing, Nothing)
        return (Mail.sendIoRef mailRef)
      else do
        mailAwsAccessKey <- getEnv "LIONS_AWS_SES_ACCESS_KEY"
        mailAwsSecretAccessKey <- getEnv "LIONS_AWS_SES_SECRET_ACCESS_KEY"
        let aKey = AWS.AccessKey (encodeUtf8 (Text.pack mailAwsAccessKey))
            sKey = AWS.SecretKey (encodeUtf8 (Text.pack mailAwsSecretAccessKey))
        awsEnv <- AWS.newEnv (AWS.FromKeys aKey sKey)
        return (Mail.sendAws awsEnv)

  let logLevel = case appEnv of
        App.Production -> K.InfoS
        _ -> K.DebugS

      verbosity = case appEnv of
        App.Production -> K.V2
        _ -> K.V1

  System.Directory.createDirectoryIfMissing True storageDir

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
                            envMail = sendMail,
                            envScryptSaltSeparator = saltSep,
                            envPort = port,
                            envEventAttachmentStorageDir = storageDir,
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
