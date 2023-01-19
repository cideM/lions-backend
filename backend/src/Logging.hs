{-# LANGUAGE TemplateHaskell #-}

module Logging where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import Katip
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai as Wai
import qualified User.Session as Session
import qualified Wai

middleware ::
  ( MonadIO m,
    KatipContext m,
    MonadThrow m,
    MonadReader env m,
    App.HasRequestIdVaultKey env,
    App.HasSessionDataVaultKey env
  ) =>
  Wai.MiddlewareT m
middleware nextApp req send = do
  now <- liftIO $ Time.getCurrentTime

  let vault = Wai.vault req
  vaultKey <- asks App.getRequestIdVaultKey
  requestId <- case Vault.lookup vaultKey vault of
    Nothing -> throwString "no request ID key in vault"
    Just key -> return . Text.pack $ show key

  sessionVaultKey <- asks App.getSessionDataVaultKey
  let sessionUserId = Session.sessionUserId <$> Session.get (Session.fromVault sessionVaultKey vault)

  nextApp req $ \response -> do
    afterResponse <- liftIO $ Time.getCurrentTime

    let status = statusCode $ Wai.responseStatus response
        -- This doesn't work. I guess it's because I'm not setting this header.
        -- responseSize = Wai.contentLength (Wai.responseHeaders response)
        -- TODO: Fix response size logging
        (durationInMilliseconds :: Integer) =
          floor
            . (1e6 *)
            . Time.nominalDiffTimeToSeconds
            $ afterResponse `Time.diffUTCTime` now

    let fields =
          (sl "time" now)
            -- <> (sl "response_size" responseSize)
            <> (sl "status" status)
            <> (sl "session_user_id" sessionUserId)
            <> (sl "request_path" (decodeUtf8 $ Wai.rawPathInfo req))
            -- Just give me this as seconds please. Why is this so hard.
            <> (sl "duration_in_ms" durationInMilliseconds)
            <> (sl "request_id" requestId)

    katipAddNamespace "request_logger" . katipAddContext fields $ $(logTM) InfoS "request"

    send response
