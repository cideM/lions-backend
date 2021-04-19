{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PasswordReset.Handlers
  ( showResetForm,
    showChangePwForm,
    handleChangePw,
    handleReset,
  )
where

import Capability.Reader (HasReader (..), ask)
import Control.Exception.Safe
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BCrypt
import Crypto.Random (SystemRandom, genBytes, newGenIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Debug.Trace
import Katip
import Layout (ActiveNavLink (..), layout)
import Lucid
import Network.URI.Encode (decode, encode)
import qualified Network.Wai as Wai
import PasswordReset.DB (deleteToken, getTokenByValue, getTokenForUser, insertToken, updatePassword)
import PasswordReset.Domain (Token (..), TokenCreate (..), TokenId (..), hashPw)
import qualified PasswordReset.Form
import Time.Time (timeDaysFromNow)
import User.DB (getIdAndPwByEmail, getUser)
import User.Domain (UserId (..))
import Wai (parseParams, parseQueryParams)

newTokenCreate :: (MonadIO m, MonadThrow m) => UserId -> m TokenCreate
newTokenCreate userid = do
  expires <- liftIO $ timeDaysFromNow 10
  (g :: SystemRandom) <- liftIO newGenIO
  token <- case genBytes 20 g of
    Left e -> throwString $ show e
    Right (token', _) ->
      liftIO (BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy token') >>= \case
        Nothing -> throwString "hashing reset token failed"
        Just token'' -> return $ decodeUtf8 token''
  return $ TokenCreate token expires userid

handleReset ::
  ( MonadIO m,
    MonadThrow m,
    KatipContext m,
    HasReader "dbConn" SQLite.Connection m
  ) =>
  Wai.Request ->
  m (Html ())
handleReset req = do
  conn <- ask @"dbConn"
  params <- liftIO $ parseParams req
  case Map.lookup "email" params of
    Nothing -> return $ layout "Passwort Zurücksetzen" Nothing (resetForm $ Just "Email darf nicht leer sein")
    Just email ->
      liftIO (getIdAndPwByEmail conn email) >>= \case
        Nothing -> return $ layout "Passwort Zurücksetzen" Nothing (resetForm $ Just "Diese Email-Adresse ist nicht beim Lions Club Achern registiert")
        Just (userid, _) -> do
          newToken@TokenCreate {..} <- newTokenCreate userid
          liftIO . SQLite.withTransaction conn $ do
            deleteToken conn userid
            insertToken conn newToken
          liftIO $ print (encode $ Text.unpack tokenCreateValue)
          return . layout "Passwort Zurücksetzen" Nothing $
            div_ [class_ "container p-3 d-flex justify-content-center"] $
              div_ [class_ "row col-md-6"] $ do
                p_ [class_ "alert alert-success", role_ "alert"] "Email mit Link wurde verschickt!"

resetForm :: Maybe Text -> Html ()
resetForm errMsg = do
  div_ [class_ "container p-2"] $ do
    div_ [class_ "row d-flex justify-content-center"] $ do
      div_ [class_ "col-md-6"] $ do
        h1_ [class_ "h4 mb-3"] "Passwort Zurücksetzen"
        case errMsg of
          Nothing -> mempty
          Just msg -> p_ [class_ "my-3 alert alert-danger"] $ toHtml msg
        p_ [class_ "my-3 alert alert-secondary"] "Bitte die Email-Adresse eintragen, mit der du beim Lions Club Achern angemeldet bist. Es wird dann ein Link an diese Email-Adresse verschickt, über welchen du dein Passwort ändern kannst."
        form_ [action_ "/passwort/link", method_ "post"] $ do
          label_ [for_ "emailInput", class_ "form-label"] "Email Adresse"
          input_ [required_ "required", class_ "form-control", name_ "email", id_ "emailInput", placeholder_ "platzhalter@email.de"]
          button_ [class_ "mt-3 btn btn-primary", type_ "submit"] "Absenden"

showResetForm ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  m (Html ())
showResetForm = do
  return $ layout "Passwort Zurücksetzen" Nothing (resetForm Nothing)

handleChangePw ::
  ( MonadIO m,
    MonadThrow m,
    HasReader "dbConn" SQLite.Connection m,
    KatipContext m
  ) =>
  Wai.Request ->
  m (Html ())
handleChangePw req = do
  conn <- ask @"dbConn"
  params <- liftIO $ parseParams req
  case Map.lookup "token" params of
    Nothing ->
      return $ onError noTokenMsg
    Just token ->
      let input =
            PasswordReset.Form.FormInput
              (Map.findWithDefault "" "inputPassword" params)
              (Map.findWithDefault "" "inputPasswordMatch" params)
       in case PasswordReset.Form.makePassword input of
            Left state -> do
              return $
                layout "Passwort Ändern" Nothing $
                  div_ [class_ "container p-3 d-flex justify-content-center"] $
                    PasswordReset.Form.render token input state
            Right pw -> do
              liftIO (getTokenByValue conn token) >>= \case
                Nothing -> do
                  logLocM ErrorS "password change request but token not found in DB"
                  return $ onError "Der Verifizierungs-Code aus der Email wurde nicht gefunden, bitte an einen Administrator wenden"
                Just Token {..} -> do
                  katipAddContext (sl "userid" $ show tokenUserId) $ do
                    katipAddContext (sl "tokenid" $ show tokenId) $ do
                      liftIO (getUser conn tokenUserId) >>= \case
                        Nothing -> do
                          logLocM ErrorS "password change request but user not found in DB"
                          return $ onError "Kein Nutzer zu diesem Verifizierungs-Code registriert. Bitte an einen Administrator wenden."
                        Just _ -> do
                          now <- liftIO Time.getCurrentTime
                          if now >= tokenExpires
                            then do
                              logLocM ErrorS . showLS $ "password change request but token expired at: " <> show tokenExpires
                              return $ onError expiredMsg
                            else do
                              hashed <- hashPw (encodeUtf8 pw)
                              liftIO . SQLite.withTransaction conn $ do
                                deleteToken conn tokenUserId
                                updatePassword conn hashed tokenUserId
                              return . layout "Passwort Zurücksetzen" Nothing $
                                div_ [class_ "container p-3 d-flex justify-content-center"] $
                                  div_ [class_ "row col-md-6"] $ do
                                    p_ [class_ "alert alert-success", role_ "alert"] "Password erfolgreich geändert"
  where
    onError :: Text -> Html ()
    onError msg =
      layout "Fehler" Nothing $
        div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-md-6"] $ do
            p_ [class_ "alert alert-danger mb-4", role_ "alert"] $ toHtml msg

    expiredMsg = "Der Verifizierungs-Code ist bereits abgelaufen. Bitte nochmals einen neuen Link anfordern per 'Password vergessen' Knopf. Falls das Problem weiterhin besteht bitte an einen Administrator wenden"

    noTokenMsg = "Zum Ändern des Passworts ist ein Verifizierungs-Code notwendig, der normalerweise automatisch aus dem Link in der Email herausgelesen wird. Dieser Code fehlt jedoch. Wurde die /passwort/aendern Adreses manuell aufgerufen? Das Password kann nur über den Link in der Email geändert werden. Falls der richtige Link verwendet wurde, bitte an einen Administrator wenden."

showChangePwForm ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  Wai.Request ->
  m (Html ())
showChangePwForm req = do
  let params = parseQueryParams req
  case Map.lookup "token" params of
    Nothing -> throwString "password change form requires ?token= to be set, but it's empty"
    Just token -> do
      let decoded = Text.pack . decode $ Text.unpack token
      return $
        layout "Passwort Ändern" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            PasswordReset.Form.render
              decoded
              PasswordReset.Form.emptyForm
              PasswordReset.Form.emptyState
