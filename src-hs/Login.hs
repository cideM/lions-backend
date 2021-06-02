{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Login (logout, login, showLoginForm) where

import Control.Exception.Safe
import qualified Crypto.BCrypt as BCrypt
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Env (Environment (..))
import Layout (ActiveNavLink (..), describedBy_, layout)
import Lucid
import Network.HTTP.Types (status302, status401)
import qualified Network.Wai as Wai
import Session.DB (deleteSession, getSessionsFromDbByUser, saveSession)
import Session.Domain (Session (..), SessionDataVaultKey, SessionId (..), ValidSession (..), createNewSession)
import User.DB (getIdAndPwByEmail)
import Wai (parseParams)
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

type Email = Text

type EmailError = Text

type Pw = Text

type PwError = Text

-- TODO: Would be nice to make this use my Form utils stuff
data LoginFormState = NotLoggedInNotValidated | NotLoggedInValidated Email (Maybe EmailError) Pw (Maybe PwError) | LoggedIn

-- The actual login form. You didn't think it'd be this much code, did you?
loginForm :: LoginFormState -> Html ()
loginForm LoggedIn =
  layout "Login" (Just Login) $ do
    div_ [class_ "container p-3 d-flex justify-content-center"] $
      div_ [class_ "row col-6"] $ do
        p_ [class_ "alert alert-secondary", role_ "alert"] "Du bist bereits eingelogged!"
        form_ [class_ "p-0", method_ "post", action_ "/logout"] $ do
          button_ [class_ "btn btn-primary", type_ "submit", autofocus_] "Ausloggen"
loginForm formState =
  let (email, emailClass, emailErr, pw, pwClass, pwErr) =
        case formState of
          NotLoggedInValidated email' emailErr' pw' pwErr' ->
            ( email',
              makeFormClass emailErr',
              makeErrMsg "invalidEmailFeedback" emailErr',
              pw',
              makeFormClass pwErr',
              makeErrMsg "invalidPasswordFeedback" pwErr'
            )
          _ ->
            ( "",
              makeFormClass Nothing,
              makeErrMsg "invalidEmailFeedback" Nothing,
              "",
              makeFormClass Nothing,
              makeErrMsg "invalidPasswordFeedback" Nothing
            )
   in layout "Login" (Just Login) $
        div_ [class_ "container-md d-flex justify-content-center p-3"] $ do
          form_ [class_ "col-8", method_ "post", action_ "/login"] $ do
            div_ [class_ "row row-cols-8 g-2"] $ do
              div_ [class_ "mb-3 form-floating"] $ do
                input_
                  [ class_ emailClass,
                    type_ "email",
                    name_ "email",
                    id_ "email",
                    required_ "required",
                    value_ email,
                    autofocus_,
                    describedBy_ "invalidEmailFeedback",
                    placeholder_ "hallo@gmail.com"
                  ]
                label_ [class_ "form-label", for_ "email"] "Email Adresse"
                emailErr
            div_ [class_ "row row-cols-8 g-2"] $ do
              div_ [class_ "mb-3 form-floating"] $ do
                input_
                  [ class_ pwClass,
                    type_ "password",
                    name_ "password",
                    id_ "password",
                    required_ "required",
                    value_ pw,
                    describedBy_ "invalidPasswordFeedback",
                    placeholder_ "foobar"
                  ]
                label_ [class_ "form-label", for_ "password"] "Passwort"
                pwErr
            div_ [class_ "row g-2"] $ do
              div_ [class_ "col-md-6"] $ do
                button_ [class_ "w-100 btn btn-primary", type_ "submit"] "Einloggen"
              div_ [class_ "col-md-6"] $ do
                a_ [class_ "w-100 btn btn-secondary", href_ "/passwort/link", role_ "button"] "Passwort vergessen"
  where
    makeFormClass e = "form-control" <> if isJust e then " is-invalid" else ""
    makeErrMsg :: Text -> Maybe Text -> Html ()
    makeErrMsg id text = maybe mempty (div_ [class_ "invalid-feedback", id_ id] . toHtml) text

-- Creates a new session ID and returns that ID and its expiration date
createSessionId ::
  SQLite.Connection ->
  ClientSession.Key ->
  Text ->
  Text ->
  IO (Either Text (ByteString, Time.UTCTime))
createSessionId conn sessionKey email formPw = do
  getIdAndPwByEmail conn email >>= \case
    Nothing -> return $ Left "no user found"
    Just (userId, dbPw) -> do
      if not $ BCrypt.validatePassword (encodeUtf8 dbPw) (encodeUtf8 formPw)
        then return $ Left "incorrect password"
        else do
          newSession@(ValidSession (Session (SessionId sessionId) expires _)) <- createNewSession userId
          saveSession conn newSession
          encryptedSessionId <- ClientSession.encryptIO sessionKey (encodeUtf8 sessionId)
          return $ Right (encryptedSessionId, expires)

logout ::
  SQLite.Connection ->
  Environment ->
  SessionDataVaultKey ->
  Wai.Request ->
  (Wai.Response -> IO a) ->
  IO a
logout conn env sessionDataVaultKey req send = do
  case Vault.lookup sessionDataVaultKey $ Wai.vault req of
    Nothing -> throwString "logout request but no session in vault"
    Just (_, userId) -> do
      -- TODO: Extract logic
      getSessionsFromDbByUser conn userId >>= \case
        [] -> throwString $ "logout request but no session for user with ID: " <> show userId
        sessions -> do
          mapM_ (deleteSession conn) sessions
          send
            . Wai.responseLBS
              status302
              [ ("Content-Type", "text/html; charset=UTF-8"),
                ("Set-Cookie", logoutCookie env),
                ("Location", "/login")
              ]
            $ renderBS ""
  where
    logoutCookie _ =
      LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
        Cookie.defaultSetCookie
          { Cookie.setCookieName = "lions_session",
            Cookie.setCookieValue = "",
            Cookie.setCookieExpires = Nothing,
            Cookie.setCookiePath = Just "/",
            Cookie.setCookieSecure = env == Production,
            Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
            Cookie.setCookieHttpOnly = True
          }

-- POST handler that creates a new session in the DB and sets a cookie with the
-- encrypted session ID
login ::
  SQLite.Connection ->
  ClientSession.Key ->
  Environment ->
  Wai.Request ->
  (Wai.Response -> IO a) ->
  IO a
login conn sessionKey env req send = do
  params <- parseParams req
  let email = Map.findWithDefault "" "email" params
      formPw = Map.findWithDefault "" "password" params
  (createSessionId conn sessionKey email formPw) >>= \case
    Left _ -> renderFormInvalid email formPw
    Right (sessionId, expires) -> do
      let cookie = makeCookie sessionId expires
      send $ Wai.responseLBS status302 [("Set-Cookie", cookie), ("Location", "/")] mempty
  where
    makeCookie sessionId expires =
      LBS.toStrict . BSBuilder.toLazyByteString . Cookie.renderSetCookie $
        Cookie.defaultSetCookie
          { Cookie.setCookieName = "lions_session",
            Cookie.setCookieValue = sessionId,
            Cookie.setCookieExpires = Just expires,
            Cookie.setCookiePath = Just "/",
            Cookie.setCookieSecure = env == Production,
            Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
            Cookie.setCookieHttpOnly = True
          }
    renderFormInvalid email formPw =
      send
        . Wai.responseLBS status401 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . loginForm
        $ NotLoggedInValidated
          email
          (Just "Ungültige Kombination aus Email und Passwort")
          formPw
          (Just "Ungültige Kombination aus Email und Passwort")

-- GET handler for showing the login form
showLoginForm :: SessionDataVaultKey -> Wai.Request -> IO (Html ())
showLoginForm sessionDataVaultKey req = do
  let isLoggedIn = isJust . Vault.lookup sessionDataVaultKey $ Wai.vault req
  return . loginForm $ if isLoggedIn then LoggedIn else NotLoggedInNotValidated
