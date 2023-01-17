module Password.Change.Handlers (get, post) where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Control.Error hiding (tryIO, tryJust)
import Form (FormFieldState (..))
import qualified Katip as K
import Layout (LayoutStub (..), success, warning)
import Lucid
import Network.URI.Encode (decode)
import qualified Network.Wai as Wai
import qualified Password.Change.Form as F
import qualified Password.Password as Password
import qualified Password.Reset.Token as Token
import qualified UnliftIO
import Wai (parseParams, parseQueryParams)
import Prelude hiding (id, log)

-- ... the continuation of the above comment. This is for actually typing in the new PW
layout :: Html () -> LayoutStub
layout = LayoutStub "Passwort Ändern"

post ::
  ( MonadIO m,
    MonadReader env m,
    K.KatipContext m,
    UnliftIO.MonadUnliftIO m,
    App.HasDb env,
    MonadThrow m
  ) =>
  Wai.Request ->
  m LayoutStub
post req = do
  params <- liftIO $ parseParams req

  case Map.lookup "token" params of
    Nothing ->
      return . layout . warning $
        [i|Zum Ändern des Passworts ist ein Verifizierungs-Code notwendig, der
        normalerweise automatisch aus dem Link in der Email herausgelesen wird.
        Dieser Code fehlt jedoch. Das Password kann nur über den Link in der Email
        geändert werden. Falls der richtige Link verwendet wurde, bitte an einen
        Administrator wenden.|]
    Just tok -> do
      let pw = (Map.findWithDefault "" "inputPassword" params)
          pwMatch = (Map.findWithDefault "" "inputPasswordMatch" params)
          input = F.FormInput pw pwMatch

      if (pw /= pwMatch)
        then
          return $
            page
              ( F.form
                  tok
                  input
                  ( F.FormState
                      (Invalid "Passwörter stimmen nicht überein")
                      (Invalid "Passwörter stimmen nicht überein")
                  )
              )
        else do
          let notEmptyMsg = "Feld darf nicht leer sein"

          if ((pw == "") || (pwMatch == ""))
            then
              return $
                page
                  (F.form tok input (F.FormState (Invalid notEmptyMsg) (Invalid notEmptyMsg)))
            else do
              hashed <- Password.hash pw

              runExceptT (Token.parse tok) >>= \case
                Left (Token.NotFound token) -> do
                  K.logLocM K.InfoS "token not found"
                  return . layout $
                    warning
                      [i|Der Verifizierungs-Code aus der Email wurde nicht gefunden, bitte an
                         einen Administrator wenden: #{token}|]
                Left (Token.NoUser userid) -> do
                  K.logLocM K.ErrorS "user not found for token"
                  return . layout $
                    warning
                      [i|Kein Nutzer zu diesem Verifizierungs-Code registriert. Bitte an einen
                         Administrator wenden: #{userid}|]
                Left (Token.Expired expired) -> do
                  K.logLocM K.InfoS "token expired"
                  return . layout $
                    warning
                      [i|Der Verifizierungs-Code ist bereits abgelaufen. Bitte nochmals einen
                         neuen Link anfordern per 'Password vergessen' Knopf. Falls das Problem
                         weiterhin besteht bitte an einen Administrator wenden. Der Code ist am
                         #{expired} abgelaufen.|]
                Right (Token.Valid (Token.Token {..})) -> do
                  K.katipAddContext (K.sl "token_user_id" tokenUserId) $ do
                    K.logLocM K.InfoS "successfully changed password"
                    Password.update tokenUserId hashed
                    return . layout $ success "Password erfolgreich geändert"

-- GET handler that shows the form that lets users enter a new password.
-- Expects a token to be passed via query string parameters. That token is
-- later used to verify that the change password request is actually valid.
get ::
  (MonadIO m, MonadThrow m) =>
  Wai.Request ->
  m LayoutStub
get req = do
  case decode' <$> (Map.lookup "token" $ parseQueryParams req) of
    Nothing -> throwString "password change form requires ?token= to be set, but it's empty"
    Just token ->
      return $
        LayoutStub "Passwort Ändern" $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            F.form token F.emptyForm F.emptyState
  where
    decode' = T.pack . decode . T.unpack

page :: Html () -> LayoutStub
page = layout . div_ [class_ "container p-3 d-flex justify-content-center"]
