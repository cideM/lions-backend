module Password.Change.Handlers (get, post) where

import qualified App
import Control.Exception.Safe
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import qualified Error as E
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
layout = LayoutStub "Passwort Ändern" Nothing

-- TODO: inline this into handler
makeHashedPassword ::
  ( MonadIO m,
    E.MonadError TryResetError m,
    MonadThrow m
  ) =>
  Text ->
  Text ->
  Text ->
  m Password.Hashed
makeHashedPassword tokenInput pw pwMatch = do
  let input = F.FormInput pw pwMatch

  when (pw /= pwMatch) $
    E.throwError
      ( InvalidPassword
          input
          tokenInput
          (F.FormState (Invalid changePwNoMatch) (Invalid changePwNoMatch))
      )

  let notEmptyMsg = "Feld darf nicht leer sein"

  when ((pw == "") || (pwMatch == "")) $
    E.throwError
      ( InvalidPassword
          input
          tokenInput
          (F.FormState (Invalid notEmptyMsg) (Invalid notEmptyMsg))
      )

  Password.hash pw

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
    Nothing -> return . layout . warning $ changePwNoToken
    Just tok -> do
      let pw = (Map.findWithDefault "" "inputPassword" params)
          pwMatch = (Map.findWithDefault "" "inputPasswordMatch" params)

      ( E.runExceptT $
          (,)
            <$> (E.withExceptT' ParseE $ Token.parse tok)
            <*> makeHashedPassword tok pw pwMatch
        )
        >>= \case
          Left e -> do
            K.logLocM K.ErrorS $ K.ls $ show e
            return $ renderTryResetError e
          Right (Token.Valid (Token.Token {..}), hashed) -> do
            updatePw tokenUserId hashed
            return . layout $ success "Password erfolgreich geändert"
  where
    updatePw userid hashed = do
      dbConn <- asks App.getDb
      UnliftIO.withRunInIO $ \runInIO ->
        SQLite.withTransaction
          dbConn
          ( runInIO $ do
              Token.delete userid
              Password.update hashed userid
          )

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
        LayoutStub "Passwort Ändern" Nothing $
          div_ [class_ "container p-3 d-flex justify-content-center"] $
            F.form token F.emptyForm F.emptyState
  where
    decode' = T.pack . decode . T.unpack

-- The various things that I expect to happen when trying to change a user's
-- password. Each has a different error message that is shown to the user.
data TryResetError
  = InvalidPassword F.FormInput Text F.FormState
  | ParseE Token.ParseError
  deriving (Show)

-- Render the different variations of the TryResetError into Html that we can
-- send to the user directly. The goal of functions like this one is to have less noise in the handler.
renderTryResetError :: TryResetError -> LayoutStub
renderTryResetError (InvalidPassword input token state) =
  layout $
    div_
      [class_ "container p-3 d-flex justify-content-center"]
      $ F.form token input state
renderTryResetError (ParseE (Token.NotFound token)) =
  layout . warning . changePwTokenNotFound . T.pack $ show token
renderTryResetError (ParseE (Token.NoUser userid)) =
  layout . warning . changePwUserNotFound . T.pack $ show userid
renderTryResetError (ParseE (Token.Expired expired)) =
  layout . warning . changePwTokenExpired . T.pack $ show expired

-- Copy that I didn't want in the handler code because it distracts from the
-- actual logic
changePwNoToken :: Text
changePwNoToken =
  [i|Zum Ändern des Passworts ist ein Verifizierungs-Code notwendig, der
  normalerweise automatisch aus dem Link in der Email herausgelesen wird.
  Dieser Code fehlt jedoch. Das Password kann nur über den Link in der Email
  geändert werden. Falls der richtige Link verwendet wurde, bitte an einen
  Administrator wenden.|]

changePwTokenExpired :: Text -> Text
changePwTokenExpired expired =
  [i|Der Verifizierungs-Code ist bereits abgelaufen. Bitte nochmals einen
  neuen Link anfordern per 'Password vergessen' Knopf. Falls das Problem
  weiterhin besteht bitte an einen Administrator wenden. Der Code ist am
  #{expired} abgelaufen.|]

changePwUserNotFound :: Text -> Text
changePwUserNotFound userid =
  [i|Kein Nutzer zu diesem Verifizierungs-Code registriert. Bitte an einen
  Administrator wenden: #{userid}|]

changePwTokenNotFound :: Text -> Text
changePwTokenNotFound token =
  [i|Der Verifizierungs-Code aus der Email wurde nicht gefunden, bitte an
  einen Administrator wenden: #{token}|]

changePwNoMatch :: Text
changePwNoMatch = "Passwörter stimmen nicht überein"
