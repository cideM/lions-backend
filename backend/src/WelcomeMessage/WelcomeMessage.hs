module WelcomeMessage.WelcomeMessage
  ( saveNewMessage,
    showMessageEditForm,
    showFeed,
    showAddMessageForm,
    WelcomeMsgId (..),
    WelcomeMsg (..),
    handleEditMessage,
    showDeleteConfirmation,
    handleDeleteMessage,
  )
where

import Control.Exception.Safe
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Layout (ActiveNavLink (..), LayoutStub (..), success)
import Lucid
import qualified Network.Wai as Wai
import qualified Routes.Data as Auth
import Wai (parseParams)
import WelcomeMessage.DB
import WelcomeMessage.Feed (renderFeed)
import WelcomeMessage.Form (WelcomeMsgFormState (..))
import qualified WelcomeMessage.Form as Form
import WelcomeMessage.Types
import Prelude hiding (id)

-- TODO: Something like this should probably exist for all pages. Maybe make
-- the title smaller, so it's almost more like breadcrums?
pageLayout :: Text -> Html () -> LayoutStub
pageLayout title content =
  LayoutStub title (Just Welcome) $
    div_ [class_ "container p-2"] $ do
      h1_ [class_ "h4 mb-3"] $ toHtml title
      content

editPageLayout, createPageLayout, deletePageLayout :: Html () -> LayoutStub
editPageLayout = pageLayout "Nachricht Editieren"
createPageLayout = pageLayout "Nachricht Erstellen"
deletePageLayout = pageLayout "Nachricht Löschen"

saveNewMessage :: SQLite.Connection -> Wai.Request -> Auth.AdminUser -> IO LayoutStub
saveNewMessage conn req _ = do
  params <- parseParams req
  case Map.lookup "message" params of
    Nothing -> return . createPageLayout $ Form.form (Invalid "Nachricht darf nicht leer sein") "/neu" ""
    Just msg -> do
      saveNewWelcomeMsg conn msg
      return . createPageLayout $ Form.form Valid "/neu" msg

handleEditMessage ::
  SQLite.Connection ->
  Wai.Request ->
  WelcomeMsgId ->
  Auth.AdminUser ->
  IO LayoutStub
handleEditMessage conn req mid@(WelcomeMsgId msgid) _ =
  Map.findWithDefault "" "message" <$> parseParams req >>= \case
    "" ->
      return . editPageLayout $
        Form.form (Invalid "Nachricht darf nicht leer sein") [i|/editieren/#{msgid}|] ""
    newMsg -> do
      updateWelcomeMsg conn mid newMsg
      return . editPageLayout $ success "Nachricht erfolgreich editiert"

showDeleteConfirmation :: SQLite.Connection -> WelcomeMsgId -> Auth.AdminUser -> IO LayoutStub
showDeleteConfirmation conn mid@(WelcomeMsgId msgid) _ = do
  getWelcomeMsgFromDb conn mid >>= \case
    Nothing -> throwString [i|delete request, but no message with ID: #{msgid}|]
    Just (WelcomeMsg _ content _) -> do
      return . deletePageLayout $ do
        p_ [] "Nachricht wirklich löschen?"
        p_ [class_ "border p-2 mb-4", role_ "alert"] $ toHtml content
        form_ [action_ [i|/loeschen/#{msgid}|], method_ "post", class_ ""] $
          button_ [class_ "btn btn-danger", type_ "submit"] "Ja, Nachricht löschen!"

handleDeleteMessage :: SQLite.Connection -> WelcomeMsgId -> Auth.AdminUser -> IO LayoutStub
handleDeleteMessage conn msgid _ = do
  deleteMessage conn msgid
  return . editPageLayout $ success "Nachricht erfolgreich gelöscht"

showAddMessageForm :: Auth.AdminUser -> IO LayoutStub
showAddMessageForm _ = return . createPageLayout $ Form.form NotValidated "/neu" ""

showMessageEditForm :: SQLite.Connection -> WelcomeMsgId -> Auth.AdminUser -> IO LayoutStub
showMessageEditForm conn mid@(WelcomeMsgId msgid) _ = do
  getWelcomeMsgFromDb conn mid >>= \case
    Nothing -> throwString $ "edit message but no welcome message found for id: " <> show msgid
    Just (WelcomeMsg _ content _) ->
      return . editPageLayout $ Form.form NotValidated [i|/editieren/#{msgid}|] content

showFeed :: SQLite.Connection -> Auth.Authenticated -> IO LayoutStub
showFeed conn auth = do
  let userIsAdmin = case auth of
        Auth.IsAdmin _ -> True
        _ -> False
  msgs <- handleAny onError (getAllWelcomeMsgsFromDb conn)
  zone <- Time.getCurrentTimeZone
  return $ renderFeed zone userIsAdmin msgs
  where
    onError e = throwString $ "error getting welcome messages: " <> show e
