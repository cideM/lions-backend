module WelcomeMessage.WelcomeMessage
  ( saveNewMessage,
    showMessageEditForm,
    showAddMessageForm,
    WelcomeMsgId (..),
    WelcomeMsg (..),
    handleEditMessage,
    showDeleteConfirmation,
    getAllWelcomeMsgsFromDb,
    handleDeleteMessage,
  )
where

import Control.Exception.Safe
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Layout (ActiveNavLink (..), layout, success)
import Lucid
import qualified Network.Wai as Wai
import qualified Routes.Data as Auth
import Wai (parseParams)
import WelcomeMessage.Form (WelcomeMsgFormState (..))
import qualified WelcomeMessage.Form as Form
import Prelude hiding (id)

-- | Returns the MOST RECENT welcome message, if there is one
getWelcomeMsgFromDb :: SQLite.Connection -> WelcomeMsgId -> IO (Maybe WelcomeMsg)
getWelcomeMsgFromDb conn (WelcomeMsgId id) =
  (SQLite.query conn "SELECT id, content, date FROM welcome_text WHERE id = ?" [id])
    >>= \case
      [(mid, msg, createdAt) :: (Int, Text, Time.UTCTime)] ->
        return . Just $ WelcomeMsg (WelcomeMsgId mid) msg createdAt
      [] -> return Nothing
      other -> return . throwString $ "unexpected result from DB for welcome message" <> show other

-- | Returns all welcome messages in chronological order
getAllWelcomeMsgsFromDb :: SQLite.Connection -> IO [WelcomeMsg]
getAllWelcomeMsgsFromDb conn =
  (SQLite.query_ conn "SELECT id, content, date FROM welcome_text ORDER BY date DESC")
    >>= \case
      (msgs :: [(Int, Text, Time.UTCTime)]) ->
        return $ map (\(id, msg, createdAt) -> WelcomeMsg (WelcomeMsgId id) msg createdAt) msgs

updateWelcomeMsg :: SQLite.Connection -> WelcomeMsgId -> Text -> IO ()
updateWelcomeMsg conn (WelcomeMsgId id) newMsg =
  SQLite.execute conn "UPDATE welcome_text SET content = ? WHERE id = ?" [newMsg, Text.pack $ show id]

saveNewWelcomeMsg :: SQLite.Connection -> Text -> IO ()
saveNewWelcomeMsg conn =
  SQLite.execute conn "INSERT INTO welcome_text (content, date) VALUES (?, datetime('now'))" . SQLite.Only

deleteMessage :: SQLite.Connection -> WelcomeMsgId -> IO ()
deleteMessage conn (WelcomeMsgId id) = SQLite.execute conn "DELETE FROM welcome_text WHERE id = ?" [id]

newtype WelcomeMsgId = WelcomeMsgId Int deriving (Show)

data WelcomeMsg = WelcomeMsg WelcomeMsgId Text Time.UTCTime deriving (Show)

-- TODO: Something like this should probably exist for all pages. Maybe make
-- the title smaller, so it's almost more like breadcrums?
pageLayout :: Text -> Html () -> Html ()
pageLayout title content =
  layout title (Just Welcome) $
    div_ [class_ "container p-2"] $ do
      h1_ [class_ "h4 mb-3"] $ toHtml title
      content

editPageLayout, createPageLayout, deletePageLayout :: Html () -> Html ()
editPageLayout = pageLayout "Nachricht Editieren"
createPageLayout = pageLayout "Nachricht Erstellen"
deletePageLayout = pageLayout "Nachricht Löschen"

saveNewMessage :: SQLite.Connection -> Wai.Request -> Auth.AdminUser -> IO (Html ())
saveNewMessage conn req _ = do
  params <- parseParams req
  case Map.lookup "message" params of
    Nothing -> return $ Form.form (Invalid "Nachricht darf nicht leer sein") "/neu" ""
    Just msg -> do
      saveNewWelcomeMsg conn msg
      return . createPageLayout $ Form.form Valid "/neu" msg

handleEditMessage ::
  SQLite.Connection ->
  Wai.Request ->
  WelcomeMsgId ->
  Auth.AdminUser ->
  IO (Html ())
handleEditMessage conn req mid@(WelcomeMsgId msgid) _ =
  Map.findWithDefault "" "message" <$> parseParams req >>= \case
    "" ->
      return . editPageLayout $
        Form.form (Invalid "Nachricht darf nicht leer sein") [i|/editieren/#{msgid}|] ""
    newMsg -> do
      updateWelcomeMsg conn mid newMsg
      return . editPageLayout $ success "Nachricht erfolgreich editiert"

showDeleteConfirmation :: SQLite.Connection -> WelcomeMsgId -> Auth.AdminUser -> IO (Html ())
showDeleteConfirmation conn mid@(WelcomeMsgId msgid) _ = do
  getWelcomeMsgFromDb conn mid >>= \case
    Nothing -> throwString [i|delete request, but no message with ID: #{msgid}|]
    Just (WelcomeMsg _ content _) -> do
      return . deletePageLayout $ do
        p_ [] "Nachricht wirklich löschen?"
        p_ [class_ "border p-2 mb-4", role_ "alert"] $ toHtml content
        form_ [action_ [i|/loeschen/#{msgid}|], method_ "post", class_ ""] $
          button_ [class_ "btn btn-danger", type_ "submit"] "Ja, Nachricht löschen!"

handleDeleteMessage :: SQLite.Connection -> WelcomeMsgId -> Auth.AdminUser -> IO (Html ())
handleDeleteMessage conn msgid _ = do
  deleteMessage conn msgid
  return . editPageLayout $ success "Nachricht erfolgreich gelöscht"

showAddMessageForm :: Auth.AdminUser -> IO (Html ())
showAddMessageForm _ = return . createPageLayout $ Form.form NotValidated "/neu" ""

showMessageEditForm :: SQLite.Connection -> WelcomeMsgId -> Auth.AdminUser -> IO (Html ())
showMessageEditForm conn mid@(WelcomeMsgId msgid) _ = do
  getWelcomeMsgFromDb conn mid >>= \case
    Nothing -> throwString $ "edit message but no welcome message found for id: " <> show msgid
    Just (WelcomeMsg _ content _) ->
      return . editPageLayout $ Form.form NotValidated [i|/editieren/#{msgid}|] content
