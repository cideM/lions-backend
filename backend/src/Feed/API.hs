module Feed.API
  ( saveNewMessage,
    showMessageEditForm,
    showFeed,
    showAddMessageForm,
    Id (..),
    Message (..),
    handleEditMessage,
    showDeleteConfirmation,
    handleDeleteMessage,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Feed.Form as Form
import Feed.Message
import Feed.DB
import Layout (LayoutStub (..), infoBox, success)
import Locale (german)
import Lucid
import qualified Network.Wai as Wai
import qualified User.Session
import Wai (parseParams)
import Prelude hiding (id)

type ShowEditBtn = Bool

newtype EditHref = EditHref Text

newtype DeleteHref = DeleteHref Text

renderSingleMessage :: EditHref -> DeleteHref -> (Text, Time.ZonedTime) -> ShowEditBtn -> Html ()
renderSingleMessage (EditHref editHref) (DeleteHref deleteHref) (content, date) canEdit =
  div_ [class_ "card"] $ do
    let formatted = Time.formatTime german "%A, %d. %B %Y" date
     in do
          div_ [class_ "card-header"] $ toHtml formatted
          div_ [class_ "card-body"] $
            p_ [class_ "card-text"] $ render content
          div_ [class_ "card-footer"] $
            when canEdit $ do
              a_ [class_ "link-primary me-3", href_ editHref] "Ändern"
              a_ [class_ "link-danger me-3", href_ deleteHref] "Löschen"

renderFeed :: Time.TimeZone -> Bool -> [Message] -> LayoutStub
renderFeed zone userIsAdmin msgs =
  LayoutStub "Willkommen" $
    div_ [class_ "container"] $ do
      div_ [class_ "row row-cols-1 g-4"] $ do
        div_ [class_ "col"] $ do
          when userIsAdmin $
            a_ [class_ "btn btn-sm btn-primary mb-2", href_ "/neu", role_ "button"] "Neue Nachricht"
          h1_ [class_ "h3 m-0 mb-1"] "Interne Neuigkeiten"
        div_ [class_ "col"] $ do
          infoBox $ do
            "Alle Dateien (inklusive Bilderarchiv) des Lions Club Achern befinden sich auf "
            a_ [href_ "https://1drv.ms/f/s!As3H-io1fRdFcZnEJ0BXdpeV9Lw"] "Microsoft OneDrive"
        div_ [class_ "col"] $ do
          when userIsAdmin $ do
            infoBox $ do
              "Mit diesem Link "
              a_ [href_ "https://1drv.ms/f/s!As3H-io1fRdFcUPc-Dz3SC08Wno"] "(Microsoft OneDrive)"
              [i|
              können die Dateien im geteilten Ordner "Lions Dateien" bearbeitet
              werden. Dieser Link ist nur für Administratoren gedacht und wird
              auch nur Administratoren angezeigt. Zum Bearbeiten ist jedoch ein
              Microsoft Account notwendig!
              |]
        div_ [class_ "col"] $ do
          div_ [class_ "row row-cols-1 g-5"] $ do
            mapM_
              ( \(Message (Id id) content datetime) ->
                  let editHref = EditHref $ Text.pack $ "/editieren/" <> show id
                      deleteHref = DeleteHref $ Text.pack $ "/loeschen/" <> show id
                      zoned = Time.utcToZonedTime zone datetime
                   in div_ [class_ "col"] (renderSingleMessage editHref deleteHref (content, zoned) userIsAdmin)
              )
              msgs

-- TODO: Something like this should probably exist for all pages. Maybe make
-- the title smaller, so it's almost more like breadcrums?
pageLayout :: Text -> Html () -> LayoutStub
pageLayout title content =
  LayoutStub title $
    div_ [class_ "container p-2"] $ do
      h1_ [class_ "h4 mb-3"] $ toHtml title
      content

editPageLayout, createPageLayout, deletePageLayout :: Html () -> LayoutStub
editPageLayout = pageLayout "Nachricht Editieren"
createPageLayout = pageLayout "Nachricht Erstellen"
deletePageLayout = pageLayout "Nachricht Löschen"

saveNewMessage ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Wai.Request ->
  User.Session.Admin ->
  m LayoutStub
saveNewMessage req _ = do
  params <- liftIO $ parseParams req
  let input =
        Form.Input
          (Map.findWithDefault "" "date" params)
          (Map.findWithDefault "" "message" params)

  case Form.parse input of
    Left state ->
      return . createPageLayout $ Form.render input state "/neu"
    Right (message, date) -> do
      save message date
      return . createPageLayout $ success "Nachricht erfolgreich erstellt"

handleEditMessage ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Wai.Request ->
  Id ->
  User.Session.Admin ->
  m LayoutStub
handleEditMessage req mid@(Id msgid) _ = do
  params <- liftIO $ parseParams req
  let input =
        Form.Input
          (Map.findWithDefault "" "date" params)
          (Map.findWithDefault "" "message" params)

  case Form.parse input of
    Left state ->
      return . editPageLayout $ Form.render input state [i|/editieren/#{msgid}|]
    Right (message, date) -> do
      update mid message date
      return . editPageLayout $ success "Nachricht erfolgreich editiert"

showDeleteConfirmation ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  User.Session.Admin ->
  m LayoutStub
showDeleteConfirmation mid@(Id msgid) _ = do
  get mid >>= \case
    Nothing -> throwString [i|delete request, but no message with ID: #{msgid}|]
    Just (Message _ content _) -> do
      return . deletePageLayout $ do
        p_ [] "Nachricht wirklich löschen?"
        p_ [class_ "border p-2 mb-4", role_ "alert"] $ toHtml content
        form_ [action_ [i|/loeschen/#{msgid}|], method_ "post", class_ ""] $
          button_ [class_ "btn btn-danger", type_ "submit"] "Ja, Nachricht löschen!"

handleDeleteMessage ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  User.Session.Admin ->
  m LayoutStub
handleDeleteMessage msgid _ = do
  delete msgid
  return . deletePageLayout $ success "Nachricht erfolgreich gelöscht"

showAddMessageForm ::
  (MonadIO m) =>
  User.Session.Admin ->
  m LayoutStub
showAddMessageForm _ = do
  now <- liftIO $ Time.getCurrentTime
  let formatted = Text.pack . Time.formatTime german "%d.%m.%Y %R" $ now
  return . createPageLayout $ Form.render (Form.Input formatted "") Form.emptyState "/neu"

showMessageEditForm ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  User.Session.Admin ->
  m LayoutStub
showMessageEditForm mid@(Id msgid) _ = do
  get mid >>= \case
    Nothing -> throwString $ "edit message but no welcome message found for id: " <> show msgid
    Just (Message _ content date) -> do
      let formatted = Text.pack . Time.formatTime german "%d.%m.%Y %R" $ date
      return . editPageLayout $ Form.render (Form.Input formatted content) Form.emptyState [i|/editieren/#{msgid}|]

showFeed ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  User.Session.Authenticated ->
  m LayoutStub
showFeed auth = do
  msgs :: [Message] <- getAll
  zone <- liftIO $ Time.getCurrentTimeZone
  return $ renderFeed zone (User.Session.isAdmin' auth) msgs
