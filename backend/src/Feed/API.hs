{-# LANGUAGE TemplateHaskell #-}

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
import qualified CMarkGFM as MD
import Control.Error hiding (err)
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import Feed.DB
import qualified Feed.Form as Form
import Feed.Message
import Katip
import Layout (LayoutStub (..), infoBox, success)
import Locale (german)
import Lucid
import qualified Network.Wai as Wai
import Network.Wai.Parse
  ( FileInfo (..),
    ParseRequestBodyOptions,
    defaultParseRequestBodyOptions,
    parseRequestBodyEx,
    setMaxRequestFileSize,
    tempFileBackEnd,
  )
import qualified Text.HTML.SanitizeXSS as SanitizeXSS
import qualified Text.XML.Light as XML
import qualified UnliftIO
import qualified User.Session
import Prelude hiding (id)

type ShowEditBtn = Bool

newtype EditHref = EditHref Text

newtype DeleteHref = DeleteHref Text

renderSingleMessage ::
  EditHref ->
  DeleteHref ->
  Message (Html ()) Time.ZonedTime ->
  [Text] -> -- attachment filenames
  ShowEditBtn ->
  Html ()
renderSingleMessage
  (EditHref editHref)
  (DeleteHref deleteHref)
  message
  filenames
  canEdit =
    div_ [class_ "card"] $ do
      let formatted = Time.formatTime german "%A, %d. %B %Y" $ date message
       in do
            div_ [class_ "card-header"] $ toHtml formatted
            div_ [class_ "card-body"] $ do
              content message

              when (length filenames > 0) $ do
                h2_ [class_ "mt-3 text-muted h6"] "Anhänge"
                ul_ [class_ "m-0"] $ do
                  forM_
                    filenames
                    ( \filename -> do
                        let url =
                              Text.pack $
                                "/news/"
                                  <> show (id message)
                                  <> "/"
                                  <> Text.unpack filename

                        li_ []
                          . a_ [class_ "card-link", href_ url]
                          $ toHtml filename
                    )
            div_ [class_ "card-footer"] $
              when canEdit $ do
                a_ [class_ "link-primary me-3", href_ editHref] "Ändern"
                a_ [class_ "link-danger me-3", href_ deleteHref] "Löschen"

renderFeed :: Bool -> [(Message (Html ()) Time.ZonedTime, [Text])] -> LayoutStub
renderFeed userIsAdmin msgs =
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
              ( \(message, filenames) -> do
                  let editHref =
                        EditHref $
                          Text.pack $
                            "/editieren/" <> show (id message)

                      deleteHref =
                        DeleteHref $
                          Text.pack $
                            "/loeschen/" <> show (id message)

                  div_
                    [class_ "col"]
                    ( renderSingleMessage
                        editHref
                        deleteHref
                        message
                        filenames
                        userIsAdmin
                    )
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

fileUploadOpts :: ParseRequestBodyOptions
fileUploadOpts = setMaxRequestFileSize 100000000 defaultParseRequestBodyOptions

saveNewMessage ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    KatipContext m,
    UnliftIO.MonadUnliftIO m,
    MonadReader env m
  ) =>
  Wai.Request ->
  User.Session.Admin ->
  m LayoutStub
saveNewMessage r _ = do
  Resource.runResourceT $
    Resource.withInternalState $ \internalState -> do
      let backend = tempFileBackEnd internalState
      (params, files) <- liftIO $ parseRequestBodyEx fileUploadOpts backend r

      let paramsMap =
            Map.fromList
              [ (decodeUtf8 k, decodeUtf8 v)
                | (k, v) <- params
              ]

      let checkboxes =
            [ (decodeUtf8 (fileName fileInfo), True)
              | (_, fileInfo) <- files
            ]

          input =
            Form.Input
              (Map.findWithDefault "" "date" paramsMap)
              (Map.findWithDefault "" "message" paramsMap)
              checkboxes

      case Form.parse input of
        Left state ->
          return . createPageLayout $ Form.render input state "/neu"
        Right (message, date) -> do
          conn <- asks App.getDb

          UnliftIO.withRunInIO $ \runInIO ->
            SQLite.withTransaction conn . runInIO $ do
              katipAddNamespace "saveNewMessage -> transaction" $ do
                postId <- save message date

                let newFiles =
                      [ f
                        | f@(_, fileInfo) <- files,
                          fileName fileInfo /= "\"\""
                      ]

                katipAddContext (sl "post_id" postId) $ do
                  $(logTM) DebugS $ showLS newFiles

                  saveAttachments conn postId newFiles

                  return
                    . createPageLayout
                    $ success "Nachricht erfolgreich erstellt"

handleEditMessage ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    UnliftIO.MonadUnliftIO m,
    MonadReader env m
  ) =>
  Wai.Request ->
  Id ->
  User.Session.Admin ->
  m LayoutStub
handleEditMessage r postId _ = do
  Resource.runResourceT . Resource.withInternalState $ \internalState -> do
    let backend = tempFileBackEnd internalState
    (params, files) <- liftIO $ parseRequestBodyEx fileUploadOpts backend r

    -- Is it really a good idea to add these to input? I guess...
    let checkboxes =
          [ (decodeUtf8 value, True)
            | (key, value) <- params,
              key == "newFileCheckbox"
          ]

    let paramsMap =
          Map.fromList
            [ (decodeUtf8 k, decodeUtf8 v)
              | (k, v) <- params
            ]

        input =
          Form.Input
            (Map.findWithDefault "" "date" paramsMap)
            (Map.findWithDefault "" "message" paramsMap)
            checkboxes

    case Form.parse input of
      Left state ->
        return
          . editPageLayout
          . Form.render input state
          $ Text.pack ("/editieren/" <> show postId)
      Right (message, date) -> do
        conn <- asks App.getDb

        UnliftIO.withRunInIO $ \runInIO ->
          SQLite.withTransaction conn . runInIO $ do
            current <- fetchAttachmentFilenames conn postId

            let (keep :: [Text]) = map fst checkboxes
                (remove :: [Text]) = [s | s <- current, not (elem s keep)]

            deleteAttachments conn postId remove

            let newFiles =
                  [ f
                    | f@(_, fileInfo) <- files,
                      fileName fileInfo /= "\"\""
                  ]
            saveAttachments conn postId newFiles

            update postId message date

            return
              . editPageLayout
              $ success "Nachricht erfolgreich editiert"

showDeleteConfirmation ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  User.Session.Admin ->
  m LayoutStub
showDeleteConfirmation postId _ = do
  maybePost <- get postId

  case maybePost of
    Nothing -> do
      let err = "delete request, but no message with ID: " <> show postId
      throwString err
    Just (message, _) -> do
      let (Message _ content _) = message
          formActionURL = Text.pack $ "/loeschen/" <> show postId

      return . deletePageLayout $ do
        p_ [] "Nachricht wirklich löschen?"
        p_ [class_ "border p-2 mb-4", role_ "alert"] $ toHtml content
        form_ [action_ formActionURL, method_ "post", class_ ""] $
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
  return . createPageLayout $ Form.render (Form.Input formatted "" []) Form.emptyState "/neu"

showMessageEditForm ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    MonadReader env m
  ) =>
  Id ->
  User.Session.Admin ->
  m LayoutStub
showMessageEditForm postId _ = do
  maybePost <- get postId

  case maybePost of
    Nothing -> do
      -- TODO: This should be a 404, this is probably the case for many of
      -- these Nothing cases
      let err = "edit message but no welcome message found for id: " <> show postId
      throwString err
    Just (message, filenames) -> do
      let formatted = Text.pack . Time.formatTime german "%d.%m.%Y %R" $ date
          (Message _ content date) = message
          checkboxes = [(f, True) | f <- filenames]
          formInput = Form.Input formatted content checkboxes

      return
        . editPageLayout
        . Form.render formInput Form.emptyState
        $ Text.pack ("/editieren/" <> show postId)

-- Attribute key; "class"
cardTextClass :: XML.QName
cardTextClass = XML.blank_name {XML.qName = "class"}

-- Attribute with key and value; class="card-text"
cardTextAttr :: XML.Attr
cardTextAttr = XML.Attr cardTextClass "card-text"

-- Add class="card-text" to every paragraph
addCardTextClass :: Text -> Either Text Text
addCardTextClass text =
  case XML.parseXML text of
    [] -> Left $ "can't parse source: " <> text
    contents ->
      Right . Text.pack $
        concatMap (XML.showContent . go) contents
  where
    go :: XML.Content -> XML.Content
    go content@(XML.Elem element) =
      let tagName = XML.qName (XML.elName element)
       in case tagName of
            "p" ->
              let attrs' = XML.elAttribs element ++ [cardTextAttr]
                  el' =
                    element
                      { XML.elAttribs = attrs',
                        XML.elContent = map go (XML.elContent element)
                      }
               in XML.Elem el'
            _ -> content
    go other = other

showFeed ::
  ( MonadIO m,
    App.HasDb env,
    MonadThrow m,
    KatipContext m,
    MonadReader env m
  ) =>
  User.Session.Authenticated ->
  m LayoutStub
showFeed auth = do
  zone <- liftIO $ Time.getCurrentTimeZone

  posts :: [(Message Text Time.UTCTime, [Text])] <- getAll

  posts' :: [(Message (Html ()) Time.ZonedTime, [Text])] <-
    traverse transformHTML $ map (berlinTime zone) posts

  $(logTM) DebugS $ "first post: " <> (showLS $ head posts')

  return $ renderFeed (User.Session.isAdmin' auth) posts'
  where
    berlinTime zone (message, filenames) =
      (message {date = Time.utcToZonedTime zone (date message)}, filenames)

    parseMarkdown =
      SanitizeXSS.sanitize
        . MD.commonmarkToHtml [] [MD.extAutolink]

    -- Parse as Markdown, then add a CSS class to each paragraph
    -- of the resulting HTML
    transformHTML (message, filenames) = do
      content' <-
        either
          (throwString . Text.unpack)
          (pure . toHtmlRaw)
          . addCardTextClass
          $ parseMarkdown (content message)

      let message' = message {content = content'}

      pure (message', filenames)
