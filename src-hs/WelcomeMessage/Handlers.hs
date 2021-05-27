{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WelcomeMessage.Handlers
  ( saveNewMessage,
    showMessageEditForm,
    showAddMessageForm,
    handleEditMessage,
    showDeleteConfirmation,
    handleDeleteMessage,
  )
where

import Control.Exception.Safe
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import Layout (ActiveNavLink (..), layout)
import Lucid
import qualified Network.Wai as Wai
import qualified Routes.Data as Auth
import Wai (parseParams)
import WelcomeMessage.DB (deleteMessage, getWelcomeMsgFromDb, saveNewWelcomeMsg, updateWelcomeMsg)
import WelcomeMessage.Domain (WelcomeMsg (..), WelcomeMsgId (..))
import WelcomeMessage.Form (WelcomeMsgFormState (..))
import qualified WelcomeMessage.Form

saveNewMessage :: SQLite.Connection -> Wai.Request -> Auth.AdminUser -> IO (Html ())
saveNewMessage conn req _ = do
  params <- parseParams req
  case Map.lookup "message" params of
    Nothing ->
      let formAction = "/neu"
       in return $
            WelcomeMessage.Form.render
              (Invalid "Nachricht darf nicht leer sein")
              formAction
              ""
    Just msg -> do
      saveNewWelcomeMsg conn msg
      let formAction = "/neu"
       in return . layout "Nachricht Erstellen" (Just Welcome) $
            div_ [class_ "container-md"] $ do
              h1_ [class_ "card-title fs-3"] "Nachricht Erstellen"
              WelcomeMessage.Form.render Valid formAction msg

handleEditMessage ::
  SQLite.Connection ->
  Wai.Request ->
  WelcomeMsgId ->
  Auth.AdminUser ->
  IO (Html ())
handleEditMessage conn req mid@(WelcomeMsgId msgid) _ = do
  params <- parseParams req
  let input = Map.findWithDefault "" "message" params
      formAction = Text.pack $ "/editieren/" <> show msgid
  case input of
    "" ->
      return . layout "Nachricht Editieren" (Just Welcome) $
        div_ [class_ "container-md"] $ do
          h1_ [class_ "card-title fs-3"] "Nachricht Editieren"
          WelcomeMessage.Form.render
            (Invalid "Nachricht darf nicht leer sein")
            formAction
            ""
    newMsg -> do
      updateWelcomeMsg conn mid newMsg
      return
        . layout "Nachricht Editieren" (Just Welcome)
        $ div_ [class_ "container p-2"] $ do
          p_ [class_ "alert alert-success", role_ "alert"] "Nachricht erfolgreich editiert"

showDeleteConfirmation ::
  SQLite.Connection ->
  WelcomeMsgId ->
  Auth.AdminUser ->
  IO (Html ())
showDeleteConfirmation conn mid@(WelcomeMsgId msgid) _ = do
  getWelcomeMsgFromDb conn mid >>= \case
    Nothing -> throwString $ "delete request, but no message for welcome message with ID: " <> show msgid
    Just (WelcomeMsg _ content _) -> do
      return . layout "Nachricht Löschen" (Just Welcome) $
        div_ [class_ "container p-3"] $ do
          h1_ [class_ "fs-5"] "Nachricht Wirklich Löschen?"
          p_ [class_ "border p-2 mb-4", role_ "alert"] $
            toHtml content
          form_
            [ action_ . Text.pack $ "/loeschen/" <> show msgid,
              method_ "post",
              class_ ""
            ]
            $ button_ [class_ "btn btn-danger", type_ "submit"] "Ja, Nachricht löschen!"

handleDeleteMessage ::
  SQLite.Connection ->
  WelcomeMsgId ->
  Auth.AdminUser ->
  IO (Html ())
handleDeleteMessage conn msgid _ = do
  deleteMessage conn msgid
  return $
    layout "Nachricht Editieren" (Just Welcome) $
      div_ [class_ "container p-3 d-flex justify-content-center"] $
        div_ [class_ "row col-6"] $ do
          p_ [class_ "alert alert-success", role_ "alert"] "Nachricht erfolgreich gelöscht"

showAddMessageForm :: Auth.AdminUser -> IO (Html ())
showAddMessageForm _ = do
  let formAction = "/neu"
   in return . layout "Neue Nachricht Erstellen" (Just Welcome) $
        div_ [class_ "container p-2"] $ do
          h1_ [class_ "h4 mb-3"] "Neue Nachricht Erstellen"
          WelcomeMessage.Form.render NotValidated formAction ""

showMessageEditForm ::
  SQLite.Connection ->
  WelcomeMsgId ->
  Auth.AdminUser ->
  IO (Html ())
showMessageEditForm conn mid@(WelcomeMsgId msgid) _ = do
  getWelcomeMsgFromDb conn mid >>= \case
    Nothing -> throwString $ "edit message but no welcome message found for id: " <> show msgid
    Just (WelcomeMsg _ content _) ->
      let formAction = Text.pack $ "/editieren/" <> show msgid
       in return . layout "Nachricht Editieren" (Just Welcome) $
            div_ [class_ "container p-2"] $ do
              h1_ [class_ "h4 mb-3"] "Nachricht Editieren"
              WelcomeMessage.Form.render NotValidated formAction content
