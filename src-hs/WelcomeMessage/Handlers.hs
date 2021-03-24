{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WelcomeMessage.Handlers (saveNewMessage, showMessageEditForm) where

import Control.Monad
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Database.SQLite.Simple as SQLite
import Lucid
import qualified System.Log.FastLogger as Log
import qualified Web.Scotty as S
import WelcomeMessage.DB (getWelcomeMsgFromDb, saveNewWelcomeMsg)
import WelcomeMessage.Domain (WelcomeMsg (..))
import WelcomeMessage.Form (WelcomeMsgFormState (..))
import qualified WelcomeMessage.Form

saveNewMessage ::
  SQLite.Connection ->
  Log.FastLogger ->
  S.ActionM ()
saveNewMessage conn _ = do
  (message :: Text) <-
    S.param "message"
      `S.rescue` const ("" <$ (S.html . renderText $ WelcomeMessage.Form.render (Invalid "Nachricht darf nicht leer sein") ""))
  S.liftAndCatchIO $ saveNewWelcomeMsg conn message
  S.html $ renderText $ WelcomeMessage.Form.render Valid message

showMessageEditForm ::
  SQLite.Connection ->
  Log.FastLogger ->
  S.ActionM ()
showMessageEditForm conn logger = do
  msg <- S.liftAndCatchIO $ getWelcomeMsgFromDb conn
  when (isNothing msg) . S.liftAndCatchIO $ logger "no welcome msg\n"
  S.html . renderText $ WelcomeMessage.Form.render NotValidated (maybe "" (\(WelcomeMsg content _) -> content) msg)
