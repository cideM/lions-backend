{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WelcomeMessage.Handlers (saveNewMessage, showMessageEditForm) where

import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Database.SQLite.Simple as SQLite
import Lucid
import qualified Network.Wai as Wai
import qualified System.Log.FastLogger as Log
import Wai (parseParams)
import WelcomeMessage.DB (getWelcomeMsgFromDb, saveNewWelcomeMsg)
import WelcomeMessage.Domain (WelcomeMsg (..))
import WelcomeMessage.Form (WelcomeMsgFormState (..))
import qualified WelcomeMessage.Form

saveNewMessage ::
  SQLite.Connection ->
  Log.FastLogger ->
  Wai.Request ->
  IO (Html ())
saveNewMessage conn _ req = do
  params <- parseParams req
  case Map.lookup "message" params of
    Nothing -> return $ WelcomeMessage.Form.render (Invalid "Nachricht darf nicht leer sein") ""
    Just msg -> do
      saveNewWelcomeMsg conn msg
      return $ WelcomeMessage.Form.render Valid msg

showMessageEditForm ::
  SQLite.Connection ->
  Log.FastLogger ->
  IO (Html ())
showMessageEditForm conn logger = do
  msg <- getWelcomeMsgFromDb conn
  when (isNothing msg) $ logger "no welcome msg\n"
  return $ WelcomeMessage.Form.render NotValidated (maybe "" (\(WelcomeMsg content _) -> content) msg)
