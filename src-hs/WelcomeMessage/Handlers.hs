{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WelcomeMessage.Handlers (saveNewMessage, showMessageEditForm) where

import App
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Lucid
import qualified Network.Wai as Wai
import Wai (parseParams)
import WelcomeMessage.DB (getWelcomeMsgFromDb, saveNewWelcomeMsg)
import WelcomeMessage.Domain (WelcomeMsg (..))
import WelcomeMessage.Form (WelcomeMsgFormState (..))
import qualified WelcomeMessage.Form

saveNewMessage ::
  ( MonadIO m,
    MonadReader Env m
  ) =>
  Wai.Request ->
  m (Html ())
saveNewMessage req = do
  (conn, _, _, _) <- ask
  params <- liftIO $ parseParams req
  case Map.lookup "message" params of
    Nothing -> return $ WelcomeMessage.Form.render (Invalid "Nachricht darf nicht leer sein") ""
    Just msg -> do
      liftIO $ saveNewWelcomeMsg conn msg
      return $ WelcomeMessage.Form.render Valid msg

showMessageEditForm ::
  ( MonadIO m,
    MonadReader Env m
  ) =>
  m (Html ())
showMessageEditForm = do
  (conn, _, logger, _) <- ask
  msg <- liftIO $ getWelcomeMsgFromDb conn
  when (isNothing msg) $ liftIO $ logger "no welcome msg\n"
  return $ WelcomeMessage.Form.render NotValidated (maybe "" (\(WelcomeMsg content _) -> content) msg)
