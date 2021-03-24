{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Session.Middleware (middleware) where

import Control.Error (note, runExceptT)
import Control.Exception.Safe
import Control.Monad.Except (MonadError, liftEither)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Network.HTTP.Types (status302, status500)
import qualified Network.Wai as Wai
import Session.DB (getSessionFromDb)
import Session.Domain (Session (..), SessionId (..), ValidSession (..), VaultKey, makeValidSession)
import qualified System.Log.FastLogger as Log
import TextShow
import User.DB
  ( getRolesFromDb,
  )
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (id)

getSessionIdFromReq :: (MonadError Text m) => ClientSession.Key -> Wai.Request -> m SessionId
getSessionIdFromReq sessionKey req =
  liftEither $
    (note "no cookie header" . lookup "cookie" $ Wai.requestHeaders req)
      >>= note "no session cookie" . lookup "lions_session" . Cookie.parseCookies
      >>= fmap (SessionId . decodeUtf8) . note "empty session cookie" . ClientSession.decrypt sessionKey

-- Consider renewing session ID everytime
middleware ::
  SQLite.Connection ->
  ClientSession.Key ->
  VaultKey ->
  Log.FastLogger ->
  Wai.Middleware
middleware conn sessionKey vaultKey logger nextApp req send =
  ( tryAny . runExceptT $ do
      id <- getSessionIdFromReq sessionKey req
      s <- getSessionFromDb conn id >>= liftEither . note ("no session for id: " <> showt id)
      (ValidSession (Session _ _ userId)) <- makeValidSession s
      roles <- getRolesFromDb conn userId >>= liftEither . note ("no roles for userid: " <> showt userId)
      return (roles, userId)
  )
    >>= either onExcept (either onError onSuccess)
  where
    onExcept e = do
      logger . Log.toLogStr $ show e <> "\n"
      send $ Wai.responseBuilder status500 [] "Interner Fehler"
    onError e = do
      logger . Log.toLogStr $ e <> "\n"
      case Wai.pathInfo req of
        ["login"] -> do
          logger "error but login route, proceeding\n"
          nextApp req send
        _ -> do
          send $ Wai.responseBuilder status302 [("Location", "/login")] ""
    onSuccess (roles, userid) = do
      let vault' = Vault.insert vaultKey (roles, userid) (Wai.vault req)
          req' = req {Wai.vault = vault'}
      logger "successful session authentication in middleware\n"
      nextApp req' send
