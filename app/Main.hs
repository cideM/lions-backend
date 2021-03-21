{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
-- import Database.SQLite.Simple.FromRow
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.QQ (sql)
import Lucid
import Lucid.Base (makeAttribute)
import Network.HTTP.Types (status200, status404)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (Policy (..), addBase, staticPolicy)
import System.Environment (getEnv)
import System.Exit (ExitCode (..), exitWith)
import qualified System.Log.FastLogger as Log
import qualified Web.ClientSession as ClientSession
import qualified Web.Cookie as Cookie
import Prelude hiding (div, head)

expanded_ = makeAttribute "expanded"

data ActiveNavLink = Welcome | Events | Login deriving (Eq)

type NavLink = (Text, Text, ActiveNavLink)

welcomeL, loginL, eventL :: NavLink
welcomeL = ("/", "Startseite", Welcome)
eventL = ("/veranstaltungen", "Veranstaltungen", Events)
loginL = ("/login", "Login", Login)

fst3 :: NavLink -> Text
fst3 (a, _, _) = a

layout :: ActiveNavLink -> Text -> Html () -> Html ()
layout activeNavLink pageTitle pageContent = doctypehtml_ $ do
  head_ $ do
    title_ $ toHtml pageTitle
    meta_
      [ name_ "viewport",
        content_ "width=device-width, initial-scale=1, maximum-scale=1",
        charset_ "utf-8"
      ]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "bootstrap.min.css"]
    body_ $ do
      nav_ [class_ "navbar navbar-expand-md navbar-light bg-light"] $
        do
          div_ [class_ "container-fluid"] $ do
            a_
              [ class_ "d-flex flex-row align-items-center navbar-brand",
                href_ $ fst3 welcomeL
              ]
              $ do
                img_ [src_ "/emblem_color.svg", width_ "50"]
                p_ [class_ "my-0 mx-2 d-none d-md-block"] "LIONS Achern Mitgliederbereich"
                p_ [class_ "my-0 mx-2 d-none d-sm-block d-md-none"] "Mitgliederbereich"
            button_
              [ class_ "navbar-toggler",
                type_ "button",
                data_ "bs-toggle" "collapse",
                data_ "bs-target" "#navbarSupportedContent",
                controls_ "navbarSupportedContent",
                expanded_ "false",
                label_ "Navigation öffnen oder schließen"
              ]
              $ span_ [class_ "navbar-toggler-icon"] ""
            div_ [class_ "collapse navbar-collapse", id_ "navbarSupportedContent"] $
              div_ [class_ "navbar-nav"] $ mapM_ f [welcomeL, loginL, eventL]
      div_ [class_ "py-3 content"] pageContent
  where
    f :: NavLink -> Html ()
    f (href, label, route) =
      let classes = Text.unwords $ ["nav-link"] ++ if route == activeNavLink then ["active"] else []
       in a_ [class_ classes, href_ href] $ toHtml label

app :: Wai.Application
app req send =
  case Wai.pathInfo req of
    [] ->
      send
        . Wai.responseLBS status200 [("Content-Type", "text/html; charset=UTF-8")]
        . renderBS
        . layout Welcome "Startseite"
        $ div_ "what"
    _ -> send $ Wai.responseBuilder status404 [("Content-Type", "text/plain; charset=UTF-8")] "Nicht gefunden"

sessionMiddleware ::
  SQLite.Connection ->
  ClientSession.Key ->
  Log.FastLogger ->
  Wai.Middleware
sessionMiddleware conn key log nextApp req send = do
  case getSessionId of
    -- Redirect to login if not already on login
    Nothing -> nextApp req send
    Just id -> do
      session <- SQLite.execute conn "SELECT expires,userid FROM session WHERE id = ?" [id]
      -- Check if session is expired
      log $ Log.toLogStr $ show session
      nextApp req send
  where
    getSessionId =
      Wai.requestHeaders req & lookup "Cookie"
        >>= lookup "lions_session" . Cookie.parseCookies
        >>= ClientSession.decrypt key

{-
   Read cookie, if none found redirect to login
   Cookie has session ID -> lookup in DB -> if expired redirect to login

   On login -> write session to DB and store in cookie

   Session Key must be unique hence encryption or strong random
-}

main :: IO ()
main =
  Log.withFastLogger
    (Log.LogStdout Log.defaultBufSize)
    ( \log -> do
        sqlitePath <- getEnv "LIONS_SQLITE_PATH"
        sessionKey <- ClientSession.getKeyEnv "LIONS_SESSION_KEY"
        SQLite.withConnection
          sqlitePath
          ( \conn -> do
              SQLite.execute_ conn "PRAGMA foreign_keys"
              log $ "Running server at port: " <> Log.toLogStr (3000 :: Int) <> "\n"
              run 3000
                . sessionMiddleware conn sessionKey log
                . logStdout
                . staticPolicy (addBase "public")
                $ app
          )
    )
