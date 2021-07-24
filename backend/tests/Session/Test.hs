module Session.Test where

import qualified App
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Database.SQLite.Simple as SQLite
import Helpers
  ( runSession',
    withTestEnvProd,
  )
import qualified Katip as K
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Session.Middleware (middleware)
import Test.Tasty
import Test.Tasty.HUnit
import Time (timeDaysFromNow)
import qualified Wai.Class as Wai
import qualified Web.ClientSession as ClientSession
import Web.Cookie

testApp :: Wai.ApplicationT m
testApp _ respond = do
  respond $
    responseLBS
      status200
      [("Content-Type", "text/plain")]
      "Hello, Web!"

tests :: TestTree
tests = testGroup "Session" [middlewareTests]

middlewareTests :: TestTree
middlewareTests =
  testGroup
    "middleware"
    [ testCase "returns 200 if cookie has valid session" $ do
        withTestEnvProd $ \_ -> do
          conn <- asks App.getDb
          sessionKey <- asks App.getSessionEncryptionKey

          let testApp' = middleware testApp
              rawId = "pppppppppppppppppppppppp" :: T.Text

          encryptedSessionId <- liftIO $ ClientSession.encryptIO sessionKey $ T.encodeUtf8 rawId
          date <- liftIO $ timeDaysFromNow 30

          liftIO $ SQLite.execute_ conn "insert into users (id, password_digest, email) values (1, 'foo', 'foo@bar.com')"
          liftIO $ SQLite.execute conn "insert into sessions (key, expires, userid) values (?, ?, 1)" (rawId, date)
          liftIO $ SQLite.execute_ conn "insert into roles (id, label) values (0, 'admin')"
          liftIO $ SQLite.execute_ conn "insert into user_roles (userid, roleid) values (1, 0)"

          let cookie = defaultSetCookie {setCookieName = "lions_session", setCookieValue = encryptedSessionId}
              cookieRendered = LBS.toStrict . BSBuilder.toLazyByteString . renderSetCookie $ cookie
              req = defaultRequest {requestHeaders = [(hCookie, cookieRendered)]}

          out <- do
            let session = srequest $ SRequest req ""
            runSession' session testApp'

          liftIO $ simpleStatus out @?= status200,
      testCase "returns 302 if session id is invalid" $ do
        withTestEnvProd $ \_ -> do
          K.katipNoLogging $ do
            conn <- asks App.getDb
            sessionKey <- asks App.getSessionEncryptionKey

            let testApp' = middleware testApp

            encryptedSessionId <- liftIO $ ClientSession.encryptIO sessionKey "foobar"
            date <- liftIO $ timeDaysFromNow 30

            liftIO $ SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"
            liftIO $ SQLite.execute conn "insert into sessions (key, expires, userid) values (?, ?, 1)" (encryptedSessionId, date)

            let cookie = defaultSetCookie {setCookieName = "lions_session", setCookieValue = "foo"}
                cookieRendered = LBS.toStrict . BSBuilder.toLazyByteString . renderSetCookie $ cookie
                req = defaultRequest {requestHeaders = [(hCookie, cookieRendered)]}

            out <- do
              let session = srequest $ SRequest req ""
              runSession' session testApp'

            liftIO $ simpleStatus out @?= status302
    ]
