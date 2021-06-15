module Session.Test where

import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Data.Vault.Lazy as Vault
import qualified Database.SQLite.Simple as SQLite
import Helpers (withDB, withoutLogging)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Session (middleware)
import Test.Tasty
import Test.Tasty.HUnit
import Time (timeDaysFromNow)
import qualified Web.ClientSession as ClientSession
import Web.Cookie

testApp :: Application
testApp _ respond = do
  respond $
    responseLBS
      status200
      [("Content-Type", "text/plain")]
      "Hello, Web!"

tests :: TestTree
tests = testGroup "Session" [middelwareTests]

middelwareTests :: TestTree
middelwareTests =
  testGroup
    "middleware"
    [ testCase "returns 200 if cookie has valid session" $ do
        withDB $ \conn -> do
          withoutLogging $ \logger -> do
            sessionDataVaultKey <- Vault.newKey
            (_, sessionKey) <- ClientSession.randomKey
            let testApp' = middleware logger sessionDataVaultKey conn sessionKey testApp
                rawId = "pppppppppppppppppppppppp" :: T.Text
            encryptedSessionId <- ClientSession.encryptIO sessionKey $ T.encodeUtf8 rawId
            date <- timeDaysFromNow 30
            SQLite.execute_ conn "insert into users (id, password_digest, email) values (1, 'foo', 'foo@bar.com')"
            SQLite.execute conn "insert into sessions (key, expires, userid) values (?, ?, 1)" (rawId, date)
            SQLite.execute_ conn "insert into roles (id, label) values (0, 'admin')"
            SQLite.execute_ conn "insert into user_roles (userid, roleid) values (1, 0)"
            let cookie = defaultSetCookie {setCookieName = "lions_session", setCookieValue = encryptedSessionId}
                cookieRendered = LBS.toStrict . BSBuilder.toLazyByteString . renderSetCookie $ cookie
                req = defaultRequest {requestHeaders = [(hCookie, cookieRendered)]}
            out <- do
              let session = srequest $ SRequest req ""
              runSession session (\r send -> testApp' r send)
            simpleStatus out @?= status200,
      testCase "returns 302 if session id is invalid" $ do
        withDB $ \conn -> do
          withoutLogging $ \logger -> do
            sessionDataVaultKey <- Vault.newKey
            (_, sessionKey) <- ClientSession.randomKey
            let testApp' = middleware logger sessionDataVaultKey conn sessionKey testApp
            encryptedSessionId <- ClientSession.encryptIO sessionKey "foobar"
            date <- timeDaysFromNow 30
            SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"
            SQLite.execute conn "insert into sessions (key, expires, userid) values (?, ?, 1)" (encryptedSessionId, date)
            let cookie = defaultSetCookie {setCookieName = "lions_session", setCookieValue = "foo"}
                cookieRendered = LBS.toStrict . BSBuilder.toLazyByteString . renderSetCookie $ cookie
                req = defaultRequest {requestHeaders = [(hCookie, cookieRendered)]}
            out <- do
              let session = srequest $ SRequest req ""
              runSession session (\r send -> testApp' r send)
            simpleStatus out @?= status302
    ]
