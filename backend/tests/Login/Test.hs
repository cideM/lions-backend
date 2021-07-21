module Login.Test where

import qualified App
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Database.SQLite.Simple as SQLite
import Helpers (withFormRequest, withTestEnvProd)
import qualified Login.Login as Login
import Network.HTTP.Types (status302)
import Network.HTTP.Types.Header
import Network.Wai.Test
import Test.Tasty
import Test.Tasty.HUnit
import qualified Web.ClientSession as ClientSession
import Web.Cookie

userSalt :: B.ByteString
userSalt = "42xEC+ixf3L2lw=="

tests :: TestTree
tests = do
  testGroup
    "Login"
    [ testGroup
        "login"
        [ testCase "render form with message if email is missing" $ do
            withTestEnvProd $ \_ -> do
              sresponse <- withFormRequest "" Login.postLogin
              let actual = T.decodeUtf8 . B.concat . LB.toChunks $ simpleBody sresponse
              liftIO $ T.isInfixOf "Ungültige Kombination aus Email und Passwort" actual @?= True,
          testCase "render form with message if password is missing" $ do
            withTestEnvProd $ \_ -> do
              sresponse <- withFormRequest "email=foo@bar.com" Login.postLogin
              let actual = T.decodeUtf8 . B.concat . LB.toChunks $ simpleBody sresponse
              liftIO $ T.isInfixOf "Ungültige Kombination aus Email und Passwort" actual @?= True,
          testCase "render form with message if credentials don't match" $ do
            withTestEnvProd $ \_ -> do
              conn <- asks App.getDb
              liftIO $ SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"

              sresponse <- withFormRequest "email=foo@bar.com&password=foo" $ Login.postLogin

              let actual = T.decodeUtf8 . B.concat . LB.toChunks $ simpleBody sresponse

              liftIO $ T.isInfixOf "Ungültige Kombination aus Email und Passwort" actual @?= True,
          testCase "returns encrypted session cookie upon successful login" $ do
            withTestEnvProd $ \_ -> do
              conn <- asks App.getDb
              sessionKey <- asks App.getSessionEncryptionKey

              liftIO $ SQLite.execute_ conn "insert into users (password_digest, email) values ('$2y$04$NFwlwssLnLtvJEwZ0XtXgOjAHPqUIDHZfd2CiZsVDgmk1NTrdwT1a', 'foo@bar.com')"

              sresponse <- withFormRequest "email=foo@bar.com&password=foobar" $ Login.postLogin

              liftIO $ simpleStatus sresponse @?= status302

              rows <- liftIO $ SQLite.query_ conn "select key from sessions"

              liftIO $ case rows of
                [[storedSessionId :: T.Text]] -> do
                  cookieHeader <-
                    maybe
                      (assertFailure "expected Set-Cookie header")
                      return
                      (lookup hSetCookie $ simpleHeaders sresponse)
                  sessionCookie <-
                    maybe
                      (assertFailure "expected lions_session cookie")
                      return
                      (lookup "lions_session" $ parseCookiesText cookieHeader)
                  sessionIdFromCookie <-
                    maybe
                      (assertFailure "couldn't decrypt session key")
                      return
                      (ClientSession.decrypt sessionKey (T.encodeUtf8 sessionCookie))
                  storedSessionId @?= T.decodeUtf8 sessionIdFromCookie
                r -> assertFailure $ "unexpected DB result: " <> show r,
          testCase "can also verify firebase passwords" $ do
            withTestEnvProd $ \_ -> do
              conn <- asks App.getDb
              sessionKey <- asks App.getSessionEncryptionKey

              liftIO $ SQLite.execute conn "insert into users (password_digest, salt, email) values ('lSrfV15cpx95/sZS2W9c9Kp6i/LVgQNDNC/qzrCnh1SAyZvqmZqAjTdn3aoItz+VHjoZilo78198JAdRuid5lQ==', ?, 'foo@bar.com')" ["42xEC+ixf3L2lw==" :: T.Text]

              sresponse <- withFormRequest "email=foo@bar.com&password=user1password" Login.postLogin

              liftIO $ simpleStatus sresponse @?= status302

              rows <- liftIO $ SQLite.query_ conn "select key from sessions"

              liftIO $ case rows of
                [[storedSessionId :: T.Text]] -> do
                  cookieHeader <-
                    maybe
                      (assertFailure "expected Set-Cookie header")
                      return
                      (lookup hSetCookie $ simpleHeaders sresponse)
                  sessionCookie <-
                    maybe
                      (assertFailure "expected lions_session cookie")
                      return
                      (lookup "lions_session" $ parseCookiesText cookieHeader)
                  sessionIdFromCookie <-
                    maybe
                      (assertFailure "couldn't decrypt session key")
                      return
                      (ClientSession.decrypt sessionKey (T.encodeUtf8 sessionCookie))
                  storedSessionId @?= T.decodeUtf8 sessionIdFromCookie
                r -> assertFailure $ "unexpected DB result: " <> show r
        ]
    ]
