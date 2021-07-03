module Login.Test where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Database.SQLite.Simple as SQLite
import Scrypt (verifyPassword)
import Env (Environment (..))
import Helpers (withDB, withFormRequest, withoutLogging)
import Login.Login (login)
import Network.HTTP.Types (status302)
import Network.HTTP.Types.Header
import Network.Wai.Test
import qualified Logging
import Test.Tasty
import Test.Tasty.HUnit
import qualified Session
import qualified Web.ClientSession as ClientSession
import Web.Cookie

signerKey :: B.ByteString
signerKey = "jxspr8Ki0RYycVU8zykbdLGjFQ3McFUH0uiiTvC8pVMXAn210wjLNmdZJzxUECKbm0QsEmYUSDzZvpjeJ9WmXA=="

userSalt :: B.ByteString
userSalt = "42xEC+ixf3L2lw=="

saltSep :: B.ByteString
saltSep = "Bw=="

tests :: TestTree
tests = do
  testGroup
    "Login"
    [ testGroup
        "login"
        [ testCase "render form with message if email is missing" $ do
            withDB $ \conn -> do
              withoutLogging $ \logger -> do
                (_, sessionKey) <- ClientSession.randomKey
                let tryLogin' = Session.tryLogin conn (verifyPassword signerKey saltSep) (ClientSession.encryptIO sessionKey)
                out <- T.decodeUtf8 . B.concat . LB.toChunks . simpleBody <$> withFormRequest "" (\r s -> do login logger tryLogin' Production r s)
                T.isInfixOf "Ungültige Kombination aus Email und Passwort" out @?= True,
          testCase "render form with message if password is missing" $ do
            withDB $ \conn -> do
              withoutLogging $ \logger -> do
                (_, sessionKey) <- ClientSession.randomKey
                let tryLogin' = Session.tryLogin conn (verifyPassword signerKey saltSep) (ClientSession.encryptIO sessionKey)
                out <- T.decodeUtf8 . B.concat . LB.toChunks . simpleBody <$> withFormRequest "email=foo@bar.com" (\r s -> do login logger tryLogin' Production r s)
                T.isInfixOf "Ungültige Kombination aus Email und Passwort" out @?= True,
          testCase "render form with message if credentials don't match" $ do
            withDB $ \conn -> do
              withoutLogging $ \logger -> do
                (_, sessionKey) <- ClientSession.randomKey
                let tryLogin' = Session.tryLogin conn (verifyPassword signerKey saltSep) (ClientSession.encryptIO sessionKey)
                SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"
                out <- T.decodeUtf8 . B.concat . LB.toChunks . simpleBody <$> withFormRequest "email=foo@bar.com&password=foo" (\r s -> do login logger tryLogin' Production r s)
                T.isInfixOf "Ungültige Kombination aus Email und Passwort" out @?= True,
          testCase "returns encrypted session cookie upon successful login" $ do
            withDB $ \conn -> do
              withoutLogging $ \logger -> do
                (_, sessionKey) <- ClientSession.randomKey
                SQLite.execute_ conn "insert into users (password_digest, email) values ('$2y$04$NFwlwssLnLtvJEwZ0XtXgOjAHPqUIDHZfd2CiZsVDgmk1NTrdwT1a', 'foo@bar.com')"
                let tryLogin' = Session.tryLogin conn (verifyPassword signerKey saltSep) (ClientSession.encryptIO sessionKey)
                out <- withFormRequest "email=foo@bar.com&password=foobar" (\r s -> do login logger tryLogin' Production r s)
                simpleStatus out @?= status302
                rows <- SQLite.query_ conn "select key from sessions"
                case rows of
                  [[storedSessionId :: T.Text]] -> do
                    cookieHeader <-
                      maybe
                        (assertFailure "expected Set-Cookie header")
                        return
                        (lookup hSetCookie $ simpleHeaders out)
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
            withDB $ \conn -> do
              withoutLogging $ \logger -> do
                (_, sessionKey) <- ClientSession.randomKey
                let tryLogin' = Session.tryLogin conn (verifyPassword signerKey saltSep) (ClientSession.encryptIO sessionKey)
                SQLite.execute conn "insert into users (password_digest, salt, email) values ('lSrfV15cpx95/sZS2W9c9Kp6i/LVgQNDNC/qzrCnh1SAyZvqmZqAjTdn3aoItz+VHjoZilo78198JAdRuid5lQ==', ?, 'foo@bar.com')" ["42xEC+ixf3L2lw==" :: T.Text]
                out <- withFormRequest "email=foo@bar.com&password=user1password" (\r s -> do login logger tryLogin' Production r s)
                simpleStatus out @?= status302
                rows <- SQLite.query_ conn "select key from sessions"
                case rows of
                  [[storedSessionId :: T.Text]] -> do
                    cookieHeader <-
                      maybe
                        (assertFailure "expected Set-Cookie header")
                        return
                        (lookup hSetCookie $ simpleHeaders out)
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

-- TODO: Test with scrypt
