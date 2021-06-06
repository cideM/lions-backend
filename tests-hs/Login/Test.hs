module Login.Test where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Database.SQLite.Simple as SQLite
import Env (Environment (..))
import Helpers (withDB, withFormRequest)
import Login.Login (login)
import Network.HTTP.Types (status302)
import Network.HTTP.Types.Header
import Network.Wai.Test
import Test.Tasty
import Test.Tasty.HUnit
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
              (_, sessionKey) <- ClientSession.randomKey
              out <- T.decodeUtf8 . B.concat . LB.toChunks . simpleBody <$> withFormRequest "" (\r s -> do login conn userSalt signerKey saltSep sessionKey Production r s)
              T.isInfixOf "Ungültige Kombination aus Email und Passwort" out @?= True,
          testCase "render form with message if password is missing" $ do
            withDB $ \conn -> do
              (_, sessionKey) <- ClientSession.randomKey
              out <- T.decodeUtf8 . B.concat . LB.toChunks . simpleBody <$> withFormRequest "email=foo@bar.com" (\r s -> do login conn userSalt signerKey saltSep sessionKey Production r s)
              T.isInfixOf "Ungültige Kombination aus Email und Passwort" out @?= True,
          testCase "render form with message if credentials don't match" $ do
            withDB $ \conn -> do
              (_, sessionKey) <- ClientSession.randomKey
              SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"
              out <- T.decodeUtf8 . B.concat . LB.toChunks . simpleBody <$> withFormRequest "email=foo@bar.com&password=foo" (\r s -> do login conn userSalt signerKey saltSep sessionKey Production r s)
              T.isInfixOf "Ungültige Kombination aus Email und Passwort" out @?= True,
          testCase "returns encrypted session cookie upon successful login" $ do
            withDB $ \conn -> do
              (_, sessionKey) <- ClientSession.randomKey
              SQLite.execute_ conn "insert into users (password_digest, email) values ('$2y$04$NFwlwssLnLtvJEwZ0XtXgOjAHPqUIDHZfd2CiZsVDgmk1NTrdwT1a', 'foo@bar.com')"
              out <- withFormRequest "email=foo@bar.com&password=foobar" (\r s -> do login conn userSalt signerKey saltSep sessionKey Production r s)
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
              (_, sessionKey) <- ClientSession.randomKey
              SQLite.execute_ conn "insert into users (password_digest, email) values ('lSrfV15cpx95/sZS2W9c9Kp6i/LVgQNDNC/qzrCnh1SAyZvqmZqAjTdn3aoItz+VHjoZilo78198JAdRuid5lQ==', 'foo@bar.com')"
              out <- withFormRequest "email=foo@bar.com&password=user1password" (\r s -> do login conn userSalt signerKey saltSep sessionKey Production r s)
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