module Login.Test where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Database.SQLite.Simple as SQLite
import Env (Environment (..))
import Helpers (withDB, withFormRequest)
import Login.Login (login)
import Network.Wai.Test
import Test.Tasty
import Test.Tasty.HUnit
import qualified Web.ClientSession as ClientSession

tests :: TestTree
tests = do
  testGroup
    "Login.login"
    [ testCase "render form with message if email is missing" $ do
        withDB $ \conn -> do
          (_, sessionKey) <- ClientSession.randomKey
          out <- T.decodeUtf8 . B.concat . LB.toChunks . simpleBody <$> withFormRequest "" (\r s -> do login conn sessionKey Production r s)
          T.isInfixOf "Ungültige Kombination aus Email und Passwort" out @?= True,
      testCase "render form with message if password is missing" $ do
        withDB $ \conn -> do
          (_, sessionKey) <- ClientSession.randomKey
          out <- T.decodeUtf8 . B.concat . LB.toChunks . simpleBody <$> withFormRequest "email=foo@bar.com" (\r s -> do login conn sessionKey Production r s)
          T.isInfixOf "Ungültige Kombination aus Email und Passwort" out @?= True,
      testCase "render form with message if credentials don't match" $ do
        withDB $ \conn -> do
          (_, sessionKey) <- ClientSession.randomKey
          SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"
          out <- T.decodeUtf8 . B.concat . LB.toChunks . simpleBody <$> withFormRequest "email=foo@bar.com&password=foo" (\r s -> do login conn sessionKey Production r s)
          T.isInfixOf "Ungültige Kombination aus Email und Passwort" out @?= True,
      testCase "foo" $ do
        withDB $ \conn -> do
          (_, sessionKey) <- ClientSession.randomKey
          SQLite.execute_ conn "insert into users (password_digest, email) values ('$2y$04$cZQmyFjUahmyZnojYpM4rOhwWI629ulZKI2Un92/ysvovfMnzN2/e', 'foo@bar.com')"
          out <- withFormRequest "email=foo@bar.com&password=foobar" (\r s -> do login conn sessionKey Production r s)
          let body = T.decodeUtf8 . B.concat . LB.toChunks $ simpleBody out
          T.isInfixOf "Ungültige Kombination aus Email und Passwort" body @?= False
        -- TODO: simpleHeaders -> Network.HTTP.Types.Header -> hCookie -> Web.Cookie parseCookies
    ]

-- in (runSession session $ \r send -> handler r send) >>= return . lb2st . simpleBody
