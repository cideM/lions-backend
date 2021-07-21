module Password.Change.Test where

import qualified App
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Database.SQLite.Simple as SQLite
import Helpers
  ( renderLayoutStub200,
    withFormRequest,
    withQueryString,
    withRender200,
    withTestEnvProd,
  )
import Network.AWS.SES.SendEmail
import Network.URI.Encode (decode)
import Network.Wai.Test
import Password.Change.Handlers as ChangeHandlers
import Test.Tasty
import Test.Tasty.HUnit
import Time (timeDaysFromNow)

tests :: TestTree
tests =
  testGroup
    "Change"
    [ testCase "changes password in database with valid token" $ do
        withTestEnvProd $ \_ -> do
          conn <- asks App.getDb

          date <- liftIO $ timeDaysFromNow 30

          liftIO $
            SQLite.execute_
              conn
              "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"

          liftIO $
            SQLite.execute
              conn
              "insert into reset_tokens (token, userid, expires) values ('tokenvaluetokenvaluetoken', 1, ?)"
              [date]

          out <- withFormRequest "token=tokenvaluetokenvaluetoken&inputPassword=newpw&inputPasswordMatch=newpw" (withRender200 ChangeHandlers.post)

          let body = T.decodeUtf8 $ simpleBody' out
              needle = "Password erfolgreich geändert"

          liftIO $
            assertBool
              [i|Expected '#{needle}' in response but got: #{body}|]
              (T.isInfixOf needle body)

          rows <- liftIO $ SQLite.query_ conn "select password_digest from users"

          liftIO $ case rows of
            ([[pw :: T.Text]]) -> do
              assertBool "Password should not be old password" $ pw /= "newpw"
            r -> assertFailure $ "unexpected DB result: " <> show r
    ]
  where
    simpleBody' = B.concat . LB.toChunks . simpleBody
