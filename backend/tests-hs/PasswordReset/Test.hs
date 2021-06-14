module PasswordReset.Test where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Database.SQLite.Simple as SQLite
import Helpers (as200, withDB, withFormRequest, withQueryString, withoutLogging)
import Network.AWS.SES.SendEmail
import Network.URI.Encode (decode)
import Network.Wai.Test
import PasswordReset.PasswordReset (handleChangePw, handleReset)
import Test.Tasty
import Test.Tasty.HUnit
import Time.Time (timeDaysFromNow)

tests :: TestTree
tests =
  testGroup "PasswordReset" $
    [ testGroup
        "handleReset"
        [ testCase "no email body param returns message" $ do
            withDB $ \conn -> do
              out <- simpleBody' <$> withFormRequest "" (\r s -> handleReset conn r sendMail >>= s . as200)
              B.isInfixOf "Email darf nicht leer sein" out @?= True,
          testCase "email not found returns message" $ do
            withDB $ \conn -> do
              out <-
                simpleBody'
                  <$> withFormRequest
                    "email=foo@bar.com"
                    (\r s -> handleReset conn r sendMail >>= s . as200)
              B.isInfixOf "Diese Email-Adresse ist nicht beim Lions Club Achern registiert" out @?= True,
          testCase "inserts new token in DB and passes that token to send email" $ do
            withDB $ \conn -> do
              tokenRef <- newIORef ""
              SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"
              let sendMail' = sendMailWithRef tokenRef
              _ <- withFormRequest "email=foo@bar.com" (\r s -> handleReset conn r sendMail' >>= s . as200)
              rows <- SQLite.query_ conn "select token,expires,userid from reset_tokens"
              case rows of
                [(token, _, userid) :: (T.Text, T.Text, Integer)] -> do
                  userid @?= 1
                  tokenRefValue <- readIORef tokenRef
                  tokenRefValue @?= token
                r -> assertFailure $ "unexpected DB result: " <> show r
        ],
      testGroup
        "handleChangePw"
        [ testCase "changes password in database with valid token" $ do
            withDB $ \conn -> do
              withoutLogging $ \logger -> do
                date <- timeDaysFromNow 30
                SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"
                SQLite.execute conn "insert into reset_tokens (token, userid, expires) values ('tokenvaluetokenvaluetoken', 1, ?)" [date]
                out <-
                  withFormRequest
                    "token=tokenvaluetokenvaluetoken&inputPassword=newpw&inputPasswordMatch=newpw"
                    (\r s -> handleChangePw logger conn r >>= s . as200)
                let body = T.decodeUtf8 $ simpleBody' out
                    needle = "Password erfolgreich geändert"
                assertBool
                  [i|Expected '#{needle}' in response but got: #{body}|]
                  (T.isInfixOf needle body)
                rows <- SQLite.query_ conn "select password_digest from users"
                case rows of
                  ([[pw :: T.Text]]) -> do
                    assertBool "Password should not be old password" $ pw /= "newpw"
                  r -> assertFailure $ "unexpected DB result: " <> show r
        ]
    ]
  where
    simpleBody' = B.concat . LB.toChunks . simpleBody

sendMailWithRef :: IORef T.Text -> T.Text -> T.Text -> IO SendEmailResponse
sendMailWithRef ref _ token = do
  writeIORef ref (T.pack . decode $ T.unpack token)
  return $ sendEmailResponse 1 "foo"

sendMail :: T.Text -> T.Text -> IO SendEmailResponse
sendMail _ _ = do
  return $ sendEmailResponse 1 "foo"
