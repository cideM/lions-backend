{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PasswordResetTests where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import Helpers (withDB, withFormRequest, as200)
import Network.AWS.SES.SendEmail
import Network.URI.Encode (decode)
import PasswordReset (handleReset)
import Test.Tasty
import Network.Wai.Test
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "PasswordReset.handleReset"
    [ testCase "no email body param" $ do
        withDB $ \conn -> do
          out <- B.concat . LB.toChunks . simpleBody <$> withFormRequest "" (\r s -> handleReset conn r sendMail >>= s . as200)
          B.isInfixOf "Email darf nicht leer sein" out @=? True,
      testCase "email not found" $ do
        withDB $ \conn -> do
          out <- B.concat . LB.toChunks . simpleBody <$> withFormRequest "email=foo@bar.com" (\r s -> handleReset conn r sendMail >>= s . as200)
          B.isInfixOf "Diese Email-Adresse ist nicht beim Lions Club Achern registiert" out @=? True,
      testCase "inserts new token in DB and passes that token to send email" $ do
        withDB $ \conn -> do
          tokenRef <- newIORef ""
          SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"
          let sendMail' = sendMailWithRef tokenRef
          _ <- B.concat . LB.toChunks . simpleBody <$> withFormRequest "email=foo@bar.com" (\r s -> handleReset conn r sendMail' >>= s . as200)
          rows <- SQLite.query_ conn "select token,expires,userid from reset_tokens"
          case rows of
            [(token, _, userid) :: (T.Text, T.Text, Integer)] -> do
              userid @=? 1
              tokenRefValue <- readIORef tokenRef
              tokenRefValue @=? token
            r -> assertFailure $ "unexpected DB result: " <> show r
    ]

sendMailWithRef :: IORef T.Text -> T.Text -> T.Text -> IO SendEmailResponse
sendMailWithRef ref _ token = do
  writeIORef ref (T.pack . decode $ T.unpack token)
  return $ sendEmailResponse 1 "foo"

sendMail :: T.Text -> T.Text -> IO SendEmailResponse
sendMail _ _ = do
  return $ sendEmailResponse 1 "foo"
