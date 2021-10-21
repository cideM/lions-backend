module Password.Reset.Test where

import qualified App
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import Helpers
  ( withFormRequest,
    withRender200,
    withTestEnvProd,
  )
import Network.Wai.Test
import Password.Reset.Handlers as ResetHandlers
import Password.Reset.Mail as Mail
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Reset"
    [ testCase "no email body param returns message" $ do
        withTestEnvProd $ \_ -> do
          actual <- simpleBody' <$> withFormRequest "" (withRender200 ResetHandlers.post)
          liftIO $ B.isInfixOf "Email darf nicht leer sein" actual @?= True,
      testCase "email not found returns message" $ do
        withTestEnvProd $ \_ -> do
          out <- simpleBody' <$> withFormRequest "email=foo@bar.com" (withRender200 ResetHandlers.post)
          liftIO $ B.isInfixOf "Diese Email-Adresse ist nicht beim Lions Club Achern registiert" out @?= True,
      testCase "inserts new token in DB and passes that token to send email" $ do
        withTestEnvProd $ \ref -> do
          conn <- asks App.getDb
          liftIO $ SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"

          _ <- withFormRequest "email=foo@bar.com" (withRender200 ResetHandlers.post)

          rows <- liftIO $ SQLite.query_ conn "select token,expires,userid from reset_tokens"

          liftIO $ case rows of
            [(token, _, userid) :: (T.Text, T.Text, Integer)] -> do
              userid @?= 1
              (Just _, Just Mail.Mail {..}) <- readIORef ref
              let (Mail.PlainText content) = mailPlainText
              T.isInfixOf token content @?= True
            r -> assertFailure $ "unexpected DB result: " <> show r,
      testCase "works with email surrounded by whitespace" $ do
        withTestEnvProd $ \ref -> do
          conn <- asks App.getDb
          liftIO $ SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"

          _ <- withFormRequest "email=%20foo@bar.com%20" (withRender200 ResetHandlers.post)

          rows <- liftIO $ SQLite.query_ conn "select token,expires,userid from reset_tokens"

          liftIO $ case rows of
            [(token, _, userid) :: (T.Text, T.Text, Integer)] -> do
              userid @?= 1
              (Just _, Just Mail.Mail {..}) <- readIORef ref
              let (Mail.PlainText content) = mailPlainText
              T.isInfixOf token content @?= True
            r -> assertFailure $ "unexpected DB result: " <> show r,
      testCase "works with different case" $ do
        withTestEnvProd $ \ref -> do
          conn <- asks App.getDb
          liftIO $ SQLite.execute_ conn "insert into users (password_digest, email) values ('foo', 'foo@bar.com')"

          _ <- withFormRequest "email=%20fOO@bar.com%20" (withRender200 ResetHandlers.post)

          rows <- liftIO $ SQLite.query_ conn "select token,expires,userid from reset_tokens"

          liftIO $ case rows of
            [(token, _, userid) :: (T.Text, T.Text, Integer)] -> do
              userid @?= 1
              (Just _, Just Mail.Mail {..}) <- readIORef ref
              let (Mail.PlainText content) = mailPlainText
              T.isInfixOf token content @?= True
            r -> assertFailure $ "unexpected DB result: " <> show r
    ]
  where
    simpleBody' = B.concat . LB.toChunks . simpleBody
