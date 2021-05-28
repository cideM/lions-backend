{-# LANGUAGE OverloadedStrings #-}

-- import Control.Lens
import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy (toStrict)
import qualified Database.SQLite.Simple as SQLite
import Lucid
import Network.AWS.SES.SendEmail
import Network.HTTP.Types (hContentEncoding, status200)
import qualified Network.Wai as Wai
import Network.Wai.Test
import PasswordReset.Handlers (handleReset)
import Scrypt
import System.FilePattern.Directory
-- import System.IO
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, passwordResetTests]

signerKey :: B.ByteString
signerKey = "jxspr8Ki0RYycVU8zykbdLGjFQ3McFUH0uiiTvC8pVMXAn210wjLNmdZJzxUECKbm0QsEmYUSDzZvpjeJ9WmXA=="

userSalt :: B.ByteString
userSalt = "42xEC+ixf3L2lw=="

saltSep :: B.ByteString
saltSep = "Bw=="

memcost :: Integer
memcost = 14

rounds :: Integer
rounds = 8

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "Scrypt" $
        Right "lSrfV15cpx95/sZS2W9c9Kp6i/LVgQNDNC/qzrCnh1SAyZvqmZqAjTdn3aoItz+VHjoZilo78198JAdRuid5lQ=="
          @=? firebaseHashPw userSalt signerKey saltSep memcost rounds "user1password"
    ]

withDB :: (SQLite.Connection -> IO b) -> IO b
withDB f = do
  migrations <- getDirectoryFiles "migrations" ["*.up.sql"]
  SQLite.withConnection
    ":memory:"
    ( \conn -> do
        forM_ migrations (\m -> readFile ("./migrations/" <> m) >>= SQLite.execute_ conn . SQLite.Query . T.pack)
        f conn
    )

sendMail :: (Monad m) => p1 -> p2 -> m SendEmailResponse
sendMail _ _ =
  return $ sendEmailResponse 1 "foo"

makeApp :: (Monad m) => (t -> m (Html a)) -> t -> (Wai.Response -> m b) -> m b
makeApp handler req send =
  handler req >>= \html ->
    send
      . Wai.responseLBS status200 [("Content-Type", "text/html; charset=UTF-8")]
      $ renderBS html

-- lazy BS to strict T
lb2st :: LB.ByteString -> T.Text
lb2st = T.decodeUtf8 . B.concat . LB.toChunks

passwordResetTests :: TestTree
passwordResetTests =
  testGroup
    "handlers"
    [ testCase "handleReset email not found" $ do
        withDB $ \conn -> do
          let req = defaultRequest {Wai.requestHeaders = [(hContentEncoding, "application/x-www-form-urlencoded")]}
          let session = srequest $ SRequest req "email=foo@bar.com"
              app = makeApp (\r -> handleReset conn r sendMail)
          out <- lb2st . simpleBody <$> runSession session app
          T.isInfixOf "Diese Email-Adresse ist nicht beim Lions Club Achern registiert" out @=? True
    ]
