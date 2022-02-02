module Feed.Message.Test where

import qualified Feed.Message as Message
import Lucid
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Message.render"
    [ testCase "Converts empty lines into paragraphs" $
        "<p>foo\nbar</p>\n" @=? renderText (Message.render "foo\r\nbar"),
      testCase "Converts URLs" $
        "<p><a href=\"http://www.google.de\">http://www.google.de</a></p>\n" @=? renderText (Message.render "http://www.google.de"),
      testCase "Does not recognize colon as a URL" $
        "<p>foo:</p>\n" @=? renderText (Message.render "foo:")
    ]
