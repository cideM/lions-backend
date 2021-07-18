module Scrypt.Test where

import qualified Data.ByteString as B
import Scrypt
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Scrypt"
    [ testCase "Creates same hash as Firebase Scrypt" $
        Right "lSrfV15cpx95/sZS2W9c9Kp6i/LVgQNDNC/qzrCnh1SAyZvqmZqAjTdn3aoItz+VHjoZilo78198JAdRuid5lQ=="
          @=? firebaseHashPw userSalt signerKey saltSep memcost rounds "user1password"
    ]

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

