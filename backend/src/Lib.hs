module Lib (main) where

import qualified Server

main :: IO ()
main = Server.run
