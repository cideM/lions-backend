#! /usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}
{--
   This can't be a Cabal test suite. Because what will happen is that I run the
   tests, and Nix will build my Haskell package, as part of that it'll run
   the tests of that package, which will then cause infinite docker recursion.
   Amazing.

   I could try to disable running just that test automatically or whatever but
   I can also just make it a script or a standalone executable.

   $ chmod +x ./backend/app/Main.hs
   $ ./backend/app/Main.hs

   So what's going on here?

   1. Ask Docker to start a Nix container
   2. Inside the container, build our VM
   3. Run the VM, with ports forwarded from Host -> Docker -> QEMU
   4. Then run tests against the server inside QEMU inside Docker

   The Docker indirection isn't really necessary on a system where QEMU just
   works.

   I'm currently using an ubuntu runner though.
--}

module Main where

import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad
import Control.Retry
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import Network.HTTP.Req
import System.Directory
import System.FilePath
import qualified System.Process as Proc
import Test.Tasty
import Test.Tasty.HUnit
import Turtle hiding ((</>))

buildServer :: IO Proc.ProcessHandle
buildServer = do

runServer' :: IO Proc.ProcessHandle
runServer' = do
  home <- getHomeDirectory
  let sshDir = home </> ".ssh"

  pwd <- getCurrentDirectory

  Proc.spawnProcess
    "docker"
    [ "run",
      "-t",
      "-p",
      "127.0.0.1:81:8081",
      "-p",
      "127.0.0.1:80:8080",
      "--rm",
      "-v",
      "nixcache2:/nix",
      "-v",
      pwd ++ ":/foo",
      "-w",
      "/foo",
      "-v",
      sshDir ++ ":/root/.ssh:ro",
      "nixpkgs/nix-flakes",
      "bash",
      "-c",
      "nix build .#vm && QEMU_NET_OPTS=hostfwd=tcp::2221-:22,hostfwd=tcp::8080-:80,hostfwd=tcp::8081-:443 ./result/bin/run-lions-server-vm"
    ]

httpOptions =
  defaultHttpConfig
    { httpConfigRetryJudgeException = \_ _ -> True,
      -- One minute in microseconds * number of minutes
      httpConfigRetryPolicy = constantDelay 60000000 <> limitRetries 20
    }

waitForServer :: IO ()
waitForServer = do
  _ <- runReq httpOptions $ do
    result <- req GET (http "localhost" /: "login") NoReqBody bsResponse (port 80)
    let code = (responseStatusCode result :: Int)
    return code
  pure ()

main :: IO ()
main = do
  {--
  Using Turtle's async facilities doesn't work here. Seems to be:
  https://github.com/Gabriel439/Haskell-Turtle-Library/issues/103

  runServer :: IO ExitCode
  runServer = proc "cabal" ["v2-run", "--cabal-file", "./backend/lions-backend.cabal", "run-lions-backend"] empty

  asyncProcess <- fork runServer
  ...
  cancel asyncProcess

  ^--- Does not work

  Also you need -threaded or else fork won't work at all.
  --}
  bracket runServer' Proc.interruptProcessGroupOf $ \_ -> do
    waitForServer
    defaultMain test

test = testCase "Should show login page" $ do
  code <- runReq defaultHttpConfig $ do
    result <- req GET (http "localhost" /: "login") NoReqBody bsResponse (port 80)
    return (responseStatusCode result :: Int)
  code @?= 200 
