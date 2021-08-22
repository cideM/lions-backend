module Main where

{--
   This can't be a Cabal test suite. Because what will happen is that I run the
   tests, and Nix will build my Haskell package, as part of that it'll run
   the tests of that package, which will then cause infinite docker recursion.
   Amazing.

   This does not work on MacOS because I can't build the NixOS VM on MacOS and
   I don't want to do it with Docker because then how do you get the VM out of
   the Docker container?

   The weird CI flag exists because I think interruptProcessGroupOf is killing
   the CI runner itself in GitHub Actions. It works perfectly fine locally and
   I'm too tired of all this shit to figure this out.
--}

import Control.Exception.Safe
import Control.Retry
import Network.HTTP.Req
import System.Environment
import System.Exit
import qualified System.Process as Proc
import Test.Tasty
import Test.Tasty.HUnit

waitForServer :: IO ()
waitForServer = do
  let httpOptions =
        defaultHttpConfig
          { httpConfigRetryJudgeException = \_ _ -> True,
            httpConfigRetryPolicy = constantDelay 1000000 <> limitRetries 300
          }

  _ <- runReq httpOptions $ do
    result <- req GET (http "localhost" /: "login") NoReqBody bsResponse (port 8080)
    return (responseStatusCode result :: Int)

  pure ()

main :: IO ()
main = do
  testEnv <- getEnv "TEST_ENV"

  let runner = do
        buildHandle <- Proc.spawnProcess "nix" ["build", ".#vm"]

        exitCodeBuild <- Proc.waitForProcess buildHandle

        print ("Done building VM" :: String)

        case exitCodeBuild of
          (ExitFailure _) -> throwString "couldn't build server"
          _ -> do
            currentEnv <- getEnvironment

            let run = Proc.proc "./result/bin/run-lions-server-vm" []
                qemuNetOpts = "hostfwd=tcp::2221-:22,hostfwd=tcp::8080-:80,hostfwd=tcp::8081-:443"
                run' = run {Proc.env = Just (("QEMU_NET_OPTS", qemuNetOpts) : currentEnv)}

            (_, _, _, runHandle) <- Proc.createProcess run'
            return runHandle

  exitCode <-
    bracket
      runner
      ( \h ->
          if testEnv == "ci"
            then return ()
            else Proc.interruptProcessGroupOf h
      )
      ( const $ do
          waitForServer
          tests `catch` (\testErr -> return $ if testErr == ExitSuccess then 0 else 1)
      )

  if exitCode == 0
    then do
      print ("Success" :: String)
      exitWith ExitSuccess
    else do
      print ("Failure" :: String)
      exitWith $ ExitFailure exitCode

tests :: IO Int
tests = do
  defaultMain test
  return 0
  where
    test =
      testCase "Should show login page" $ do
        code <- runReq defaultHttpConfig $ do
          result <- req GET (http "localhost" /: "login") NoReqBody bsResponse (port 8080)
          return (responseStatusCode result :: Int)
        code @?= 200
