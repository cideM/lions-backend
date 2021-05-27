module App (Environment(..), parseEnv) where

import Control.Exception.Safe

data Environment = Production | Development deriving (Show, Eq)

parseEnv :: String -> IO Environment
parseEnv "production" = return Production
parseEnv "development" = return Development
parseEnv s = throwString $ "unknown environment: " <> show s
