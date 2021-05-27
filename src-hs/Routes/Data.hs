{-# LANGUAGE OverloadedStrings #-}

module Routes.Data
  ( UserSession (..),
    Authentication (..),
    AdminUser (..),
    Authenticated (..),
  )
where

-- import Session.Domain (SessionId)
import User.Domain (Role, UserId)

-- userSessionSessionId :: SessionId,
data UserSession = UserSession
  { userSessionUserId :: UserId,
    userSessionUserRoles :: [Role]
  }
  deriving (Show, Eq)

data Authentication = IsNotAuthenticated | IsAuthenticated Authenticated deriving (Show, Eq)

data Authenticated = IsUser UserSession | IsAdmin AdminUser deriving (Show, Eq)

newtype AdminUser = AdminUser UserSession deriving (Show, Eq)
