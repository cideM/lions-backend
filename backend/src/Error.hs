module Error
  ( withExceptT',
    withExceptT'',
    note',
    module Control.Error,
    module Control.Exception.Safe,
    module Control.Monad.Except,
  )
where

import Control.Error hiding (tryIO, tryJust)
import Control.Exception.Safe
import Control.Monad.Except

withExceptT' :: MonadError e m => (e' -> e) -> ExceptT e' m a -> m a
withExceptT' f = either (throwError . f) return <=< runExceptT

withExceptT'' :: MonadError e m => (e' -> e) -> Either e' a -> m a
withExceptT'' f = either (throwError . f) return <=< runExceptT . liftEither

note' :: MonadError e m => e -> Maybe a -> m a
note' a = liftEither . note a
