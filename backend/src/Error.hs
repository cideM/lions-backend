module Error
  ( withExceptT',
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

-- Why does this work? If you change "m a" back to ExceptT blabla it fails
-- TODO: Research
note' :: MonadError e m => e -> Maybe a -> m a
note' a = liftEither . note a

