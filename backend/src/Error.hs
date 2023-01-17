module Error
  ( note',
    module Control.Error,
    module Control.Exception.Safe,
    module Control.Monad.Except,
  )
where

import Control.Error hiding (tryIO, tryJust)
import Control.Exception.Safe
import Control.Monad.Except

note' :: MonadError e m => e -> Maybe a -> m a
note' a = liftEither . note a
