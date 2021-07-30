module Session.Valid
  ( unvalid,
    save,
    Valid,
    parse,
    create,
  )
where

import qualified App
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQLite
import qualified Error as E
import Network.Wai.Session (genSessionId)
import Session.Session (Id (..), Session (..))
import Time (timeDaysFromNow)
import qualified User.Id as User

create :: (MonadIO m) => User.Id -> m Valid
create uid = do
  expires <- liftIO $ timeDaysFromNow 30
  sessionid <- liftIO $ decodeUtf8 <$> genSessionId
  return . Valid $ Session (Id sessionid) expires uid

parse :: (E.MonadError Text m, MonadIO m) => Session -> m Valid
parse s@(Session _ expires _) = do
  now <- liftIO $ Time.getCurrentTime
  if now >= expires
    then E.throwError $ ([i|session expired at: #{expires}|] :: Text)
    else return $ Valid s

-- A wrapper around a potentially invalid session so that I can differentiate
-- the two possible session types through types. I should not export the
-- constructor actually.  TODO: Hide constructor
newtype Valid = Valid Session deriving (Show)

unvalid :: Valid -> Session
unvalid (Valid s) = s

save ::
  ( MonadIO m,
    App.HasDb env,
    MonadReader env m
  ) =>
  Valid ->
  m ()
save (Valid (Session (Id key) expires (User.Id uid))) = do
  conn <- asks App.getDb
  liftIO $ SQLite.execute conn "INSERT INTO sessions (key,expires,userid) VALUES (?,?,?)" (key, expires, uid)
