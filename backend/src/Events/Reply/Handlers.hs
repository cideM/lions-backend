module Events.Reply.Handlers
  ( post,
  )
where

import qualified App
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Events.Event.Id as Event
import Events.Reply.Reply (Reply (..))
import qualified Events.Reply.Reply as Reply
import Network.HTTP.Types (status303)
import qualified Network.Wai as Wai
import qualified Session.Auth as Auth
import Text.Read (readEither)
import qualified User.Session as User
import qualified User.User as User
import Wai (parseParams)

post ::
  ( MonadIO m,
    MonadReader env m,
    MonadThrow m,
    App.HasDb env
  ) =>
  Wai.Request ->
  (Wai.Response -> m a) ->
  Event.Id ->
  Auth.Authenticated ->
  m a
post req send eventid@(Event.Id eid) auth = do
  let User.Session {..} = Auth.get' auth

  (_, User.Profile {userEmail = userEmail}) <-
    User.get sessionUserId >>= \case
      Nothing -> throwString [i|"no user for userid from session #{sessionUserId}"|]
      Just v -> return v

  (coming, numberOfGuests) <- liftIO $ parseParams'

  -- This is the core branching in this handler, everything else is just
  -- necessary edge cases and parsing inputs. The "Nothing" case is for when
  -- the user doesn't want commit to either coming or not coming, which I model
  -- as having no reply. The "Just" case is then either yes or no, which
  -- doesn't matter. What matters is that I now store that repl.
  case coming of
    Nothing -> Reply.delete eventid sessionUserId
    Just yesno ->
      Reply.upsert eventid $
        (Reply yesno userEmail sessionUserId numberOfGuests)

  send $ Wai.responseLBS status303 [("Location", encodeUtf8 [i|/veranstaltungen/#{eid}|])] mempty
  where
    -- 10 lines of code that say two params are required
    parseParams' :: IO (Maybe Bool, Int)
    parseParams' = do
      params <- parseParams req

      let replyParam = Map.findWithDefault "" "reply" params
          numGuestsParam = Map.findWithDefault "" "numberOfGuests" params

      case parseComing replyParam of
        Left e -> throwString $ Text.unpack e
        Right coming -> case parseNumGuests numGuestsParam of
          Left e -> throwString $ Text.unpack e
          Right numGuests -> return (coming, numGuests)

    parseComing :: Text -> Either Text (Maybe Bool)
    parseComing "coming" = Right $ Just True
    parseComing "notcoming" = Right $ Just False
    parseComing "noreply" = Right Nothing
    parseComing s = Left [i|unknown coming value '#{s :: Text.Text}'|]

    parseNumGuests :: Text -> Either Text Int
    parseNumGuests "" = Right 0
    parseNumGuests s =
      case readEither (Text.unpack s) of
        Left e -> Left [i|couldn't parse '#{s}' as number: #{e}|]
        Right i' -> Right i'
