module Events.Event where

import Data.Text (Text)
import qualified Data.Time as Time
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Form (FormFieldState (..), notEmpty, validDate)
import GHC.Generics

newtype EventTitle = EventTitle Text
  deriving (Show, Eq, Ord, Generic)
  deriving (FromField, ToField) via Text

unEventTitle :: EventTitle -> Text
unEventTitle (EventTitle s) = s

newtype EventDate = EventDate Time.UTCTime
  deriving (Show, Eq, Ord, Generic)
  deriving (FromField, ToField) via Time.UTCTime

unEventDate :: EventDate -> Time.UTCTime
unEventDate (EventDate s) = s

newtype EventDescription = EventDescription Text
  deriving (Show, Eq, Ord, Generic)
  deriving (FromField, ToField) via Text

unEventDescription :: EventDescription -> Text
unEventDescription (EventDescription s) = s

newtype EventLocation = EventLocation Text
  deriving (Show, Eq, Ord, Generic)
  deriving (FromField, ToField) via Text

unEventLocation :: EventLocation -> Text
unEventLocation (EventLocation s) = s

newtype EventID = EventID Integer
  deriving (Show, Eq, Ord, Generic)
  deriving (FromField, ToField) via Integer

unEventID :: EventID -> Integer
unEventID (EventID s) = s

type Event = (EventID, EventTitle, EventDate, Bool, EventLocation, EventDescription)

getEventDate :: Event -> EventDate
getEventDate (_, _, date, _, _, _) = date

data FormState = FormState
  { createEventStateTitle :: FormFieldState Text,
    createEventStateDate :: FormFieldState Time.UTCTime,
    createEventStateLocation :: FormFieldState Text,
    createEventStateDescription :: FormFieldState Text
  }
  deriving (Show, Eq, Ord, Generic)

data FormInput = FormInput
  { createEventInputTitle :: Text,
    createEventInputDate :: Text,
    createEventInputLocation :: Text,
    createEventInputDescription :: Text,
    createEventInputFamilyAllowed :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

emptyForm :: FormInput
emptyForm =
  FormInput
    { createEventInputTitle = "",
      createEventInputDate = "",
      createEventInputLocation = "",
      createEventInputDescription = "",
      createEventInputFamilyAllowed = False
    }

emptyState :: FormState
emptyState = FormState NotValidated NotValidated NotValidated NotValidated

-- makeEvent knows how to construct an event from the input data of the htmlFor
-- form.
makeEvent :: FormInput -> Either FormState (EventTitle, EventDate, Bool, EventLocation, EventDescription)
makeEvent input =
  case FormState
    (notEmpty $ createEventInputTitle input)
    (validDate $ createEventInputDate input)
    (notEmpty $ createEventInputLocation input)
    (Valid $ createEventInputDescription input) of
    FormState (Valid title) (Valid date) (Valid location) (Valid description) ->
      Right $
        ( EventTitle title,
          EventDate date,
          (createEventInputFamilyAllowed input),
          EventLocation location,
          EventDescription description
        )
    state -> Left state

data ReplyBox = Own | Yes | No deriving (Show, Eq)
