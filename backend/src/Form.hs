module Form (notEmpty, processField, validDate, FormFieldState (..), fieldIsValid) where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Time.Format as TF
import Locale (german)
import qualified Data.String
import qualified Data.Text as Text

data FormFieldState a = NotValidated | Valid a | Invalid Text deriving (Show, Eq)

classValidated :: [(Text, Bool)] -> Text
classValidated pairs = Text.intercalate " " [className | (className, predicate) <- pairs, predicate]

processField :: FormFieldState a -> (Text, Maybe Text)
processField field = (makeInputClass field, getErrorMsg field)

getErrorMsg :: FormFieldState a -> Maybe Text
getErrorMsg (Invalid msg) = Just msg
getErrorMsg _ = Nothing

fieldIsValid :: FormFieldState a -> Bool
fieldIsValid (Valid _) = True
fieldIsValid _ = False

makeInputClass :: FormFieldState a -> Text
makeInputClass field = classValidated [("form-control", True), ("is-invalid", isJust $ getErrorMsg field), ("is-valid", fieldIsValid field)]

notEmpty :: (Eq a, Data.String.IsString a) => a -> FormFieldState a
notEmpty "" = Invalid "Feld darf nicht leer sein"
notEmpty v = Valid v

validDate :: TF.ParseTime a => Text -> FormFieldState a
validDate "" = Invalid "Feld darf nicht leer sein"
validDate datestr =
  case TF.parseTimeM True german "%d.%m.%Y %R" $ Text.unpack datestr of
    Nothing -> Invalid "Falsches Format"
    Just date -> Valid date
