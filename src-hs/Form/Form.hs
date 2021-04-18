{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Form.Form (processField, FormFieldState (..)) where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text

data FormFieldState a = NotValidated | Valid a | Invalid Text deriving (Show)

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
