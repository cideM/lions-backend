module Main where

import Prelude hiding (add)
import Web.Event.EventTarget as ET
import Data.String (joinWith)
import Web.Event.Event as Event
import Effect (Effect)
import Data.Array (length, filter, partition)
import Web.HTML (window)
import Effect.Exception (throw)
import Web.DOM.DOMTokenList (add, remove)
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (toElement)
import Data.Traversable (for_, traverse)
import Web.DOM.Element as Element
import Web.DOM.NodeList (toArray)
import Data.Maybe (maybe, Maybe(..))
import Web.HTML.HTMLInputElement as InputElement
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.DOM.Node (Node, toEventTarget)

data ListState
  = AllSelected
  | NotAllSelected
  | NoneSelected

getEmailState :: Element.Element -> Effect { state :: ListState, uncheckedEmails :: Array String, checkedEmails :: Array String }
getEmailState root = do
  itemsState <- getCheckboxes root >>= traverse getCheckboxState
  let
    state = getState (length itemsState) (filter _.checked itemsState # length)

    { no: no, yes: yes } = partition _.checked itemsState
  pure { state: state, uncheckedEmails: map _.email no, checkedEmails: map _.email yes }
  where
  getState total checked
    | checked == total = AllSelected
    | checked == 0 = NoneSelected
    | otherwise = NotAllSelected

  getCheckboxState :: Node -> Effect { email :: String, checked :: Boolean }
  getCheckboxState checkbox = do
    checkboxAsEl <- maybe (throw "couldn't convert checkbox to element") pure $ Element.fromNode checkbox
    checkbox' <- maybe (throw "couldn't convert checkbox to input element") pure $ InputElement.fromNode checkbox
    isChecked <- InputElement.checked checkbox'
    email <- Element.getAttribute "data-email" checkboxAsEl >>= maybe (throw "no data-email attr") pure
    pure { email: email, checked: isChecked }

getCheckboxes :: Element.Element -> Effect (Array Node)
getCheckboxes root = querySelectorAll (QuerySelector "[data-email]") (Element.toParentNode root) >>= toArray

setAllCheckboxes :: Boolean -> Element.Element -> Effect Unit
setAllCheckboxes newValue root = do
  checkboxes <- getCheckboxes root
  for_ checkboxes
    $ \el -> do
        checkbox <- maybe (throw "couldn't convert to input element") pure $ InputElement.fromNode el
        InputElement.setChecked newValue checkbox

activateAll :: Element.Element -> Effect Unit
activateAll = setAllCheckboxes true

deactivateAll :: Element.Element -> Effect Unit
deactivateAll = setAllCheckboxes false

buttonOff :: Element.Element -> Effect Unit
buttonOff el =
  Element.classList el
    >>= \list -> do
        add list "text-primary"
        remove list "text-muted"

buttonOn :: Element.Element -> Effect Unit
buttonOn el =
  Element.classList el
    >>= \list -> do
        remove list "text-primary"
        add list "text-muted"

setMails :: Array String -> Element.Element -> Effect Unit
setMails [] btn = do
  Element.setAttribute "href" "" btn
  Element.classList btn >>= \list -> add list "disabled"

setMails mails btn = do
  Element.setAttribute "href" ("mailto:" <> joinWith "," mails) btn
  Element.classList btn >>= \list -> remove list "disabled"

onClickToggleButton :: Element.Element -> Element.Element -> Element.Element -> Event.Event -> Effect Unit
onClickToggleButton root toggleButton mailToButton =
  const
    $ do
        { state: state, uncheckedEmails: no, checkedEmails: yes } <- getEmailState root
        case state of
          AllSelected -> do
            deactivateAll root
            buttonOn toggleButton
            setMails [] mailToButton
          NotAllSelected -> do
            activateAll root
            buttonOff toggleButton
            setMails (yes <> no) mailToButton
          NoneSelected -> do
            activateAll root
            buttonOff toggleButton
            setMails (yes <> no) mailToButton

onClickCheckbox :: Element.Element -> Element.Element -> Element.Element -> Event.Event -> Effect Unit
onClickCheckbox root toggleButton mailToButton =
  const
    $ do
        { state: state, checkedEmails: yes } <- getEmailState root
        case state of
          AllSelected -> do
            buttonOff toggleButton
          _ -> do
            buttonOn toggleButton
        setMails yes mailToButton

initializeUserEmailFeature :: Element.Element -> Effect Unit
initializeUserEmailFeature root = do
  listItems <- getCheckboxes root
  mailToButton <- qs "#email-button" $ Element.toParentNode root
  case mailToButton of
    Nothing -> pure unit
    Just mailToButton' -> do
      toggleButton <- qsOrThrow "toggle mail to button not found" "#toggle-email-button" $ Element.toParentNode root
      toggleListener <- ET.eventListener $ onClickToggleButton root toggleButton mailToButton'
      _ <- ET.addEventListener (Event.EventType "click") toggleListener false (Element.toEventTarget toggleButton)
      checkboxListener <- ET.eventListener $ onClickCheckbox root toggleButton mailToButton'
      for_ listItems (ET.addEventListener (Event.EventType "click") checkboxListener false <<< toEventTarget)
      pure unit
  where
  qs = querySelector <<< QuerySelector

  qsOrThrow msg sel el = qs sel el >>= maybe (throw msg) pure

main :: Effect Unit
main = do
  htmlDoc <- window >>= Window.document
  body <- HTMLDocument.body htmlDoc >>= maybe (throw "body not found") pure
  initializeUserEmailFeature $ toElement body
