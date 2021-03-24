module Main where

import Prelude hiding (add)
import Web.Event.EventTarget as EventTarget
import Data.String (joinWith)
import Web.Event.Event as Event
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Array (length, filter, partition)
import Web.HTML (window)
import Effect.Exception (throw)
import Web.DOM.DOMTokenList (add, remove)
import Web.HTML.Window as Window
import Web.DOM.Text as Text
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (toParentNode)
import Data.Traversable (for_, traverse)
import Web.DOM.Element as Element
import Web.DOM.Document (createElementNS, createElement, createTextNode)
import Web.DOM.NodeList (toArray)
import Data.Maybe (maybe, Maybe(..))
import Web.HTML.HTMLInputElement as InputElement
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.DOM.Node (appendChild, setTextContent, parentNode, insertBefore, Node, toEventTarget)

data ListState
  = AllSelected
  | NotAllSelected
  | NoneSelected

setAllCheckboxes :: Boolean -> Element.Element -> Effect Unit
setAllCheckboxes newValue root = do
  checkboxes <- querySelectorAll (QuerySelector "li input") (Element.toParentNode root) >>= toArray
  for_ checkboxes
    $ \el -> do
        checkbox <- maybe (throw "couldn't convert to input element") pure $ InputElement.fromNode el
        InputElement.setChecked newValue checkbox

getEmailState :: Element.Element -> Effect { state :: ListState, uncheckedEmails :: Array String, checkedEmails :: Array String }
getEmailState root = do
  listItems <- querySelectorAll (QuerySelector "li") (Element.toParentNode root) >>= toArray
  itemsState <- traverse getLiState listItems
  let
    state = getState (length itemsState) (filter _.checked itemsState # length)

    { no: no, yes: yes } = partition _.checked itemsState
  pure { state: state, uncheckedEmails: map _.email no, checkedEmails: map _.email yes }
  where
  getState total checked
    | checked == total = AllSelected
    | checked == 0 = NoneSelected
    | otherwise = NotAllSelected

  getLiState :: Node -> Effect { email :: String, checked :: Boolean }
  getLiState node = do
    el <- maybe (throw "couldn't convert li node to element") pure $ Element.fromNode node
    checkbox <- querySelector (QuerySelector "input") (Element.toParentNode el) >>= maybe (throw "no checkbox found in list") pure
    checkbox' <- maybe (throw "couldn't convert to input element") pure $ InputElement.fromElement checkbox
    isChecked <- InputElement.checked checkbox'
    checked <- Element.getAttribute "checked" checkbox
    email <- Element.getAttribute "data-email" el >>= maybe (throw "no data-email attr") pure
    pure { email: email, checked: isChecked }

initializeUserEmailFeature :: HTMLDocument.HTMLDocument -> Element.Element -> Effect Unit
initializeUserEmailFeature htmlDoc listRoot = do
  listItems <- querySelectorAll (QuerySelector "li") (Element.toParentNode listRoot) >>= toArray
  let
    doc = HTMLDocument.toDocument htmlDoc
  listRootParent <- parentNode (Element.toNode listRoot) >>= maybe (throw "Users list parent not found") pure
  buttonContainer <- createElement "div" doc
  Element.setAttribute "class" "d-flex justify-content-between align-items-center mb-3" buttonContainer
  insertBefore (Element.toNode buttonContainer) (Element.toNode listRoot) listRootParent
  mailToButton <- makeMailButton doc
  appendChild (Element.toNode mailToButton) (Element.toNode buttonContainer)
  toggleButton <- makeToggleButton doc
  appendChild (Element.toNode toggleButton) (Element.toNode buttonContainer)
  let
    activateAll = setAllCheckboxes true listRoot

    deactivateAll = setAllCheckboxes false listRoot

    setMails [] = do
      Element.setAttribute "href" "" mailToButton
      Element.classList mailToButton >>= \list -> add list "disabled"

    setMails mails = do
      Element.setAttribute "href" ("mailto:" <> joinWith "," mails) mailToButton
      Element.classList mailToButton >>= \list -> remove list "disabled"

    buttonOn = setTextContent "Alle für Email auswählen" (Element.toNode toggleButton)

    buttonOff = setTextContent "Alle abwählen" (Element.toNode toggleButton)

    onClickCheckbox = \_ -> do
      { state: state, uncheckedEmails: no, checkedEmails: yes } <- getEmailState listRoot
      case state of
        AllSelected -> do
          buttonOff
        _ -> do
          buttonOn
      setMails yes

    onClickToggleButton = \_ -> do
      { state: state, uncheckedEmails: no, checkedEmails: yes } <- getEmailState listRoot
      case state of
        AllSelected -> do
          deactivateAll
          buttonOn
          setMails []
        NotAllSelected -> do
          activateAll
          buttonOff
          setMails $ yes <> no
        NoneSelected -> do
          activateAll
          buttonOff
          setMails $ yes <> no
  toggleListener <- EventTarget.eventListener onClickToggleButton
  _ <- EventTarget.addEventListener (Event.EventType "click") toggleListener false (Element.toEventTarget toggleButton)
  checkboxListener <- EventTarget.eventListener onClickCheckbox
  for_ listItems
    ( \li -> do
        wrapper <- createElement "div" doc
        Element.setAttribute "class" "flex-grow-1 d-flex justify-content-end" wrapper
        label <- createElement "label" doc
        Element.setAttribute "class" "p-4" label
        checkbox <- createElement "input" doc
        Element.setAttribute "type" "checkbox" checkbox
        Element.setAttribute "class" "form-check-input me-1" checkbox
        Element.setAttribute "aria-label" "Zur Emailliste hinzufügen" checkbox
        appendChild (Element.toNode checkbox) (Element.toNode label)
        appendChild (Element.toNode label) (Element.toNode wrapper)
        appendChild (Element.toNode wrapper) li
        EventTarget.addEventListener (Event.EventType "click") checkboxListener false (toEventTarget li)
    )
  state <- getEmailState listRoot
  pure unit
  where
  makeToggleButton doc = do
    toggleButton <- createElement "button" doc
    Element.setAttribute "type" "button" toggleButton
    Element.setAttribute "class" "btn btn-sm btn-secondary" toggleButton
    setTextContent "Alle für Email auswählen" (Element.toNode toggleButton)
    pure toggleButton

  makeMailButton doc = do
    mailToButton <- createElement "a" doc
    Element.setAttribute "role" "button" mailToButton
    Element.setAttribute "class" "btn btn-sm btn-primary disabled d-flex align-items-center" mailToButton
    Element.setAttribute "href" "" mailToButton
    svg <- createElementNS (Just "http://www.w3.org/2000/svg") "svg" doc
    Element.setAttribute "width" "16" svg
    Element.setAttribute "height" "16" svg
    Element.setAttribute "fill" "currentColor" svg
    Element.setAttribute "class" "bi bi-envelope me-2" svg
    Element.setAttribute "viewBox" "0 0 16 16" svg
    Element.setAttribute "xmlns" "http://www.w3.org/2000/svg" svg
    path <- createElementNS (Just "http://www.w3.org/2000/svg") "path" doc
    Element.setAttribute "fill-rule" "evenodd" path
    Element.setAttribute "d" "M0 4a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2V4zm2-1a1 1 0 0 0-1 1v.217l7 4.2 7-4.2V4a1 1 0 0 0-1-1H2zm13 2.383-4.758 2.855L15 11.114v-5.73zm-.034 6.878L9.271 8.82 8 9.583 6.728 8.82l-5.694 3.44A1 1 0 0 0 2 13h12a1 1 0 0 0 .966-.739zM1 11.114l4.758-2.876L1 5.383v5.73z" path
    appendChild (Element.toNode path) (Element.toNode svg)
    appendChild (Element.toNode svg) (Element.toNode mailToButton)
    txt <- createTextNode "Email" doc
    appendChild (Text.toNode txt) (Element.toNode mailToButton)
    pure mailToButton

main :: Effect Unit
main = do
  htmlDoc <- window >>= Window.document
  body <- HTMLDocument.body htmlDoc >>= maybe (throw "body not found") pure
  maybeListRoot <- querySelector (QuerySelector "#userslist") (toParentNode body)
  case maybeListRoot of
    Just listRoot -> initializeUserEmailFeature htmlDoc listRoot
    Nothing -> log "no list root element found, skipping"
