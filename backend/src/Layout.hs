module Layout
  ( ActiveNavLink (..),
    layout,
    expanded_,
    infoBox,
    describedBy_,
    warning,
    LayoutStub (..),
    success,
    ariaLabel_,
    ariaCurrent_,
    ariaLabelledBy_,
    ariaHidden_,
  )
where

import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid
import Lucid.Base (makeAttribute)
import qualified User.Id
import qualified User.Session

data LayoutStub = LayoutStub
  { layoutStubTitle :: Text,
    layoutStubContent :: Html ()
  }
  deriving (Show)

data ActiveNavLink = Welcome | Events | Members | Login | Profile deriving (Eq, Show)

expanded_,
  describedBy_,
  ariaLabel_,
  ariaCurrent_,
  ariaLabelledBy_,
  ariaHidden_,
  ariaControls_ ::
    Text -> Attribute
expanded_ = makeAttribute "aria-expanded"
ariaControls_ = makeAttribute "aria-controls"
ariaCurrent_ = makeAttribute "aria-current"
describedBy_ = makeAttribute "aria-describedby"
ariaLabel_ = makeAttribute "aria-label"
ariaLabelledBy_ = makeAttribute "aria-labelledby"
ariaHidden_ = makeAttribute "aria-hidden"

layout :: User.Session.Authentication -> Maybe ActiveNavLink -> LayoutStub -> Html ()
layout auth activeNavLink (LayoutStub {layoutStubTitle = pageTitle, layoutStubContent = pageContent}) =
  doctype_ *> do
    html_ [lang_ "de-DE"] $ do
      head_ $ do
        title_ $ toHtml pageTitle
        meta_
          [ name_ "viewport",
            content_ "width=device-width, initial-scale=1, maximum-scale=1",
            charset_ "utf-8"
          ]
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/style.css"]
        -- https://github.com/chrisdone/lucid/issues/30
        body_ $ do
          nav_ [class_ "navbar navbar-expand-md navbar-light bg-light"] $
            do
              div_ [class_ "container-fluid"] $ do
                a_
                  [ class_ "d-flex flex-row align-items-center navbar-brand",
                    href_ "/"
                  ]
                  $ do
                    img_ [src_ "/emblem_color.svg", width_ "50"]
                    p_ [class_ "my-0 mx-2 d-none d-md-block"] "LIONS Achern Mitgliederbereich"
                    p_ [class_ "my-0 mx-2 d-none d-sm-block d-md-none"] "Mitgliederbereich"
                button_
                  [ class_ "navbar-toggler",
                    type_ "button",
                    data_ "bs-toggle" "collapse",
                    data_ "bs-target" "#navbarSupportedContent",
                    ariaControls_ "navbarSupportedContent",
                    expanded_ "false",
                    label_ "Navigation öffnen oder schließen"
                  ]
                  $ span_ [class_ "navbar-toggler-icon"] ""
                div_ [class_ "collapse navbar-collapse", id_ "navbarSupportedContent"] $
                  div_ [class_ "navbar-nav"] $
                    mapM_
                      f
                      ( [ ("/", "Startseite", Just Welcome),
                          ("/veranstaltungen", "Veranstaltungen", Just Events),
                          ("/nutzer", "Mitglieder", Just Members),
                          ("/login", "Login", Just Login)
                        ]
                          ++ ( case User.Session.get auth of
                                 Nothing -> []
                                 Just User.Session.Session {..} ->
                                   let (User.Id.Id uid) = sessionUserId
                                    in [([i|/nutzer/#{uid}|], "Mein Profil", Just Profile)]
                             )
                      )
          div_ [class_ "py-4 content"] pageContent
          script_ [src_ "/index.js"] ("" :: Text.Text)
          script_ [src_ "/bootstrap.bundle.min.js"] ("" :: Text.Text)
  where
    f :: (Text, Text, Maybe ActiveNavLink) -> Html ()
    f (href, label, route) =
      let classes = Text.unwords $ "nav-link" : (["active text-decoration-underline" | route == activeNavLink])
       in a_ [class_ classes, href_ href] $ toHtml label

success :: Text -> Html ()
success msg =
  div_ [class_ "container p-3 d-flex justify-content-center"] $
    div_ [class_ "row col-md-6"] $ do
      p_ [class_ "alert alert-success", role_ "alert"] $ toHtml (msg :: Text)

warning :: Text -> Html ()
warning msg =
  div_ [class_ "container p-3 d-flex justify-content-center"] $
    div_ [class_ "row col-md-6"] $ do
      p_ [class_ "alert alert-danger mb-4", role_ "alert"] $ toHtml (msg :: Text)

-- Size is hardcoded, can consider S M L
infoBox :: Html () -> Html ()
infoBox msg = do
  div_ [class_ "d-flex align-items-center border rounded p-2"] $ do
    infoCircle
    p_ [class_ "ms-3 my-0"] msg
  where
    infoCircle =
      toHtmlRaw
        ( [i|
      <svg style="min-width: 24px; min-height: 24px;" xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-info-circle" viewBox="0 0 16 16">
        <path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14zm0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16z"/>
        <path d="m8.93 6.588-2.29.287-.082.38.45.083c.294.07.352.176.288.469l-.738 3.468c-.194.897.105 1.319.808 1.319.545 0 1.178-.252 1.465-.598l.088-.416c-.2.176-.492.246-.686.246-.275 0-.375-.193-.304-.533L8.93 6.588zM9 4.5a1 1 0 1 1-2 0 1 1 0 0 1 2 0z"/>
      </svg>
      |] ::
            Text
        )
