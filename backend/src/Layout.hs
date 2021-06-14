module Layout
  ( ActiveNavLink (..),
    layout,
    expanded_,
    describedBy_,
    warning,
    LayoutStub (..),
    success,
    ariaLabel_,
    ariaLabelledBy_,
    ariaHidden_,
  )
where

import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid
import Lucid.Base (makeAttribute)
import Routes.Data
import User.Domain (UserId (..))

data LayoutStub = LayoutStub
  { layoutStubTitle :: Text,
    layoutStubActiveNavLink :: Maybe ActiveNavLink,
    layoutStubContent :: Html ()
  }
  deriving (Show)

data ActiveNavLink = Welcome | Events | Members | Login | Profile deriving (Eq, Show)

expanded_, describedBy_, ariaLabel_, ariaLabelledBy_, ariaHidden_, ariaControls_ :: Text -> Attribute
expanded_ = makeAttribute "aria-expanded"
ariaControls_ = makeAttribute "aria-controls"
describedBy_ = makeAttribute "aria-describedby"
ariaLabel_ = makeAttribute "aria-label"
ariaLabelledBy_ = makeAttribute "aria-labelledby"
ariaHidden_ = makeAttribute "aria-hidden"

layout :: Authentication -> LayoutStub -> Html ()
layout auth (LayoutStub {layoutStubTitle = pageTitle, layoutStubActiveNavLink = activeNavLink, layoutStubContent = pageContent}) =
  doctype_ *> do
    html_ [lang_ "de-DE"] $ do
      head_ $ do
        title_ $ toHtml pageTitle
        meta_
          [ name_ "viewport",
            content_ "width=device-width, initial-scale=1, maximum-scale=1",
            charset_ "utf-8"
          ]
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/bootstrap.min.css"]
        -- https://github.com/chrisdone/lucid/issues/30
        body_ $ do
          nav_ [class_ "navbar navbar-expand-md navbar-light bg-light"] $
            do
              div_ [class_ "container-fluid"] $ do
                a_
                  [ class_ "d-flex flex-row align-items-center navbar-brand",
                    href_ "/willkommen"
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
                          ++ ( case getUserId auth of
                                 Nothing -> []
                                 Just uid -> [([i|/nutzer/#{uid}|], "Mein Profil", Just Profile)]
                             )
                      )
          div_ [class_ "py-4 content"] pageContent
          script_ [src_ "/index.js"] ("" :: Text.Text)
          script_ [src_ "/bootstrap.bundle.min.js"] ("" :: Text.Text)
  where
    getUserId (IsAuthenticated (IsUser (UserSession {..}))) =
      let (UserId uid) = userSessionUserId
       in Just uid
    getUserId (IsAuthenticated (IsAdmin (AdminUser (UserSession {..})))) =
      let (UserId uid) = userSessionUserId
       in Just uid
    getUserId _ = Nothing
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
