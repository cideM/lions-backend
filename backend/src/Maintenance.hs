module Maintenance (middleware) where

import qualified App
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Layout (LayoutStub (..), layout)
import Lucid
import Network.HTTP.Types (status503)
import qualified Network.Wai as Wai
import qualified User.Session
import qualified Wai

-- When maintenance mode is enabled (see LIONS_MAINTENANCE), every request is
-- answered with a 503 and a short notice. This middleware must run after the
-- static file middleware so that the stylesheet and logo referenced by the
-- notice are still served.
middleware ::
  ( MonadIO m,
    App.HasMaintenanceMode env,
    MonadReader env m
  ) =>
  Wai.MiddlewareT m
middleware next req send = do
  enabled <- asks App.getMaintenanceMode
  if enabled
    then
      send
        . Wai.responseLBS
          status503
          [ ("Content-Type", "text/html; charset=UTF-8"),
            ("Retry-After", "3600"),
            ("Cache-Control", "no-store")
          ]
        . renderBS
        . layout User.Session.notAuthenticated Nothing
        . LayoutStub "Wartungsarbeiten"
        $ div_ [class_ "container p-3 d-flex justify-content-center"] $
          div_ [class_ "row col-md-6"] $ do
            h1_ [class_ "h4 mb-3"] "Wartungsarbeiten"
            p_ [class_ "alert alert-secondary", role_ "alert"] $ do
              "Der Mitgliederbereich wird gerade gewartet und ist vorübergehend nicht erreichbar. "
              "Bitte versuche es später noch einmal."
    else next req send
