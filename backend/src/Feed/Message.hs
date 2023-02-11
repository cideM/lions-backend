module Feed.Message where

import Data.Text (Text)
import qualified Data.Time as Time
import Prelude hiding (id)
import qualified Text.HTML.SanitizeXSS as SanitizeXSS
import qualified CMarkGFM
import Lucid

newtype Id = Id Int deriving (Show)

data Message = Message Id Text Time.UTCTime deriving (Show)

render :: Text -> Html ()
render = toHtmlRaw . SanitizeXSS.sanitize . CMarkGFM.commonmarkToHtml [] [CMarkGFM.extAutolink]
