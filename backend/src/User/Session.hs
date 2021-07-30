module User.Session (Session(..)) where

import User.Role.Role (Role)
import qualified User.Id

-- There's some overlap with the Session type defined in Session.Session. That
-- type represents the session data retrieved from the DB based on the ID
-- stored client side. This Session here, on the other hand, is the user info
-- associated with that session. I think it's fine to separate them, but
-- Session has a confusing name. It's closer to a special type to marshal and
-- unmarshal to and from a database. This Sessino is closer to the actual
-- (user) session, at least in spirit.
-- Also, this type is called Session because it's expected to be used as a
--
-- qualified import, as in User.Session
data Session = Session
  { sessionUserId :: User.Id.Id,
    sessionUserRoles :: [Role]
  }
  deriving (Show, Eq)
