module Entity.User  where

import ClassyPrelude
    ( Eq, Show, Applicative((<*>)), Generic, Text, (<$>), UTCTime )
import Database.PostgreSQL.Simple.FromField
    ( fromJSONField, FromField(..) ) 
import Entity.Auth ( IsAdmin, IsAuthor, Login, Password, UserId ) 
import Entity.ImportLibrary
    ( FromJSON,
      ToJSON,
      field,
      toJSONField,
      FromRow(..),
      ToField(..),
      ToRow(..) )

data User =
  User
    { idUser :: UserId
    , nameUser :: Text
    , lastName :: Text
    , userLogin :: Login
    , userPassword :: Password
    , avatar :: Text
    , dataCreate :: UTCTime
    , userIsAdmin :: IsAdmin
    , userIsAuthor :: IsAuthor
    }
  deriving (Show, Eq, Generic)

instance FromJSON User

instance ToJSON User

instance FromField User where
  fromField u = fromJSONField u

instance ToField User where
  toField u = toJSONField u

instance FromRow User where
  fromRow =
    User <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*>
    field <*>
    field

instance ToRow User where
  toRow u =
    [ toField (idUser u)
    , toField (nameUser u)
    , toField (lastName u)
    , toField (userLogin u)
    , toField (userPassword u)
    , toField (avatar u)
    , toField (dataCreate u)
    , toField (userIsAdmin u)
    , toField (userIsAuthor u)
    ]
