module Entity.Author  where

import Database.PostgreSQL.Simple.FromField (FromField(..), fromJSONField)
import Entity.Auth (UserId)
import Entity.ImportLibrary
    ( FromJSON,
      ToJSON,
      field,
      toJSONField,
      FromRow(..),
      ToField(..),
      ToRow(..) )
import ClassyPrelude
    ( Eq, Show, Applicative((<*>)), Generic, Int, Text, (<$>) )  

data Author =
  Author
    { idAuthor :: Int
    , idLinkUser :: UserId
    , description :: Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON Author

instance ToJSON Author

instance FromField Author where
  fromField u = fromJSONField u

instance ToField Author where
  toField u = toJSONField u

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field

instance ToRow Author where
  toRow auth =
    [ toField (idAuthor auth)
    , toField (idLinkUser auth)
    , toField (description auth)
    ]
