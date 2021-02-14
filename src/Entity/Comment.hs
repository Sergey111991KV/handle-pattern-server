module Entity.Comment where



import qualified Data.Attoparsec.ByteString.Char8 as A
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.Types (PGArray(PGArray))

import Entity.Auth 
import Entity.ImportLibrary
import Entity.ParseRowEntity


data Comment =
  Comment
    { idComment :: Int
    , textComments :: Text
    , dataCreateComments :: UTCTime
    , newsIdComments :: Int
    , usersIdComments :: UserId
    }
  deriving (Eq, Show, Generic)

instance FromField Comment where
  fromField f mb = fromPGRow' parseComment f mb

instance FromRow Comment

instance ToRow Comment

instance ToField Comment where
  toField = toJSONField

instance ToJSON Comment

instance FromJSON Comment

parseComment :: A.Parser Comment
parseComment = do
  _ <- A.char '('
  idC <- textContent
  _ <- A.char ','
  text <- textContent
  _ <- A.char ','
  dataC <- textContent
  _ <- A.char ','
  newId <- textContent
  _ <- A.char ','
  userId <- textContent
  _ <- A.char ')'
  pure
    (Comment
       (read $ unpack idC)
       text
       (timeFromByteString dataC)
       (read $ unpack newId)
       (UserId $ read $ unpack userId))

instance FromJSON (PGArray Comment)

instance ToJSON (PGArray Comment)

deriving instance
         Generic (PGArray Comment) => Generic (PGArray Comment)
