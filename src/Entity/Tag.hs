module Entity.Tag  where


import ClassyPrelude
  ( Applicative(pure)
  , Eq
  , Generic
  , Int
  , IsString(fromString)
  , Show
  , String
  , ($)
  , unpack
  )
import qualified Data.Attoparsec.ByteString.Char8 as A
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.Types (PGArray(PGArray))
import Entity.ImportLibrary
import Entity.ParseRowEntity


import qualified Prelude as P

data Tag =
  Tag
    { idTag :: Int
    , nameTag :: String
    }
  deriving (Eq, Show, Generic)

instance FromField Tag where
  fromField f mb = fromPGRow' parseTag f mb

instance FromRow Tag

instance FromJSON Tag

instance ToField Tag where
  toField (Tag idT text) =
    Many
      [ Plain "("
      , Plain (intDec idT)
      , Plain ","
      , Escape (fromString text)
      , Plain ")"
      ]

instance ToRow Tag

instance ToJSON Tag

instance FromJSON (PGArray Tag)

instance ToJSON (PGArray Tag)

deriving instance Generic (PGArray Tag) => Generic (PGArray Tag)

parseTag :: A.Parser Tag
parseTag = do
  _ <- A.char '('
  idTag' <- textContent
  _ <- A.char ','
  nameTag' <- textContent
  _ <- A.char ')'
  pure (Tag (P.read $ ClassyPrelude.unpack idTag') (unpack nameTag'))
