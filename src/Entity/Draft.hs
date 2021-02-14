{-# OPTIONS_GHC -fno-warn-orphans #-}

module Entity.Draft where


import Entity.ImportLibrary 

import qualified Database.PostgreSQL.Simple.Types as P

data Draft =
  Draft
    { idDraft :: Int
    , textDraft :: Text
    , dataCreateDraft :: UTCTime
    , newsIdDraft :: Maybe Int
    , mainPhotoUrl :: Text
    , shortNameDraft :: Text
    , otherPhotoUrl :: P.PGArray Text
    , tagsId :: P.PGArray Int
    , idAuthorDraft :: Int
    }
  deriving (Show, Generic, Ord, Eq)

instance FromRow Draft

instance ToRow Draft

instance FromJSON Draft

instance ToJSON Draft

instance FromJSON (P.PGArray Text)

instance ToJSON (P.PGArray Text)

deriving instance
         Generic (P.PGArray Text) => Generic (P.PGArray Text)

instance FromJSON (P.PGArray Int)

instance ToJSON (P.PGArray Int)

deriving instance
         Generic (P.PGArray Int) => Generic (P.PGArray Int)

instance FromJSON (P.PGArray Draft)

instance ToJSON (P.PGArray Draft)

deriving instance
         Generic (P.PGArray Draft) => Generic (P.PGArray Draft)
