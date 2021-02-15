module Entity.Auth where


import Database.PostgreSQL.Simple.FromField
import Entity.ImportLibrary
import ClassyPrelude            

type IsAdmin = Bool

type IsAuthor = Bool

newtype SessionId =
  SessionId
    { sessionRaw :: Text
    }
  deriving (Generic, Show, Eq, Ord)

instance FromField SessionId where
  fromField f mb_bytestring = SessionId <$> fromField f mb_bytestring

instance FromRow SessionId where
  fromRow = SessionId <$> field

instance FromJSON SessionId

instance ToJSON SessionId

instance ToRow SessionId

instance ToField SessionId where
  toField sess = toField $ sessionRaw sess

newtype UserId =
  UserId
    { userIdRaw :: Int
    }
  deriving (Generic, Show, Eq, Ord)

instance FromField UserId where
  fromField f mb_bytestring = UserId <$> fromField f mb_bytestring

instance FromRow UserId where
  fromRow = UserId <$> field

instance FromJSON UserId

instance ToJSON UserId

instance ToRow UserId

instance ToField UserId where
  toField idU = toField $ userIdRaw idU

newtype Login =
  Login
    { loginRaw :: Text
    }
  deriving (Generic, Show, Eq, Ord)

instance FromField Login where
  fromField f mb_bytestring = Login <$> fromField f mb_bytestring

instance FromRow Login where
  fromRow = Login <$> field

instance FromJSON Login

instance ToJSON Login

instance ToRow Login

instance ToField Login where
  toField login = toField $ loginRaw login

newtype Password =
  Password
    { passwordRaw :: Text
    }
  deriving (Generic, Show, Eq, Ord)

instance FromField Password where
  fromField f mb_bytestring = Password <$> fromField f mb_bytestring

instance FromRow Password where
  fromRow = Password <$> field

instance FromJSON Password

instance ToJSON Password

instance ToRow Password

instance ToField Password where
  toField password = toField $ passwordRaw password
