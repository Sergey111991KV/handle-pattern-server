module Database.DatabaseCommon where

import Database.ImportDatabase
import ClassyPrelude 


data Config = Config
    { configUrl ::  ByteString
    , configStripeCount :: Int
    , configMaxOpenConnPerStripe :: Int
    , configIdleConnTimeout :: NominalDiffTime
    } deriving (Show)


withPool :: Config -> (Pool Connection -> IO a)  -> IO a
withPool cfg  = Database.ImportDatabase.bracket initPool cleanPool 
  where
    initPool =
      createPool
        openConn
        closeConn
        (configStripeCount cfg)
        (configIdleConnTimeout cfg)
        (configMaxOpenConnPerStripe cfg)
    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (configUrl cfg)
    closeConn = close



data Handle = Handle
    { hConfig :: Config
    , hPool   :: Pool Connection
    }

withHandle :: Config -> (Database.DatabaseCommon.Handle -> IO a) -> IO a
withHandle cfg action = do
     withPool cfg $ \pool -> do
        -- migrate state
        action $ Handle cfg pool


type PG  m
   = (Monad m, MonadIO m, MonadError ErrorServer m)

withConn :: PG  m => Pool Connection -> (Connection -> IO a) -> m a
withConn pool action = do
  liftIO . withResource pool $ \conn -> action conn







