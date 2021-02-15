module Database where


import Data.Aeson 
import Data.Pool 
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Control.Monad.Catch
import Data.Time
import Data.Has
import Control.Monad.Catch 
import Control.Monad.Except
import Control.Monad.Reader    
import Control.Monad.Trans 
import ClassyPrelude
import Entity.ExportEntity


data Config = Config
    { configUrl ::  ByteString
    , configStripeCount :: Int
    , configMaxOpenConnPerStripe :: Int
    , configIdleConnTimeout :: NominalDiffTime
    } deriving (Show)


withPool :: Config -> (Pool Connection -> IO a)  -> IO a
withPool cfg  = Control.Monad.Catch.bracket initPool cleanPool 
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

withHandle :: Config -> (Database.Handle -> IO a) -> IO a
withHandle cfg action = do
     withPool cfg $ \pool -> do
        -- migrate state
        action $ Handle cfg pool


type PG  m
   = (Monad m, MonadIO m, MonadError ErrorServer m)

withConn :: PG  m => Pool Connection -> (Connection -> IO a) -> m a
withConn pool action = do
  liftIO . withResource pool $ \conn -> action conn


createUser :: PG  m => Database.Handle ->  User -> m ()
createUser h user = do
  
        let qUser = "INSERT INTO usernews (name_user, lastname , login_user , password_user , avatar_user , datacreate_user , admin , authoris)  VALUES (?,?,?,?,?,?,?,?);"
        result <- withConn (hPool h) $ \conn ->  execute conn qUser
            ( nameUser user
            , lastName user
            , userLogin user
            , userPassword user
            , avatar user
            , dataCreate user
            , userIsAdmin user
            , userIsAuthor user)
        case result of
            1 -> do
                return  ()
            _ -> do
                throwError DataErrorPostgreSQL




