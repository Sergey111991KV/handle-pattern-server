module Web where

import ClassyPrelude
import qualified Network.Wai.Handler.Warp as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import Data.ByteString
import Data.ByteString.Builder
import Control.Monad.Catch 
import Control.Monad.Except
import Control.Monad.Reader    
import Control.Monad.Trans     
import GHC.Generics 
import Data.Aeson
import qualified Data.ByteString.Lazy as BL

import qualified Logger
import qualified Database.ExportDatabase as Database
import Web.Route
import Entity.ExportEntity
import Web.HelpFunction


withHandle
    :: Config  -> Logger.Handle ->  Database.Handle -> (Web.Route.Handle -> IO a) -> IO a
withHandle config  logger dataFunc f =
    f $ Handle config logger dataFunc

newtype App a =
  App
    { unApp :: ReaderT Web.Route.Handle (ExceptT ErrorServer IO) a
    }
  deriving (Applicative, Functor, Monad, MonadReader Web.Route.Handle, MonadIO, MonadThrow, MonadError ErrorServer )

runApp :: Web.Route.Handle -> App a -> IO (Either ErrorServer a)
runApp conf  app =  runExceptT $ runReaderT  (unApp  app) conf


run :: Web.Route.Handle ->  Logger.Handle -> IO ()
run webH logH = do
    HTTP.run (port $ hConfig webH) $  \request respond -> do
      eitherResponse <- runApp webH $ route request
      response <- either (\e -> do
          Logger.hLogError logH "error"
          serverErrorResponse e) ( \response' -> do
            Logger.hLogDebug logH "good" 
            return response'
            ) eitherResponse
      
      respond response










