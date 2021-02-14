module Web.Route where


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

import qualified Logger as Logger
import qualified Database as Database
import Entity.ExportEntity








data Config = Config {
    port :: Int
    }  deriving (Show, Generic)

data Handle = Handle
    { hConfig   :: Config
    , hLogger   :: Logger.Handle 
    , hDatabase :: Database.Handle
    }

type HTTPMonad m
   = (Monad m, MonadIO m, MonadError ErrorServer m, MonadReader Handle m)





methodAndPath :: HTTP.Request -> API
methodAndPath req =
  case getMethod of
    HTTP.POST -> POST $ HTTP.pathInfo req
    HTTP.GET -> GET $ HTTP.pathInfo req
    HTTP.PUT -> PUT $ HTTP.pathInfo req
    HTTP.DELETE -> DELETE $ HTTP.pathInfo req
    _ -> UNKNOWN
  where
    getMethod =
      either (error . show) id $ HTTP.parseMethod (HTTP.requestMethod req)

type Router = [T.Text] 

data API
  = POST Router
  | GET Router
  | PUT Router
  | DELETE Router
  | UNKNOWN
  deriving (Show, Eq)

serverErrorResponse :: Monad m => ErrorServer ->  m HTTP.Response
serverErrorResponse err = do
        pure $ HTTP.responseLBS HTTP.status404 [] "Error"
        -- (encodeUtf8 $ fromStrict $ errorText err)

successResponse :: forall a. ToJSON a  => a  -> HTTP.Response
successResponse  b = HTTP.responseBuilder HTTP.status200 [("Content-Type", "application/json")] $ fromEncoding $ toEncoding  b

successResponse' :: ByteString -> HTTP.Response
successResponse'  b = HTTP.responseBuilder HTTP.status200 [("Content-Type", "application/json")] $ byteString  b

route :: HTTPMonad m
  => HTTP.Request -> m HTTP.Response
route req = do
    case methodAndPath req of   
            GET  ["auth","exit"] -> do
                return $ successResponse'   "auth exit" 
            POST ["user"]  -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (user :: User) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ BL.fromStrict reqBody)
                db <- asks hDatabase
                Database.createUser db user
                return $ successResponse'   "create user" 



