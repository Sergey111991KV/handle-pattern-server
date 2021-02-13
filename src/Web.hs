module Web where

import qualified Network.Wai.Handler.Warp as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import Entity.ErrorServer
import qualified Data.Text                as T
import Data.ByteString
import Data.ByteString.Builder
import Log
import Control.Monad.Catch 
import Control.Monad.Except
import Control.Monad.Reader    
import Control.Monad.Trans     




data Config = Config {
    port :: Int
    }



newtype App a =
  App
    { unApp :: ReaderT Config (ExceptT ErrorServer IO) a
    }
  deriving (Applicative, Functor, Monad, MonadReader Config, MonadIO, MonadThrow, MonadError ErrorServer )

runApp :: Config -> App a -> IO (Either ErrorServer a)
runApp conf  app =  runExceptT $ runReaderT  (unApp  app) conf



run :: Config -> IO ()
run config = do
    HTTP.run (port config) $  \request respond -> do
      eitherResponse <- runApp config $ route request
      response <- either (\e -> do
          serverErrorResponse e) pure eitherResponse
      respond response

















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

-- successResponse :: forall a. ToJSON a  => a  -> HTTP.Response
-- successResponse  b = HTTP.responseBuilder HTTP.status200 [("Content-Type", "application/json")] $ fromEncoding $ toEncoding  b

successResponse' :: ByteString -> HTTP.Response
successResponse'  b = HTTP.responseBuilder HTTP.status200 [("Content-Type", "application/json")] $ byteString  b

route :: Monad m
  => HTTP.Request -> m HTTP.Response
route req = do
    case methodAndPath req of   
            GET  ["auth","exit"] -> do
                return $ successResponse'   "auth exit" 


