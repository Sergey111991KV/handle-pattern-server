module Web.Route where








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



