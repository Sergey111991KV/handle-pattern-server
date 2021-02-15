module Web.Route where

import ClassyPrelude
import qualified Network.Wai.Handler.Warp as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP
import Data.ByteString.Builder
import Control.Monad.Catch 
import Control.Monad.Except
import Control.Monad.Reader    
import Control.Monad.Trans     

import Data.Aeson
import qualified Data.ByteString.Lazy as BL

import qualified Logger as Logger
import qualified Database.ExportDatabase as DB
import Entity.ExportEntity
import Web.HelpFunction
import qualified Prelude as P

data Config = Config {
    port :: Int
    }  deriving (Show, Generic)

data Handle = Handle
    { hConfig   :: Config
    , hLogger   :: Logger.Handle 
    , hDatabase :: DB.Handle
    }

type HTTPMonad m
   = (Monad m, MonadIO m, MonadError ErrorServer m, MonadReader Web.Route.Handle m)





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

type Router = [Text] 

data API
  = POST Router
  | GET Router
  | PUT Router
  | DELETE Router
  | UNKNOWN
  deriving (Show, Eq)



route :: HTTPMonad m
  => HTTP.Request -> m HTTP.Response
route req = do
    db <- asks hDatabase
    either  (\e -> notAutorized e db) (\sess -> autorized sess db) (getCookie req)
    where
        notAutorized ErrorGetCookie db = do
            case methodAndPath req of   
                GET  ["auth", login, pass] -> do
                    -- db <- asks hDatabase
                    uIdResult <- DB.findUserId db (Login login) (Password pass)
                    newSess <- DB.newSession db uIdResult 
                    setCookie  newSess
                _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""

        notAutorized err _ = serverErrorResponse err
        
        autorized sess db = do 
            case methodAndPath req of   
                GET  ["auth","exit"] -> do
                    DB.findUserIdBySession db sess >>= DB.deleteOldSession db
                    return $ successResponse   ("auth exit" :: Text)
                
                GET  ["publish", idE] -> do
                    let unpackIdEntity = P.read $ unpack  idE :: Int 
                    DB.findUserIdBySession db sess >>= DB.publishNews db unpackIdEntity
                    return $ successResponse   ("publish news" :: Text)

                GET  ["news", "sortedNews", condition ] -> do
                    news <- DB.sortedNews db condition
                    return $ successResponse news 

                -- GET  ["news", "filterAuthor", condition ] -> do
                --     let unpackIdAuthor = BP.read  condition :: Int 
                --     news <- filterAuthor unpackIdAuthor
                --     return $ successResponse news 
                -- GET  ["news", "filterCategory", condition ] -> do
                --     let unpackIdCategory = BP.read  condition :: Int 
                --     news <- filterCategory unpackIdCategory
                --     return $ successResponse news
                -- GET  ["news", "filterOfData", condition , date] -> do
                --     news <- filterOfData condition date
                --     return $ successResponse news 
                -- GET  ["news", "filterOneOfTags", condition ] -> do
                --     news <- filterOneOfTags condition
                --     return $ successResponse news 
                -- GET  ["news", "filterAllOfTags", condition ] -> do
                --     news <- filterAllOfTags condition
                --     return $ successResponse news 
                -- GET  ["news", "filterName", condition ] -> do
                --     news <- filterName condition
                --     return $ successResponse news 
                -- GET  ["news", "filterTag", condition ] -> do
                --     let unpackIdTag = BP.read  condition :: Int 
                --     news <- filterTag unpackIdTag
                --     return $ successResponse news 
                -- GET  ["news", "filterContent", condition ] -> do
                --     news <- filterContent condition
                --     return $ successResponse news 









                POST ["user"]  -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (user :: User) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ BL.fromStrict reqBody)
                    DB.createUser db user
                    return $ successResponse  ("create user"  :: Text)




            -- GET  ["user", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     user <- getOneCommon sess UserEntReq unpackIdEntity
            --     return $ successResponse  (getData  user :: User )
            -- GET  ["users"] -> do
            --     users <- getArrayCommon sess UserEntReq
            --     return $ successResponse  (map getData  users :: [User] )
            -- GET  ["author", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     author <- getOneCommon sess AuthorEntReq unpackIdEntity
            --     return $ successResponse  (getData  author :: Author )
            -- GET  ["authors"] -> do
            --     authors <- getArrayCommon sess AuthorEntReq
            --     return $ successResponse  (map getData  authors :: [Author] )
            -- GET  ["category", idE] -> do   
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     category <- getOneCommon sess CategoryEntReq unpackIdEntity
            --     return $ successResponse  (getData  category :: Category )
            -- GET  ["categorys"] -> do
            --     categorys <- getArrayCommon sess CategoryEntReq
            --     return $ successResponse  (map getData  categorys :: [Category] )
            -- GET  ["comment", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     comment <- getOneCommon sess CommentEntReq unpackIdEntity
            --     return $ successResponse  (getData  comment :: Comment )
            -- GET  ["comments"] -> do
            --     comments' <- getArrayCommon sess CommentEntReq
            --     return $ successResponse  (map getData  comments' :: [Comment] )
            -- GET  ["draft", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     draft <- getOneCommon sess DraftEntReq unpackIdEntity
            --     return $ successResponse  (getData  draft :: Draft )
            -- GET  ["drafts"] -> do
            --     drafts <- getArrayCommon sess DraftEntReq
            --     return $ successResponse  (map getData  drafts :: [Draft] )
            -- GET  ["tag", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     tag <- getOneCommon sess TagEntReq unpackIdEntity
            --     return $ successResponse  (getData  tag :: Tag )
            -- GET  ["tags"] -> do
            --     tags <- getArrayCommon sess TagEntReq
            --     return $ successResponse  (map getData  tags :: [Tag] )
            -- GET  ["news", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     news <- getOneCommon sess NewsEntReq unpackIdEntity
            --     return $ successResponse  (getData  news :: News )
            -- GET  ["news_s"] -> do
            --     news <- getArrayCommon sess NewsEntReq
            --     return $ successResponse  (map getData  news :: [News] )

            -- POST  ["user"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (user :: User) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     createCommon sess (AnEntity user)
            --     return $ successResponse  ("success create user" :: Text)
            -- POST  ["author"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (author :: Author) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     createCommon sess (AnEntity author)
            --     return $ successResponse  ("success create author" :: Text)
            -- POST  ["category"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (category :: Category) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     createCommon sess (AnEntity category)
            --     return $ successResponse  ("success create category" :: Text)
            -- POST  ["comment"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (comment :: Comment) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     createCommon sess (AnEntity comment)
            --     return $ successResponse  ("success create comment" :: Text)
            -- POST  ["draft"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (category :: Draft) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     createCommon sess (AnEntity category)
            --     return $ successResponse  ("success create draft" :: Text)
            -- POST  ["tag"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (comment :: Tag) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     createCommon sess (AnEntity comment)
            --     return $ successResponse  ("success create tag" :: Text)

            -- PUT  ["user"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (user :: User) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     editingCommon sess (AnEntity user)
            --     return $ successResponse  ("success create user" :: Text)
            -- PUT  ["author"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (author :: Author) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     editingCommon sess (AnEntity author)
            --     return $ successResponse  ("success create author" :: Text)
            -- PUT  ["category"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (category :: Category) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     editingCommon sess (AnEntity category)
            --     return $ successResponse  ("success create category" :: Text)
            -- PUT  ["comment"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (comment :: Comment) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     editingCommon sess (AnEntity comment)
            --     return $ successResponse  ("success create comment" :: Text)
            -- PUT  ["draft"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (category :: Draft) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     editingCommon sess (AnEntity category)
            --     return $ successResponse  ("success create draft" :: Text)
            -- PUT  ["tag"] -> do
            --     reqBody <- liftIO $ HTTP.getRequestBodyChunk req
            --     (comment :: Tag) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
            --     editingCommon sess (AnEntity comment)
            --     return $ successResponse  ("success create tag" :: Text)

            -- DELETE  ["user", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     removeCommon sess UserEntReq unpackIdEntity
            --     return $ successResponse  ("delete create user" :: Text)
            -- DELETE  ["author", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     removeCommon sess AuthorEntReq unpackIdEntity
            --     return $ successResponse   ("delete create author" :: Text)
            -- DELETE  ["category", idE] -> do   
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     removeCommon sess CategoryEntReq unpackIdEntity
            --     return $ successResponse  ("delete create category" :: Text)
            -- DELETE  ["comment", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     removeCommon sess CommentEntReq unpackIdEntity
            --     return $ successResponse  ("delete create comment" :: Text)
            -- DELETE  ["draft", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     removeCommon sess DraftEntReq unpackIdEntity
            --     return $ successResponse  ("delete create draft" :: Text)
            -- DELETE  ["tag", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     removeCommon sess TagEntReq unpackIdEntity
            --     return $ successResponse   ("delete create tag" :: Text)
            -- DELETE  ["news", idE] -> do
            --     let unpackIdEntity = BP.read  idE :: Int 
            --     removeCommon sess NewsEntReq unpackIdEntity
            --     return $ successResponse   ("delete create news" :: Text)

            -- _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""

