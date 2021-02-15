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

-- DB.checkAdminAccess serverNotAcceessAdmin
-- checkAuthorAccess serverNotAcceessAuthor

route :: HTTPMonad m
  => HTTP.Request -> m HTTP.Response
route req = do
    db <- asks hDatabase
    either  ( `notAutorized` db) ( `autorized`  db) (getCookie req)
    where
        notAutorized ErrorGetCookie db = do
            case methodAndPath req of   
                GET  ["auth", login, pass] -> do
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
                    aceess <- DB.checkAuthorAccess db sess
                    if  aceess  then do
                        let unpackIdEntity = P.read $ unpack  idE :: Int 
                        DB.findUserIdBySession db sess >>= DB.publishNews db unpackIdEntity
                        return $ successResponse   ("publish news" :: Text)
                    else serverNotAcceessAuthor 
                    
                GET  ["news", "sortedNews", condition ] -> do
                    news <- DB.sortedNews db condition
                    return $ successResponse news 

                GET  ["news", "filterAuthor", condition ] -> do
                    let unpackIdAuthor = P.read $ unpack condition :: Int 
                    news <- DB.filterAuthor db unpackIdAuthor
                    return $ successResponse news 
                GET  ["news", "filterCategory", condition ] -> do
                    let unpackIdCategory = P.read $ unpack  condition :: Int 
                    news <- DB.filterCategory db unpackIdCategory
                    return $ successResponse news
                GET  ["news", "filterOfData", condition , date] -> do
                    news <- DB.filterOfData db condition date
                    return $ successResponse news 
                GET  ["news", "filterOneOfTags", condition ] -> do
                    news <- DB.filterOneOfTags db condition
                    return $ successResponse news 
                GET  ["news", "filterAllOfTags", condition ] -> do
                    news <- DB.filterAllOfTags db condition
                    return $ successResponse news 
                GET  ["news", "filterName", condition ] -> do
                    news <- DB.filterName db condition
                    return $ successResponse news 
                GET  ["news", "filterTag", condition ] -> do
                    let unpackIdTag = P.read $ unpack condition :: Int 
                    news <- DB.filterTag db unpackIdTag
                    return $ successResponse news 
                GET  ["news", "filterContent", condition ] -> do
                    news <- DB.filterContent db condition
                    return $ successResponse news 

                GET  ["user", idE] -> do
                    let unpackIdEntity = P.read $ unpack idE :: Int 
                    user <- DB.getOneUser db unpackIdEntity
                    return $ successResponse user
                GET  ["users"] -> do
                    users <- DB.getAllUser db 
                    return $ successResponse  users
                GET  ["author", idE] -> do
                    let unpackIdEntity = P.read $ unpack idE :: Int 
                    author <- DB.getOneAuthor db  unpackIdEntity
                    return $ successResponse  author
                GET  ["authors"] -> do
                    authors <- DB.getAllAuthor db 
                    return $ successResponse  authors
                GET  ["category", idE] -> do   
                    let unpackIdEntity = P.read $ unpack idE :: Int 
                    category <- DB.getOneCategory db  unpackIdEntity
                    return $ successResponse  category
                GET  ["categorys"] -> do
                    categorys <- DB.getAllCategory db 
                    return $ successResponse  categorys
                GET  ["comment", idE] -> do
                    let unpackIdEntity = P.read $ unpack idE :: Int 
                    comment <- DB.getOneComment db  unpackIdEntity
                    return $ successResponse  comment
                GET  ["draft", idE] -> do
                    let unpackIdEntity = P.read $ unpack idE :: Int 
                    draft <-  DB.findUserIdBySession db sess >>=  DB.getOneDraft db  unpackIdEntity
                    return $ successResponse  draft
                GET  ["drafts"] -> do
                    drafts <-  DB.findUserIdBySession db sess >>= DB.getAllDraft db 
                    return $ successResponse drafts
                GET  ["tag", idE] -> do
                    let unpackIdEntity = P.read $ unpack idE :: Int 
                    tag <- DB.getOneTag db  unpackIdEntity
                    return $ successResponse  tag
                GET  ["tags"] -> do
                    tags <- DB.getAllTag db 
                    return $ successResponse  tags
                GET  ["news", idE] -> do
                    let unpackIdEntity = P.read $ unpack idE :: Int 
                    news <- DB.getOneNews db   unpackIdEntity
                    return $ successResponse  news
                GET  ["news_s"] -> do
                    news <- DB.getAllNews db 
                    return $ successResponse news

                POST  ["user"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (user :: User) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.createUser db  user
                    return $ successResponse  ("success create user" :: Text)
                POST  ["author"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (author :: Author) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.createAuthor db  author
                    return $ successResponse  ("success create author" :: Text)
                POST  ["category"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (category :: Category) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.createCategory db  category
                    return $ successResponse  ("success create category" :: Text)
                POST  ["comment"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (comment :: Comment) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.createComment db  comment
                    return $ successResponse  ("success create comment" :: Text)
                POST  ["draft"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (category :: Draft) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.createDraft db  category
                    return $ successResponse  ("success create draft" :: Text)
                POST  ["tag"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (comment :: Tag) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.createTag db  comment
                    return $ successResponse  ("success create tag" :: Text)

                PUT  ["user"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (user :: User) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.editingUser db  user
                    return $ successResponse  ("success create user" :: Text)
                PUT  ["author"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (author :: Author) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.editingAuthor db  author
                    return $ successResponse  ("success create author" :: Text)
                PUT  ["category"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (category :: Category) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.editingCategory db  category
                    return $ successResponse  ("success create category" :: Text)
                PUT  ["comment"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (comment :: Comment) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.editingComment db  comment
                    return $ successResponse  ("success create comment" :: Text)
                PUT  ["draft"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (category :: Draft) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.editingDraft db  category
                    return $ successResponse  ("success create draft" :: Text)
                PUT  ["tag"] -> do
                    reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                    (comment :: Tag) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                    DB.editingTag db  comment
                    return $ successResponse  ("success create tag" :: Text)

                DELETE  ["user", idE] -> do
                    let unpackIdEntity = P.read $ unpack  idE :: Int 
                    DB.removeUser db  unpackIdEntity
                    return $ successResponse  ("delete create user" :: Text)
                DELETE  ["author", idE] -> do
                    let unpackIdEntity = P.read $ unpack  idE :: Int 
                    DB.removeAuthor db  unpackIdEntity
                    return $ successResponse   ("delete create author" :: Text)
                DELETE  ["category", idE] -> do   
                    let unpackIdEntity = P.read $ unpack  idE :: Int 
                    DB.removeCategory db  unpackIdEntity
                    return $ successResponse  ("delete create category" :: Text)
                DELETE  ["draft", idE] -> do
                    let unpackIdEntity = P.read $ unpack  idE :: Int 
                    DB.findUserIdBySession db sess >>= DB.removeDraft db unpackIdEntity
                    return $ successResponse  ("delete create draft" :: Text)
                DELETE  ["tag", idE] -> do
                    let unpackIdEntity = P.read $ unpack  idE :: Int 
                    DB.removeTag db  unpackIdEntity
                    return $ successResponse   ("delete create tag" :: Text)
                DELETE  ["news", idE] -> do
                    let unpackIdEntity = P.read $ unpack  idE :: Int 
                    DB.removeNews db  unpackIdEntity
                    return $ successResponse   ("delete create news" :: Text)

                _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""

