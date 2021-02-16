module Database.FilterNews where

import Database.ImportDatabase
    ( Monad(return),
      MonadIO(liftIO),
      MonadError(throwError),
      Query,
      query,
      ErrorServer(DataErrorPostgreSQL),
      convertNewsRaw,
      News,
      NewsRaw )
import Database.DatabaseCommon
    ( requestForPost,
      requestForPostAllFilterTag,
      requestForPostFilterTag,
      withConn,
      Handle(hPool),
      PG ) 
import ClassyPrelude
    ( otherwise,
      ($),
      Eq((==)),
      Show(show),
      Int,
      IO,
      String,
      Text,
      (++),
      map,
      pack,
      unpack ) 
import qualified Prelude as   P

filterOfData :: PG  m => Database.DatabaseCommon.Handle -> Text -> Text -> m  [News]
filterOfData h condition time = do
  let q = requestForPost ++ conversCond condition
  result <- withConn (hPool h) $ \conn -> query conn q [time] :: IO [NewsRaw]
  case result of
    [] -> do
      throwError DataErrorPostgreSQL
    news -> do
      return $  map convertNewsRaw news

conversCond :: Text -> Query
conversCond txtCond 
                | txtCond ==  "less" =  " where data_creat_news <= (?);"
                | txtCond == "more" = " where data_creat_news >= (?);"
                | txtCond == "equel" = " where data_creat_news == (?);"
                | otherwise = "Error"

filterAuthor :: PG  m => Database.DatabaseCommon.Handle -> Int -> m  [News]
filterAuthor h idA = do
  let q = requestForPostFilterTag ++ " where endNews.id_author = (?) limit 20;"
  result <- withConn (hPool h) $ \conn -> query conn q  [idA]   :: IO [NewsRaw]
  case result of
    [] -> do
      throwError DataErrorPostgreSQL
    news -> do
      return $ map convertNewsRaw news

filterCategory :: PG  m => Database.DatabaseCommon.Handle -> Int -> m [News]
filterCategory h catId = do
  let q = requestForPostFilterTag ++ " where endNews.category_id_news = (?) limit 20;"
  result <- withConn (hPool h) $ \conn -> query conn q [ catId ] :: IO [NewsRaw]
  case result of
    [] -> do
      throwError DataErrorPostgreSQL
    news -> do
      return $ map convertNewsRaw news

filterTag :: PG  m => Database.DatabaseCommon.Handle -> Int -> m [News]
filterTag h idT = do
  let q = requestForPostFilterTag ++ " where tags_news.tags_id = (?) limit 20;"
  liftIO $ P.print  idT
  result <- withConn (hPool h) $ \conn -> query conn q [idT ] :: IO [NewsRaw]
  case result of
    [] -> do
      throwError DataErrorPostgreSQL
    news -> do
      return $ map convertNewsRaw news

filterOneOfTags :: PG  m => Database.DatabaseCommon.Handle -> Text -> m [News]
filterOneOfTags h idTarray = do
  let reqArr = "{" ++ idTarray ++ "}"
  let q = requestForPostFilterTag ++ " where  tags_news.tags_id = any (?) limit 20;"
  result <-
    withConn (hPool h) $ \conn ->
      query conn q [reqArr] :: IO [NewsRaw]
  case result of
    [] -> do
      throwError DataErrorPostgreSQL
    news -> do
      return $ map convertNewsRaw news

filterAllOfTags :: PG  m => Database.DatabaseCommon.Handle -> Text -> m  [News]
filterAllOfTags h idTarray = do
  let reqArr = createAllTagRequest $ unpack idTarray
  let q = requestForPostAllFilterTag 
  result <-
    withConn (hPool h) $ \conn ->
      query conn q [reqArr] :: IO [NewsRaw]
  case result of
    [] -> do
      throwError DataErrorPostgreSQL
    news -> do
      return $ map convertNewsRaw news

createAllTagRequest :: String -> String
createAllTagRequest  = createAllTagRequest' "%"  
  where
    createAllTagRequest' arr [] = arr
    createAllTagRequest' arr (',':xs) = createAllTagRequest' arr xs
    createAllTagRequest' arr (x:xs) = createAllTagRequest' (arr ++ [x] ++ "%") xs
 
filterName :: PG  m => Database.DatabaseCommon.Handle -> Text -> m  [News]
filterName h txtName = do
  let insertText = "%" ++ txtName ++ "%"
  let q = requestForPost ++ " where endNews.short_name_news LIKE (?) limit 20;"
  result <- withConn (hPool h) $ \conn -> query conn q [insertText] :: IO [NewsRaw]
  case result of
    [] -> do
      throwError DataErrorPostgreSQL
    news -> do
      return $ map convertNewsRaw news

filterContent :: PG  m => Database.DatabaseCommon.Handle -> Text -> m [News]
filterContent h txtContent = do
  let insertText = "%" ++ txtContent ++ "%"
  let q = requestForPost ++ " where endNews.text_news LIKE (?) limit 20;"
  result <- withConn (hPool h) $ \conn -> query conn q [insertText] :: IO [NewsRaw]
  case result of
    [] -> do
      throwError DataErrorPostgreSQL
    news -> do
      return $  map convertNewsRaw news

toStringFromArrayInt :: [Int] -> Text
toStringFromArrayInt array =
  pack $ "{" ++ P.foldl addParam "" array ++ "}"
  where
    addParam [] arr = show arr
    addParam elements arr = elements ++ (',' : show arr)
