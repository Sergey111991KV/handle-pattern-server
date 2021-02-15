module Database.SortedNews where

import Database.ImportDatabase
import Database.DatabaseCommon 
import ClassyPrelude 
  

sortedNews :: PG  m => Database.DatabaseCommon.Handle -> Text -> m  [News]
sortedNews h txtCond 
  | txtCond == "date" = sortedDate h
  | txtCond == "author" = sortedAuthor h
  | txtCond == "category" = sortedCategory h
  | txtCond == "photo" = sortedPhoto h
  | otherwise = throwError ErrorSortedTextNotSupposed

sortedDate :: PG  m => Database.DatabaseCommon.Handle -> m [News]
sortedDate h = do
  let q = requestForPost ++ " ORDER BY data_creat_news limit 20;"
  result <- withConn (hPool h) $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      -- writeLogE (errorText DataErrorPostgreSQL ++ " sortedDate")
      throwError DataErrorPostgreSQL
    news -> do
      -- writeLogD "sortedDate success "
      return $ map convertNewsRaw news

sortedAuthor :: PG  m => Database.DatabaseCommon.Handle -> m [News]
sortedAuthor h = do
  let q = requestForPost ++ " ORDER BY authors_id_news limit 20;"
  result <- withConn (hPool h) $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      -- writeLogE (errorText DataErrorPostgreSQL ++ " sortedAuthor")
      throwError DataErrorPostgreSQL
    news -> do
      -- writeLogD "sortedAuthor success "
      return $ map convertNewsRaw news

sortedCategory :: PG  m => Database.DatabaseCommon.Handle -> m [News]
sortedCategory h = do
  let q = requestForPost ++ " ORDER BY endNews.category_id_news limit 20;"
  result <- withConn (hPool h) $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      -- writeLogE (errorText DataErrorPostgreSQL ++ " sortedCategory")
      throwError DataErrorPostgreSQL
    news -> do
      -- writeLogD "sortedCategory success "
      return $ map convertNewsRaw news

sortedPhoto :: PG  m => Database.DatabaseCommon.Handle -> m [News]
sortedPhoto h = do
  let q = requestForPost ++ " ORDER BY other_photo_news limit 20"
  result <- withConn (hPool h) $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      -- writeLogE (errorText DataErrorPostgreSQL ++ " sortedPhoto")
      throwError DataErrorPostgreSQL
    news -> do
      -- writeLogD "sortedPhoto success "
      return $ map convertNewsRaw news
      
--  Здесь можно еще добавить в запрос к базе DESC или ASC - это или ввести новую переменную(что предпочтительнее - так как нельзя 
-- будет ошибиться) или добавить еще варианты txt - тут опять же плохая масштабируемость, но в задании не говорили конкретно как 
-- сортировать)
