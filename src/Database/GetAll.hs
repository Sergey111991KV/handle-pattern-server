module Database.GetAll where

import Database.ImportDatabase
    ( Monad(return),
      MonadError(throwError),
      query,
      query_,
      Draft,
      Tag,
      ErrorServer(DataErrorPostgreSQL),
      UserId,
      convertCategoryRawArray,
      Category,
      CategoryRaw,
      Author,
      User,
      convertNewsRaw,
      News,
      NewsRaw )
import Database.DatabaseCommon
    ( requestForPost, withConn, Handle(hPool), PG ) 
import ClassyPrelude ( ($), IO, (++), map, null ) 

getAllAuthor :: PG  m => Database.DatabaseCommon.Handle ->  m [Author]
getAllAuthor h = do
    let qAuthor = "SELECT * from author limit 20;"
    result <- withConn (hPool h) $ \conn -> query_ conn qAuthor :: IO [Author]
    case result of
      [] -> do
        throwError DataErrorPostgreSQL
      authorsArray -> do
        return  authorsArray


getAllUser :: PG  m => Database.DatabaseCommon.Handle ->  m [User]
getAllUser h = do
    let qUser = "SELECT * from usernews limit 20;"
    result <- withConn (hPool h) $ \conn -> query_ conn qUser :: IO [User]
    case result of
      [] -> do
        throwError DataErrorPostgreSQL
      users -> do
        return users

getAllTag :: PG  m => Database.DatabaseCommon.Handle ->  m [Tag]
getAllTag h = do
    let qTag = "SELECT * from tag limit 20;"
    result <- withConn (hPool h) $ \conn -> query_ conn qTag :: IO [Tag]
    case result of
      [] -> do
        throwError DataErrorPostgreSQL
      users -> do
        return  users

getAllCategory :: PG  m => Database.DatabaseCommon.Handle ->  m [Category]
getAllCategory h = do
    let qCat =
          "with recursive temp1 (id_category, parent_category, name_category) as ( \
                                    \ select t1.id_category, t1.parent_category, t1.name_category \
                                    \ from category t1  \
                                    \ union \
                                    \ select t2.id_category, t2.parent_category, t2.name_category \
                                    \ from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) \
                                    \ SELECT distinct id_category, name_category, parent_category from temp1 limit 20;"
    result <- withConn (hPool h) $ \conn -> query_ conn qCat :: IO [CategoryRaw]
    case result of
      [] -> do
        throwError DataErrorPostgreSQL
      cat -> do
        return $ convertCategoryRawArray cat


getAllNews :: PG  m => Database.DatabaseCommon.Handle ->  m [News]
getAllNews h = do
    let qDraft = requestForPost ++ " limit 20;"
    result <- withConn (hPool h) $ \conn -> query_ conn qDraft :: IO [NewsRaw]
    case result of
      [] -> do
        throwError DataErrorPostgreSQL
      news -> do
        return $  map convertNewsRaw news


getAllDraft :: PG  m => Database.DatabaseCommon.Handle -> UserId ->  m [Draft]
getAllDraft h uId = do
  let q =
        "SELECT  draft.id_draft, \
                           \ draft.text_draft, \
                           \ draft.data_create_draft, \
                           \ draft.news_id_draft, \
                           \ draft.main_photo_draft, \
                           \ draft.short_name_draft, \
                           \ draft.other_photo_draft, \
                           \ draft.tags_id, \
                           \ draft.id_author_draft \
                                 \   from draft, author where draft.id_author_draft= author.id_author  \
                                                            \    and author.id_link_user = (?) limit 20;"
  result <- withConn (hPool h) $ \conn -> query conn q [uId] :: IO [Draft]
  if null result
    then do
      throwError DataErrorPostgreSQL
    else do
      return  result