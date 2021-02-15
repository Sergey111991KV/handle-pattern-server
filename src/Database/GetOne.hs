module Database.GetOne where


import Database.ImportDatabase
import Database.DatabaseCommon 
import ClassyPrelude 



getOneAuthor :: PG  m => Database.DatabaseCommon.Handle ->  Int -> m Author
getOneAuthor h idE = do
      let qAuthor = "SELECT * from author where id_author=(?)"
      i <- withConn (hPool h) $ \conn -> query conn qAuthor [idE] :: IO [Author]
      case i of
        [x] -> do
        --   writeLogD "getOne Author success!"
          return  x
        _ -> do
        --   writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL

getOneUser :: PG  m => Database.DatabaseCommon.Handle ->  Int -> m User
getOneUser h idE = do
      let qUser = "SELECT * from usernews where id_user=(?)"
      i <- withConn (hPool h) $ \conn -> query conn qUser [idE] :: IO [User]
      case i of
        [x] -> do
        --   writeLogD "getOne User success!"
          return  x
        _ -> do
        --   writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL

getOneNews :: PG  m => Database.DatabaseCommon.Handle ->  Int -> m News
getOneNews h idE = do
      let qNews = requestForPost ++ "where endNews.id_news = (?);"
      i <- withConn (hPool h) $ \conn -> query conn qNews [idE] :: IO [NewsRaw]
      print i
      case i of
        [x] -> do
        --   writeLogD "getOne News success!"
          return $ convertNewsRaw x
        _ -> do
        --   writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL

getOneTag :: PG  m => Database.DatabaseCommon.Handle ->  Int -> m Tag
getOneTag h idE = do
      let qTag = "SELECT * from tag where id_tag=(?)"
      i <- withConn (hPool h) $ \conn -> query conn qTag [idE] :: IO [Tag]
      case i of
        [x] -> do
        --   writeLogD "getOne Tag success!"
          return  x
        _ -> do
        --   writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL

getOneComment :: PG  m => Database.DatabaseCommon.Handle ->  Int -> m Comment
getOneComment h idE = do
      let qComment = "SELECT * from comment where id_comment=(?)"
      i <- withConn (hPool h) $ \conn -> query conn qComment [idE] :: IO [Comment]
      case i of
        [x] -> do
        --   writeLogD "getOne Comment success!"
          return  x
        _ -> do
        --   writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL

getOneCategory :: PG  m => Database.DatabaseCommon.Handle ->  Int -> m Category
getOneCategory h idE = do
      let qCategory =
            "with recursive temp1 (id_category, parent_category, name_category) as ( \
                                    \ select t1.id_category, t1.parent_category, t1.name_category \
                                    \ from category t1 where t1.id_category = (?) \
                                    \ union \
                                    \ select t2.id_category, t2.parent_category, t2.name_category \
                                    \ from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) \
                                    \ SELECT distinct id_category, name_category, parent_category from temp1"
      i <- withConn (hPool h) $ \conn -> query conn qCategory [idE] :: IO [CategoryRaw]
      if null i
        then do
        --   writeLogE (errorText ErrorConvert ++ " finalCategoryConvert")
          throwError DataErrorPostgreSQL
        else do
          let cat = head $ impureNonNull $ convertCategoryRawArray i
          return  cat


getOneDraft :: PG  m => Database.DatabaseCommon.Handle ->  Int -> UserId -> m  Draft
getOneDraft h idE idA = do
  resultDraft <- withConn (hPool h) $ \conn -> query conn qry (idA, idE) :: IO [Draft]
  case resultDraft of
    [x] -> do
        --   writeLogD "getOne Draft success!"
          return  x
    _ -> do
        --   writeLogE "getOneDraft DataErrorPostgreSQL "
          throwError DataErrorPostgreSQL
  where qry =
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
                                                            \    and author.id_link_user = (?) \
                                                            \    and draft.id_draft= (?);"
