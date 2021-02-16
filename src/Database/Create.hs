module Database.Create where

import Database.ImportDatabase
import Database.DatabaseCommon 
import ClassyPrelude 

createUser :: PG  m => Database.DatabaseCommon.Handle ->  User -> m ()
createUser h user = do
        let qUser = "INSERT INTO usernews (name_user, lastname , login_user , password_user , avatar_user , datacreate_user , admin , authoris)  VALUES (?,?,?,?,?,?,?,?);"
        result <- withConn (hPool h) $ \conn ->  execute conn qUser
            ( nameUser user
            , lastName user
            , userLogin user
            , userPassword user
            , avatar user
            , dataCreate user
            , userIsAdmin user
            , userIsAuthor user)
        case result of
            1 -> do
                return  ()
            _ -> do
                throwError DataErrorPostgreSQL

createAuthor :: PG  m => Database.DatabaseCommon.Handle ->  Author -> m ()
createAuthor h author = do
      let q = "INSERT INTO author (id_link_user, description) VALUES (?,?);"
      result <-
        withConn (hPool h) $ \conn ->
          execute conn q (userIdRaw $ idLinkUser author, description author)
      case result of
        1 -> do
          return ()
        _ -> do
          throwError DataErrorPostgreSQL


createCategory :: PG  m => Database.DatabaseCommon.Handle ->  Category -> m ()
createCategory h cat = do       
      case parentCategory cat of
        Nothing  -> do
          let qMainCat = "INSERT INTO category (name_category,parent_category) VALUES (?,?);"
          result <- withConn (hPool h) $ \conn -> execute conn qMainCat (nameCategory cat, Null)
          case result of
            1 -> do
              return ()
            _ -> do
              throwError DataErrorPostgreSQL
        Just pCat -> do
          let qNestedCat = "INSERT INTO category (name_category,parent_category) VALUES (?,?);"
          result <- withConn (hPool h) $ \conn -> execute conn qNestedCat (nameCategory cat , idCategory pCat)
          case result of
            1 -> do
              createCategory h pCat
            _ -> do
              throwError DataErrorPostgreSQL


createComment :: PG  m => Database.DatabaseCommon.Handle ->  Comment -> m ()
createComment h comment = do  
      let q =
            "INSERT INTO comment (text_comment,data_create_comment,news_id_comment,user_id_comment) VALUES(?,?,?,?);"
      result <-
        withConn (hPool h) $ \conn ->
          execute
            conn
            q
            ( textComments comment
            , dataCreateComments comment
            , newsIdComments comment
            , usersIdComments comment)
      case result of
        1 -> do
          return ()
        _ -> do
          throwError DataErrorPostgreSQL


createDraft :: PG  m => Database.DatabaseCommon.Handle ->  Draft -> m ()
createDraft h draft = do  
      let q =
            "INSERT INTO draft (text_draft, data_create_draft, news_id_draft, main_photo_draft,other_photo_draft, short_name_draft,tags_id, id_author_draft) VALUES (?,?,?,?,?,?,?,?);"
      result <-
        withConn (hPool h) $ \conn ->
          execute
            conn
            q
            ( textDraft draft
            , dataCreateDraft draft
            , newsIdDraft draft
            , mainPhotoUrl draft
            , otherPhotoUrl draft
            , shortNameDraft draft
            , tagsId draft
            , idAuthorDraft draft)
      case result of
        1 -> do
          return  ()
        _ -> do
          throwError DataErrorPostgreSQL

createTag :: PG  m => Database.DatabaseCommon.Handle ->  Tag -> m ()
createTag h tag = do  
      let qTag = "INSERT INTO tag (name_tag) VALUES(?);"
      result <- withConn (hPool h) $ \conn -> execute conn qTag [nameTag tag]
      case result of
        1 -> do
          return  ()
        _ -> do
          throwError DataErrorPostgreSQL
    