module Database.Edite where

import Database.ImportDatabase
import Database.DatabaseCommon 
import ClassyPrelude 

editingAuthor :: PG  m => Database.DatabaseCommon.Handle ->  Author -> m ()
editingAuthor h author = do
      let q = "UPDATE author SET id_link_user=(?), description=(?) WHERE id_author = (?) ;"
      result <-
        withConn (hPool h) $ \conn ->
          execute
            conn
            q
            (userIdRaw $ idLinkUser author, description author, idAuthor author)
      case result of
        1 -> do
        --   writeLogD "update author good!"
          return ()
        _ -> do
        --   writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL


editingCategory :: PG  m => Database.DatabaseCommon.Handle ->  Category -> m ()
editingCategory h cat = do
      case parentCategory cat of
        Nothing  -> do
          let qMainCat = "UPDATE category SET name_category=(?) WHERE id_category=(?);"
          result <- withConn (hPool h) $ \conn -> execute conn qMainCat (nameCategory cat, idCategory cat)
          case result of
            1 -> do
            --   writeLogD "update main category good!"
              return ()
            _ -> do
            --   writeLogE (errorText DataErrorPostgreSQL)
              throwError DataErrorPostgreSQL
        Just pCat -> do
          let qNestedCat = "UPDATE category SET name_category=(?),parent_category =(?)  WHERE id_category=(?);"
          result <- withConn (hPool h) $ \conn -> execute conn qNestedCat  (nameCategory cat, idCategory pCat, cat)
          case result of
            1 -> do
            --   writeLogD "update nested category good!"
              editingCategory h pCat
            _ -> do
            --   writeLogE (errorText DataErrorPostgreSQL)
              throwError DataErrorPostgreSQL

editingComment :: PG  m => Database.DatabaseCommon.Handle ->  Comment -> m ()
editingComment h comment = do             
      let q =
            "UPDATE comment SET (text_comment,data_create_comment,news_id_comment,user_id_comment) VALUES(?,?,?,?);"
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
        --   writeLogD "update comment good!"
          return ()
        _ -> do
        --   writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL

editingDraft :: PG  m => Database.DatabaseCommon.Handle ->  Draft -> m ()
editingDraft h draft = do
      let q =
            "UPDATE draft SET text_draft=(?), data_create_draft=(?), news_id_draft=(?), main_photo_draft=(?), other_photo_draft=(?), short_name_draft=(?), tags_id=(?), id_author_draft=(?) where id_draft =(?);"
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
            , idAuthorDraft draft
            , idDraft draft)
      case result of
        1 -> do
        --   writeLogD "update draft good!"
          return ()
        _ -> do
        --   writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL

editingTag :: PG  m => Database.DatabaseCommon.Handle ->  Tag -> m ()
editingTag h tag = do
      let qTag = "UPDATE tag SET name_tag=(?) where id_tag= (?);"
      result <- withConn (hPool h) $ \conn -> execute conn qTag (nameTag tag, idTag tag)
      case result of
        1 -> do
        --   writeLogD "update tag good!"
          return  ()
        _ -> do
        --   writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL

editingUser :: PG  m => Database.DatabaseCommon.Handle ->  User -> m ()
editingUser h user = do
      let qUser =
            "UPDATE usernews SET name_user=(?), lastname=(?) , login_user=(?) , password_user=(?) , avatar_user=(?) , datacreate_user=(?) , admin=(?) , authoris=(?)  where id_user=(?);"
      result <-
        withConn (hPool h) $ \conn ->
          execute
            conn
            qUser
            ( nameUser user
            , lastName user
            , userLogin user
            , userPassword user
            , avatar user
            , dataCreate user
            , userIsAdmin user
            , userIsAuthor user
            , idUser user)
      case result of
        1 -> do
        --   writeLogD "update user good!"
          return ()
        _ -> do
        --   writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL