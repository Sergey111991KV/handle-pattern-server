module Database.Delete where

import Database.ImportDatabase
import Database.DatabaseCommon 
import ClassyPrelude 

removeAuthor :: PG  m => Database.DatabaseCommon.Handle ->  Int -> m ()
removeAuthor h idE = do
    let q = "DELETE FROM author WHERE id_author = (?);"
    result <- withConn (hPool h) $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        -- writeLogD "delete author good!"
        return ()
      _ -> do
        -- writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL

removeUser :: PG  m => Database.DatabaseCommon.Handle ->  Int -> m ()
removeUser h idE = do
    let q = "DELETE FROM usernews WHERE id_user = (?);"
    result <- withConn (hPool h) $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        -- writeLogD "delete user good!"
        return ()
      _ -> do
        -- writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL

removeNews :: PG  m => Database.DatabaseCommon.Handle ->  Int -> m ()
removeNews h idE = do
    let q = "DELETE FROM news WHERE id_news = (?);"
    result <- withConn (hPool h) $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        -- writeLogD "delete news good!"
        return ()
      _ -> do
        -- writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL

removeTag :: PG  m => Database.DatabaseCommon.Handle ->  Int -> m ()
removeTag h idE = do
    let q = "DELETE FROM tag WHERE id_tag = (?);"
    result <- withConn (hPool h) $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        -- writeLogD "delete tag good!"
        return ()
      _ -> do
        -- writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL

removeCategory :: PG  m => Database.DatabaseCommon.Handle ->  Int -> m ()
removeCategory h idE = do
    let q = "DELETE FROM category WHERE id_category = (?);"
    result <- withConn (hPool h) $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        -- writeLogD "delete category1 good!"
        return ()
      _ -> do
        -- writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL


removeDraft :: PG  m => Database.DatabaseCommon.Handle ->  Int ->  UserId -> m ()
removeDraft h idEnt idA  = do
      let q =
            "DELETE FROM draft WHERE id_author_draft = (select id_link_user from author where id_author = (?) ) and id_draft = (?);"
      result <- withConn (hPool h) $ \conn -> execute conn q (idA, idEnt)
      case result of
        1 -> do
        --   writeLogD "delete draft good!"
          return  ()
        _ -> do
        --   writeLogE (errorText DataErrorPostgreSQL ++ " delete draft")
          throwError DataErrorPostgreSQL