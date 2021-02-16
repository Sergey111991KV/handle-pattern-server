module Database.Access where

import Database.ImportDatabase
import Database.DatabaseCommon 
import ClassyPrelude 
import Text.StringRandom (stringRandomIO)

  
findUserId :: PG  m => Database.DatabaseCommon.Handle -> Login -> Password -> m UserId
findUserId h login password = do
  let q =
        "SELECT id_user FROM userNews where login_user = (?) and password_user = (?)"
  i <- withConn (hPool h) $ \conn -> query conn q (login, password) :: IO [UserId]
  case i of
    [x] -> do
      return  x
    _ -> do
      throwError DataErrorPostgreSQL

newSession :: PG  m => Database.DatabaseCommon.Handle -> UserId -> m  SessionId
newSession h user = do
  deleteOldSession h user
  insertNewSession h user
  let qry = "select key from session where user_news_id= ?"
  result <- withConn (hPool h) $ \conn -> query conn qry [userIdRaw user]
  case result of
        [sId] -> do
          return  sId
        _ -> do
          throwError DataErrorPostgreSQL

deleteOldSession :: PG  m => Database.DatabaseCommon.Handle -> UserId -> m  ()
deleteOldSession h us = do
  result <- withConn (hPool h) $ \conn -> execute conn qry [userIdRaw us]
  case result of
    1 -> do
      return  ()
    0 -> return ()  
    _ -> do
      throwError DataErrorPostgreSQL
 
  where
    qry = "delete from session where user_news_id = ?"

insertNewSession :: PG  m => Database.DatabaseCommon.Handle -> UserId -> m  ()
insertNewSession h uId = do
  sess <- liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
  result <- withConn (hPool h) $ \conn -> execute conn qry (sess, userIdRaw uId)
  case result of
    1 -> do
      return  ()
    _ -> do
      throwError DataErrorPostgreSQL
  where
    qry = "INSERT INTO session (key, user_news_id) values (?,?)"

findUserIdBySession :: PG  m => Database.DatabaseCommon.Handle -> SessionId -> m  UserId
findUserIdBySession h sesId = do
  result <- withConn (hPool h) $ \conn -> query conn qry sesId
  case result of
    [uIdStr] -> do
      return  uIdStr
    _ -> do
      throwError DataErrorPostgreSQL
  where
    qry = "select user_news_id from session where key = ? "


checkAdminAccess :: PG  m => Database.DatabaseCommon.Handle -> SessionId -> m Bool
checkAdminAccess h sesId = do
    idU <- findUserIdBySession h sesId
    resultAdmin <- withConn (hPool h) $ \conn -> query conn qry idU :: IO [Only Bool]
    case resultAdmin of
        [Only b] -> do
          return b
        _ -> do
          throwError DataErrorPostgreSQL
      where qry = "select admin from usernews where id_user = ? "

checkAuthorAccess :: PG  m => Database.DatabaseCommon.Handle -> SessionId -> m Bool
checkAuthorAccess h sesId = do
    idA <- findUserIdBySession h sesId
    resultAuthor <- withConn (hPool h) $ \conn -> query conn qry [idA] :: IO [Only Bool]
    case resultAuthor of
        [Only b] -> do
          return b
        _ -> do
          throwError DataErrorPostgreSQL
      where qry = "select authoris from usernews where id_user = ? "
