module Database.Publish where

import Database.ImportDatabase
import Database.DatabaseCommon 
import ClassyPrelude 



publishNews :: PG  m => Database.DatabaseCommon.Handle ->  Int -> UserId -> m ()
publishNews h idE idU = do
  let qAuthor = "select id_author from author where id_link_user= (?);"
  resultAuthor <- withConn (hPool h) $ \conn -> query conn qAuthor idU :: IO [Only Int]
  case resultAuthor of
    [Only x] -> do
      let qPublich = "select updeite_news (?, ?);"
      resultPublic <-
        withConn (hPool h) $ \conn -> query conn qPublich (x, idE) :: IO [Only Int]
      case resultPublic of
        [Only 1] -> do
          return  ()
        _ -> do
          throwError DataErrorPostgreSQLPublish
    _ -> do
      throwError DataErrorPostgreSQL
                                            -- здесь конечно нужно запросы обьединить в один - я такое уже сделал в других местах,
                                            -- хотя с другой стороны - здесь будут выходить более явные ошибки
