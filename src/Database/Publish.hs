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
    --   writeLogD "getOne News success!"
      let qPublich = "select updeite_news (?, ?);"
      resultPublic <-
        withConn (hPool h) $ \conn -> query conn qPublich (x, idE) :: IO [Only Int]
      case resultPublic of
        [Only 1] -> do
        --   writeLog Debug "publish Draft success!"
          return  ()
        _ -> do
        --   writeLogE
            -- (errorText DataErrorPostgreSQL ++ " can't to publish news")
          throwError DataErrorPostgreSQLPublish
    _ -> do
    --   writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
                                            -- здесь конечно нужно запросы обьединить в один - я такое уже сделал в других местах,
                                            -- хотя с другой стороны - здесь будут выходить более явные ошибки
