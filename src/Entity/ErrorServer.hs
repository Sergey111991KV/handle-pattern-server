module Entity.ErrorServer where

import ClassyPrelude ( ($), Read, Show(show), Text, pack )

data ErrorServer = 
        ErrorConnection | 
        ErrorAccess     |
        ErrorParseConfig |
        ErrorGetConfig  |
        DataErrorPostgreSQL |
        DataErrorPostgreSQLPublish |
        ErrorSortedTextNotSupposed |
        ErrorConvert |
        ErrorGetCookie
        
        deriving (Read, Show)

errorText :: ErrorServer -> Text
errorText err = pack $ show err
  