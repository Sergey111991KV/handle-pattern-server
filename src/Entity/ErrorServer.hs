module Entity.ErrorServer where

import Entity.ImportLibrary 
import ClassyPrelude

data ErrorServer = 
        ErrorConnection | 
        ErrorAccess     |
        ErrorParseConfig |
        ErrorGetConfig  |
        DataErrorPostgreSQL |
        DataErrorPostgreSQLPublish |
        ErrorConvert 
        deriving (Read, Show)

errorText :: ErrorServer -> Text
errorText err = pack $ show err
  