module Entity.ErrorServer where

import qualified Data.Text                as T



data ErrorServer = 
        ErrorConnection | 
        ErrorAccess     |
        ErrorParseConfig |
        ErrorGetConfig  |
        DataErrorPostgreSQL |
        ErrorConvert
        deriving (Read, Show)

errorText :: ErrorServer -> T.Text
errorText err = T.pack $ show err
  