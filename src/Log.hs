module Log where

import GHC.Generics 
import Data.Aeson (FromJSON, ToJSON)


type LogWriteInConfig = LogWrite

data LogConfig =
  LogConfig
    { logFile :: FilePath
    , logLevelForFile :: LogWriteInConfig
    , logConsole :: Bool
    }
  deriving (Show, Generic)

type LogForFile = LogWrite

data LogWrite
  = Debug
  | Warning
  | Error
  deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON LogWrite
instance FromJSON LogWrite
instance ToJSON LogConfig
instance FromJSON LogConfig


data Handle = Handle
    { hConfig    :: LogConfig
    }

withHandle :: LogConfig -> (Handle -> IO a) -> IO a
withHandle config f =  f Handle {hConfig = config}