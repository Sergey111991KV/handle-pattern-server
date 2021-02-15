module Logger where

import ClassyPrelude
import Data.Aeson (FromJSON, ToJSON)


data Config =
  Config
    { logFile :: FilePath
    , logLevelForFile :: LogWrite
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
instance ToJSON Config
instance FromJSON Config


data Handle = Handle
    { hConfig    :: Config
    }

withHandle :: Config -> (Logger.Handle -> IO a) -> IO a
withHandle config f =  f Handle {hConfig = config}