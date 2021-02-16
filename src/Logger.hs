module Logger where

import ClassyPrelude
import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Time (formatISODateTime)
import System.IO (appendFile)

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


newtype Handle = Handle
    { hConfig  :: Config
    }

withHandle :: Config -> (Logger.Handle -> IO a) -> IO a
withHandle config f =  f Handle {hConfig = config}

writeInLogFile :: FilePath -> Bool -> Text -> IO ()
writeInLogFile lF bl txtInLog = do
  when bl $ appendFile lF (ClassyPrelude.unpack txtInLog)

writeInTerminal :: Bool -> Text -> IO ()
writeInTerminal bl txtInLog = do
  when bl $ ClassyPrelude.putStrLn txtInLog

writFileHandler ::
     UTCTime -> FilePath -> LogWrite -> LogWrite -> Bool -> Text -> IO ()
writFileHandler dat lF logConf logToCompare bl txtInLog = do
  writeInLogFile lF (logConf >= logToCompare) (txtInLog <> " " <> d)
  writeInTerminal (logConf >= logToCompare) txtInLog
  where
    d = toStrict $ formatISODateTime dat

writeLogHandler :: UTCTime -> Config -> LogWrite -> Text -> IO ()
writeLogHandler dat (Config lf logLev logBool) loging =
  writFileHandler dat lf logLev loging logBool

hLogDebug :: Logger.Handle ->  Text ->  IO ()
hLogDebug h text = do
  time <- liftIO getCurrentTime
  writeLogHandler time (hConfig h) Debug text
 
hLogError :: Logger.Handle ->  Text ->  IO ()
hLogError h text = do
  time <- liftIO getCurrentTime
  writeLogHandler time (hConfig h) Debug text
 