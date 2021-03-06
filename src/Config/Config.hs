module Config.Config where

import ClassyPrelude

import Config.ParseConfig (ConfigPair, myParser)
import Entity.ErrorServer 
import qualified Text.Parsec as Pars
import qualified Logger as Logger
import qualified Web as Web
import qualified Web.Route as Web
import qualified Database.ExportDatabase as Database
import qualified Prelude as P


data Config =
  Config
   { cLogger      :: Logger.Config
    , cDatabase   :: Database.Config
    , cWeb        :: Web.Config
    }
  deriving (Generic, Show)

parseConf :: Text -> IO (Either ErrorServer Config)
parseConf = return . configVKwithPair . getPairFromFile

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

getPairFromFile :: Text -> Either Pars.ParseError [ConfigPair]
getPairFromFile = Pars.parse myParser ""

configVKwithPair ::
     Either Pars.ParseError [ConfigPair] -> Either ErrorServer Config
configVKwithPair (Left _) = Left ErrorParseConfig
configVKwithPair (Right configPair) = do 
  Right Config {
    cLogger = Logger.Config {
                  Logger.logFile = "log-journal"
                  , Logger.logLevelForFile = Logger.Error 
                  , Logger.logConsole = True
    },
    cDatabase = Database.Config {
        Database.configUrl = pack  $ fmap charToWord8 $ fromMaybe "postgres" postgresOption
        , Database.configStripeCount = 2
        , Database.configMaxOpenConnPerStripe = 5
        , Database.configIdleConnTimeout = 10
    },
    cWeb = Web.Config {
          Web.port = P.read $ fromMaybe "3000" port
    }
  }
  where
    postgresOption = lookup "postgres" configPair
    port =  lookup "port" configPair















  -- if postgresOption == Nothing P.&& port == Nothing then Left ErrorGetConfig
  -- else do
  --  Right
  --   Config
  --     { configPort = P.read $ fromMaybe "3000" port
  --     , configLog =
  --         Log.StateLog
  --           { Log.logStCong =
  --               Log.LogConfig
  --                 { Log.logFile = "log-journal"
  --                 , Log.logLevelForFile = Log.Debug
  --                 , Log.logConsole = True
  --                 }
  --           }
  --     , configPG =
  --         Pos.Config
  --           { Pos.configUrl = pack  $ fmap charToWord8 $ fromMaybe "postgres" postgresOption
  --           , Pos.configStripeCount = 2
  --           , Pos.configMaxOpenConnPerStripe = 5
  --           , Pos.configIdleConnTimeout = 10
  --           }
  --     }
  -- where
  --   postgresOption = lookup "postgres" configPair
  --   port =  lookup "port" configPair