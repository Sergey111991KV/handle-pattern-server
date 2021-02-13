module Config.Config where

import Config.ParseConfig (ConfigPair, myParser)
import Entity.ErrorServer 
import qualified Text.Parsec as Pars
import qualified Logger as Logger
import qualified Web as Web
import qualified Data.Text                as T
import GHC.Generics 
import Data.Maybe

  -- Config
  --   { logFile :: FilePath
  --   , logLevelForFile :: LogWrite
  --   , logConsole :: Bool
  --   }


-- data Config = Config {
--     port :: Int
--     }



data Config =
  Config
   { cLogger      :: Logger.Config
    , cWeb        :: Web.Config
    }
  deriving (Generic, Show)

parseConf ::  T.Text -> IO (Either ErrorServer Config)
parseConf = return . configVKwithPair . getPairFromFile

-- charToWord8 :: Char -> Word8
-- charToWord8 = toEnum . fromEnum

getPairFromFile :: T.Text -> Either Pars.ParseError [ConfigPair]
getPairFromFile = Pars.parse myParser ""

configVKwithPair ::
     Either Pars.ParseError [ConfigPair] -> Either ErrorServer Config
configVKwithPair (Left _) = Left ErrorParseConfig
configVKwithPair (Right configPair) = do 
  Right Config {
    cLogger = Logger.Config {
                  Logger.logFile = "log-journal"
                  , Logger.logLevelForFile = Logger.Debug
                  , Logger.logConsole = True
    },
    cWeb = Web.Config {
          Web.port = read $ fromMaybe "3000" port
    }
  }
  where
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