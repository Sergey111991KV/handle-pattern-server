module Lib
    ( someFunc
    ) where
import qualified Network.Wai.Handler.Warp as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP
import ClassyPrelude
    ( ($),
      Show(show),
      IO,
      Either,
      SomeException,
      either,
      (++),
      print,
      try )
import qualified Data.Text.IO as TIO

import qualified Web
import qualified Logger 
import qualified Config.Config as Config

import qualified Data.Text                as T
import qualified Database.ExportDatabase as Database



runConfig :: Config.Config -> IO ()
runConfig config = do
    Logger.withHandle (Config.cLogger config) $ \logger ->
        Database.withHandle (Config.cDatabase config) $ \pConf ->
            Web.withHandle (Config.cWeb config) logger pConf  $ \web -> 
                Web.run web logger

someFunc :: IO ()
someFunc = do
    configFromFile :: Either SomeException T.Text <- try $ TIO.readFile "server.config" 
    either print  (\conf -> do
                caseOfConf <- Config.parseConf conf
                either    (\err -> print (show err ++ "take option for server" ))
                          runConfig 
                          caseOfConf
                                        ) configFromFile