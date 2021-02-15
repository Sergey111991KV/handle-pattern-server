module Lib
    ( someFunc
    ) where
import qualified Network.Wai.Handler.Warp as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP
import ClassyPrelude
import qualified Data.Text.IO as TIO

import qualified Web as Web
import Entity.ErrorServer
import qualified Logger as Logger
import qualified Config.Config as Config

import qualified Data.Text                as T
import qualified Database as Postgr



runConfig :: Config.Config -> IO ()
runConfig config = do
    Logger.withHandle (Config.cLogger config) $ \logger ->
        Postgr.withHandle (Config.cDatabase config) $ \pConf ->
            Web.withHandle (Config.cWeb config) logger pConf  $ \web -> 
                Web.run  web

someFunc :: IO ()
someFunc = do
    configFromFile :: Either SomeException T.Text <- try $ TIO.readFile "server.config" 
    either print  (\conf -> do
                caseOfConf <- Config.parseConf conf
                either    (\err -> print (show err ++ "take option for server" ))
                          runConfig 
                          caseOfConf
                                        ) configFromFile