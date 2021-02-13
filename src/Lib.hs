module Lib
    ( someFunc
    ) where
import qualified Network.Wai.Handler.Warp as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP

import qualified Web as W
import Entity.ErrorServer
import qualified Logger as L



data Config = Config 
    { confWeb :: W.Config 
    , logger :: L.Config
    }



state :: Config
state = Config {
    confWeb = W.Config 3000 ,
    logger = L.Config {
        L.logFile = "FilePath",
        L.logLevelForFile = L.Debug,
        L.logConsole = True
    }
 }

someFunc :: IO ()
someFunc = do
    L.withHandle (logger state) $ \l ->
        W.withHandle (confWeb state) l $ \web -> 
            W.run  web