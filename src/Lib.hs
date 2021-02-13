module Lib
    ( someFunc
    ) where
import qualified Network.Wai.Handler.Warp as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP

import qualified Web as W
import Entity.ErrorServer
import Log



data Config = Config {
    web :: W.Config
    }



state :: Config
state = Config {
    web = W.Config 3000
 }

someFunc :: IO ()
someFunc = do
    W.run  (web state)