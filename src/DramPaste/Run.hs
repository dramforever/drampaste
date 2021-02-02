module DramPaste.Run where

import DramPaste.Server
import Network.Wai.Handler.Warp
import Servant.Server
import System.Environment
import Text.Read

serveDramPaste :: IO ()
serveDramPaste = do
    app <- dramPasteApp
    port <- getEnv "PORT"
    run (read port) app
