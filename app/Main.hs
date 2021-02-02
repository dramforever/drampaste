module Main where

import           Control.Monad.IO.Class
import           Data.Functor
import           Data.String
import           DramPaste.Server
import           Network.Minio
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do
    args <- getArgs
    case args of
        [url] -> do
            ci <- setCredsFrom [fromMinioEnv] (fromString url)
            void . runMinio ci $ do
                app <- dramPasteApp
                liftIO $ hPutStrLn stderr $ "Starting server (default PORT = 8003)"
                liftIO $ Warp.runEnv 8003 app
        _ -> do
            prog <- getProgName
            hPutStrLn stderr $ "Usage: " ++ prog ++ " <s3-url>"
            hPutStrLn stderr $ "Configure access key using MINIO_ACCESS_KEY and MINIO_SECRET_KEY"
            exitFailure
