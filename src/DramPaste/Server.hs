module DramPaste.Server where

import           Conduit
import           Control.Monad.Except
import qualified Data.ByteString as B
import           Data.Proxy
import qualified Data.Text as T
import           DramPaste.Words
import           Network.Minio
import           Servant.API
import           Servant.Conduit ()
import           Servant.Server
import           System.IO
import           System.Random
import qualified UnliftIO as U

bucketName :: Bucket
bucketName = "drampaste"

identToObject :: T.Text -> Object
identToObject = ("paste--" <>)

type DramPasteAPI =
        Capture "id" T.Text
        :> StreamGet NoFraming OctetStream (ConduitT () B.ByteString IO ())
    :<|>
        StreamBody NoFraming OctetStream (ConduitT () B.ByteString Minio ())
        :> Post '[PlainText] T.Text

handleGet :: T.Text -> Minio (ConduitT () B.ByteString IO ())
handleGet ident = do
    resp <- getObject bucketName (identToObject ident) defaultGetObjectOptions
    minioConduit $ gorObjectStream resp

handlePost :: ConduitT () B.ByteString Minio () -> Minio T.Text
handlePost post = do
    ident <- liftIO (randomMnemonic <$> newStdGen)
    putObject bucketName (identToObject ident) post Nothing defaultPutObjectOptions
    pure ident

dramPaste :: ServerT DramPasteAPI Minio
dramPaste = handleGet :<|> handlePost

dramPasteApp :: Minio Application
dramPasteApp = withRunInIO (\run ->
    pure $ serve (Proxy @DramPasteAPI)
        (hoistServer (Proxy @DramPasteAPI)
            (minioToHandler run)
            dramPaste))

-- Helper functions

minioToHandler :: (forall r. Minio r -> IO r) -> Minio a -> Handler a
minioToHandler run m = do
    res <- liftIO . run $ fmap Right m
        `U.catches` [ U.Handler (pure . Left) ]
    case res of
        Left NoSuchKey -> throwError err404
        Left err -> do
            liftIO $ hPrint stderr err
            throwError err500
        Right a -> pure a

minioConduit
    :: ConduitT i o Minio r
    -> Minio (ConduitT i o IO r)
minioConduit conduit =
    withRunInIO (\run -> pure $ transPipe run conduit)
