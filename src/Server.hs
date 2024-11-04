{-# LANGUAGE OverloadedStrings #-}
module Server (app) where

import Network.HTTP.Types (Status (..))
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseFile, responseStatus)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
    ( ServerApp,
      defaultConnectionOptions,
      withPingThread,
      Connection,
      receiveDataMessage, fromDataMessage )
import Network.WebSockets.Connection (acceptRequest)
import Interface (Request)
import Data.Text (Text)
import Data.Aeson (eitherDecode)
import Data.Bifunctor (Bifunctor(first))
import Data.String (IsString(fromString))
import Control.Monad (forever)

handler :: Connection -> IO ()
handler c = forever $ do
  m <- receiveData
  print m
  return ()
    where
      receiveData :: IO (Either Text Request)
      receiveData = first fromString . eitherDecode . fromDataMessage <$> receiveDataMessage c

socket :: ServerApp
socket p = do
  c <- acceptRequest p
  withPingThread c 30 (return ()) $ handler c

static :: Application
static = staticApp $ defaultWebAppSettings "static"

staticFile :: FilePath -> Application
staticFile path _ respond = do
  respond $ responseFile status200 [] path Nothing

fallback :: Application -> Application -> Application
fallback appA appB req respond = appA req $ \res -> do
  let code = statusCode . responseStatus $ res
  if code < 300
    then respond res
    else appB req respond

app :: Application
app = websocketsOr defaultConnectionOptions socket $ static `fallback` staticFile "static/index.html"
