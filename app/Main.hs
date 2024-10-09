{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Compute
import Control.Monad (forever)
import Data.Aeson
import Data.Text
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets

handle :: Connection -> IO ()
handle conn = forever $ do
  message <- receiveDataMessage conn
  print (eitherDecode $ fromDataMessage message :: Either String ComputeRequest)
  sendTextData conn $ ("Hello, " :: Text) <> fromDataMessage message <> "!"
  return ()

socket :: ServerApp
socket p = do
  conn <- acceptRequest p
  withPingThread conn 30 (return ()) $ handle conn

static :: Application
static = staticApp $ defaultWebAppSettings "static"

app :: Application
app = websocketsOr defaultConnectionOptions socket static

main :: IO ()
main = do
  putStrLn "http://localhost:8080/"
  run 8080 app
