{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Compute
import Control.Concurrent.Async (race)
import Control.Monad (forever)
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Bifunctor (Bifunctor (first))
import Data.String (IsString (fromString))
import Data.Text
import Network.Wai hiding (Request)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets hiding (Request, receiveData)
import CubeState (cubeFromState)

handler :: Connection -> IO ()
handler c = forever $ do
  m <- receiveData
  case m of
    Left e -> sendError e
    Right Cancel -> sendError "Nothing to cancel"
    Right (Compute d) -> computeHandler d
  where
    sendData :: Text -> [Pair] -> IO ()
    sendData s p = sendTextData c $ encode $ object $ ("status" .= s) : p

    sendError :: Text -> IO ()
    sendError e = sendData "error" ["error" .= e]

    receiveData :: IO (Either Text Request)
    receiveData = first fromString . eitherDecode . fromDataMessage <$> receiveDataMessage c

    busyHandler = do
      d <- receiveData
      case d of
        Right Cancel -> return ()
        Right _ -> sendError "Process busy" >> busyHandler
        Left e -> sendError e >> busyHandler

    computeHandler d = do
      print d
      sendData "started" []
      r <- busyHandler `race` solve d (const $ return ())
      case r of
        Left _ -> sendData "canceled" []
        Right cs -> sendData "finished" ["result" .= cubeFromState cs]
      return ()

socket :: ServerApp
socket p = do
  c <- acceptRequest p
  withPingThread c 30 (return ()) $ handler c

static :: Application
static = staticApp $ defaultWebAppSettings "static"

app :: Application
app = websocketsOr defaultConnectionOptions socket static

main :: IO ()
main = do
  putStrLn "http://localhost:8080/"
  run 8080 app
