{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Algorithm
import Compute
import Control.Concurrent.Async (race)
import Control.Monad (forever)
import CubeState (cubeFromState)
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Bifunctor (Bifunctor (first))
import Data.String (IsString (fromString))
import Data.Text
import GHC.MVar (MVar, newMVar, putMVar, takeMVar)
import Network.Wai hiding (Request)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets hiding (Request, receiveData)
import System.CPUTime (getCPUTime)

data ResponseTiming = ResponseTiming
  { nextTime :: Int,
    deltaTime :: Int
  }

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

    stateUpdateHandler :: MVar ResponseTiming -> IterationIO
    stateUpdateHandler m p = do
      rt <- takeMVar m
      ct <- fromInteger <$> getCPUTime
      if ct < nextTime rt
        then putMVar m rt
        else do
          sendData "update" p
          putMVar m $ rt {nextTime = ct + deltaTime rt}

    defaultTiming = 1000 * 1000 * 1000 * 100
    computeHandler d = do
      print d
      sendData "started" []
      ct <- fromInteger <$> getCPUTime
      m <- newMVar $ ResponseTiming (ct + defaultTiming) defaultTiming
      r <- busyHandler `race` solve d (stateUpdateHandler m)
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
