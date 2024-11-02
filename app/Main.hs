{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Algorithm
import Compute
import Control.Concurrent (modifyMVar_)
import Control.Concurrent.Async (race)
import Control.Monad (forever)
import CubeState (cubeFromState)
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Bifunctor (Bifunctor (first))
import Data.String (IsString (fromString))
import Data.Text
import GHC.MVar (newMVar, putMVar, takeMVar, MVar(..))
import Network.Wai hiding (Request)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets hiding (Request, receiveData)
import System.CPUTime (getCPUTime)
import Network.HTTP.Types.Status

data ResponseTiming = ResponseTiming
  { nextTime :: Int,
    deltaTime :: Int
  }

handler :: Connection -> IO ()
handler c = do
  timing <- newMVar $ ResponseTiming 0 (16 * 1000 * 1000 * 1000)
  let sendData :: Text -> [Pair] -> IO ()
      sendData s p = sendTextData c $ encode $ object $ ("status" .= s) : p

      sendError :: Text -> IO ()
      sendError e = sendData "error" ["error" .= e]

      receiveData :: IO (Either Text Request)
      receiveData = first fromString . eitherDecode . fromDataMessage <$> receiveDataMessage c

      setTiming :: Int -> IO ()
      setTiming i = modifyMVar_ timing $ \r -> return $ r {deltaTime = i * 1000 * 1000 * 1000}

      busyHandler = do
        d <- receiveData
        case d of
          Right Cancel -> return ()
          Right (Timing i) -> setTiming i >> busyHandler
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

      computeHandler d = do
        print d
        sendData "started" []
        ct <- fromInteger <$> getCPUTime
        modifyMVar_ timing $ \r -> return $ r {nextTime = ct}
        r <- busyHandler `race` solve d (stateUpdateHandler timing)
        case r of
          Left _ -> sendData "canceled" []
          Right cs -> sendData "finished" ["result" .= cubeFromState cs]
        return ()
   in forever $ do
        m <- receiveData
        case m of
          Left e -> sendError e
          Right Cancel -> sendError "Nothing to cancel"
          Right (Compute d) -> computeHandler d
          Right (Timing i) -> setTiming i

socket :: ServerApp
socket p = do
  c <- acceptRequest p
  withPingThread c 30 (return ()) $ handler c

static :: Application
static = staticApp $ defaultWebAppSettings "static"

fallback :: Application -> Application -> Application
fallback appA appB req respond = appA req $ \res -> do
  let code = statusCode . responseStatus $ res
  if code < 300 then respond res
  else appB req respond

staticFile :: FilePath -> Application
staticFile path _ respond = do
  respond $  responseFile status200 [] path Nothing

app :: Application
app = websocketsOr defaultConnectionOptions socket $ static `fallback` staticFile "static/index.html"

main :: IO ()
main = do
  putStrLn "http://localhost:8080/"
  run 8080 app
