{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (app) where

import Compute (compute)
import Control.Monad (forever)
import Data.Aeson (ToJSON (toJSON), Value, eitherDecode, encode, object, (.=))
import Data.Bifunctor (Bifunctor (first))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Interface (ComputeRequest (size), Request (Request, run))
import LocalSearch.State (State (getPoint))
import MagicCube.Cube (IsCube (toCube), Transformer (..), createCube, cubeToMatrix, fromCube, randomMatrix)
import MagicCube.MemoizedCube (MemoizedCubeState)
import Network.HTTP.Types (Status (..))
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseFile, responseStatus)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
  ( Connection,
    ServerApp,
    defaultConnectionOptions,
    fromDataMessage,
    receiveDataMessage,
    sendTextData,
    withPingThread,
  )
import Network.WebSockets.Connection (acceptRequest)
import System.CPUTime (getCPUTime)

initState :: Int -> IO MemoizedCubeState
initState s = do
  m <- randomMatrix s
  return $ fromCube $ createCube m Digital

createLogger :: (State s) => (Value -> IO ()) -> IO ((s, Value) -> IO ())
createLogger a = do
  iteration <- newIORef (0 :: Int)
  return $ \(s, d) -> do
    let cp = getPoint s
    i <- readIORef iteration
    writeIORef iteration (i + 1)
    a $ object ["iteration" .= i, "point" .= cp, "data" .= d]

data ResponseStatus = Start | Update | Finish | Error deriving (Show)

handler :: Connection -> IO ()
handler c = forever $ do
  m <- receiveData
  print m
  case m of
    Left e -> sendData Error $ toJSON e
    Right (Request {run}) -> case run of
      Nothing -> sendData Error "Compute request undefined"
      Just r -> serveRequest r
  where
    receiveData :: IO (Either Text Request)
    receiveData = first fromString . eitherDecode . fromDataMessage <$> receiveDataMessage c

    sendData :: ResponseStatus -> Value -> IO ()
    sendData s v = sendTextData c $ encode (object ["status" .= show s, "data" .= v])

    sendUpdate :: Value -> IO ()
    sendUpdate = sendData Update

    serveRequest :: ComputeRequest -> IO ()
    serveRequest r = do
      print r
      logger <- createLogger sendUpdate :: IO ((MemoizedCubeState, Value) -> IO ())

      s <- initState $ size r
      let m = cubeToMatrix $ toCube s
      sendData Start $ toJSON m

      t <- getCPUTime
      s' <- compute logger r s
      t' <- getCPUTime
      let m' = cubeToMatrix $ toCube s'
      sendData Finish $ object ["matrix" .= m', "duration" .= (t' - t)]

      return ()

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
