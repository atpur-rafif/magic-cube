module Main (main) where
import Network.Wai.Handler.Warp (run)
import Server (app)


main :: IO ()
main = do
  run 8080 app
