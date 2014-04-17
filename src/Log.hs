module Log (
  setup
, debug
) where

import           Control.Concurrent (myThreadId)
import           Data.Time.Clock (getCurrentTime)
import           System.Log.Logger

setup :: IO ()
setup = updateGlobalLogger "sproxy" (setLevel DEBUG)

debug :: String -> IO ()
debug s = do
  tid <- myThreadId
  t <- getCurrentTime
  debugM "sproxy" $ show tid ++ " " ++ show t ++ ": " ++ s
