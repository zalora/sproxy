module Log (
  setup
, debug
, info
, warning
, error
) where

import           Prelude hiding (log, error)
import           Control.Concurrent (myThreadId)
import           Data.Time.Clock (getCurrentTime)
import           System.Log.Logger

setup :: Priority -> IO ()
setup p = updateGlobalLogger "sproxy" (setLevel p)

debug :: String -> IO ()
debug = log DEBUG

info :: String -> IO ()
info = log INFO

warning :: String -> IO ()
warning = log WARNING

error :: String -> IO ()
error = log ERROR

log :: Priority -> String -> IO ()
log p s = do
  tid <- myThreadId
  t <- getCurrentTime
  logM "sproxy" p $ show p ++ " " ++ show tid ++ " " ++ show t ++ ": " ++ s
