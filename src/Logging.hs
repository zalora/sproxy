module Logging (setup) where

import           Prelude hiding (log)
import           Control.Concurrent (myThreadId)
import           System.IO
import           System.Posix.Syslog
import           System.Logging.Facade.Sink
import           System.Logging.Facade.Types

import           ConfigFile

logSink :: LogLevel -> LogTarget -> LogSink
logSink logLevel target (LogRecord level _location message)
  | logLevel <= level = do
      tid <- myThreadId
      log (show level ++ " " ++ show tid ++ ": " ++ message)
  | otherwise = return ()
  where
    log = case target of
      Stderr -> hPutStrLn stderr
      Syslog -> syslog (logLevelToPriority level)

logLevelToPriority :: LogLevel -> Priority
logLevelToPriority l = case l of
  TRACE -> Debug
  DEBUG -> Debug
  INFO -> Info
  WARN -> Warning
  ERROR -> Error

setup :: LogLevel -> LogTarget -> IO ()
setup level = setLogSink . logSink level
