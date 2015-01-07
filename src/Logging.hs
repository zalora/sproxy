module Logging (setup) where

import           System.Logging.LogSink.Config

setup :: LogLevel -> LogTarget -> IO ()
setup level target = setupLogging [SinkConfig level "{level} {thread-id}: {message}" target]
