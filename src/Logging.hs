module Logging (
  LogLevel(..)
, error
, debug
, info
, start
, warn
) where

import Prelude hiding (error)

import Control.Applicative (empty)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (hPrint, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import qualified Data.Aeson as JSON
import qualified Data.Text as T

start :: LogLevel -> IO ()
start None = return ()
start lvl = do
  writeIORef logLevel lvl
  ch <- readIORef chanRef
  _ <- forkIO . forever $ readChan ch >>= hPrint stderr
  return ()

info :: String -> IO ()
info = send . Message Info

warn:: String -> IO ()
warn = send . Message Warning

error:: String -> IO ()
error = send . Message Error

debug :: String -> IO ()
debug = send . Message Debug


send :: Message -> IO ()
send msg@(Message l _) = do
  lvl <- readIORef logLevel
  when (l <= lvl) $ do
    ch <- readIORef chanRef
    writeChan ch msg

{-# NOINLINE chanRef #-}
chanRef :: IORef (Chan Message)
chanRef = unsafePerformIO (newChan >>= newIORef)

{-# NOINLINE logLevel #-}
logLevel :: IORef LogLevel
logLevel = unsafePerformIO (newIORef None)


data LogLevel = None | Error | Warning | Info | Debug
  deriving (Enum, Ord, Eq)

instance Show LogLevel where
  show None    = "NONE"
  show Error   = "ERROR"
  show Warning = "WARN"
  show Info    = "INFO"
  show Debug   = "DEBUG"

instance Read LogLevel where
  readsPrec _ s
        | l `elem` ["none"]            = [ (None, "") ]
        | l `elem` ["error"]           = [ (Error, "") ]
        | l `elem` ["warn", "warning"] = [ (Warning, "") ]
        | l `elem` ["info"]            = [ (Info, "") ]
        | l `elem` ["debug"]           = [ (Debug, "") ]
        | otherwise = [ ]
        where l = map toLower s

instance ToJSON LogLevel where
  toJSON = JSON.String . T.pack . show

instance FromJSON LogLevel where
  parseJSON (JSON.String s) =
    maybe (fail $ "unknown log level: " ++ show s) return (readMaybe . T.unpack $ s)
  parseJSON _ = empty


data Message = Message LogLevel String

instance Show Message where
  show (Message lvl str) = show lvl ++ ": " ++ str

