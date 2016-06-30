{-# LANGUAGE QuasiQuotes #-}
module Main
(
  main
) where

import Data.Maybe (fromJust)
import Data.Version (showVersion)
import Paths_sproxy (version) -- from cabal
import System.Environment (getArgs)
import Text.RawString.QQ (r)
import qualified System.Console.Docopt.NoTH as O

import Authorize (withDatabaseAuthorizeAction)
import ConfigFile (withConfigFile, ConfigFile(..))
import Proxy (run)

usage :: String
usage =  "SProxy " ++ showVersion version ++
  " HTTP proxy for authenticating users via OAuth2" ++ [r|

Usage:
  sproxy [options]

Options:
  -c, --config=FILE        Configuration file [default: config/sproxy.yml]
  -h, --help               Show this message

|]

main :: IO ()
main = do
  doco <- O.parseUsageOrExit usage
  args <- O.parseArgsOrExit doco =<< getArgs
  if args `O.isPresent` O.longOption "help"
  then putStrLn $ O.usage doco
  else do
    let
      configFile = fromJust . O.getArg args $ O.longOption "config"

    withConfigFile configFile $ \config ->
      withDatabaseAuthorizeAction (cfDatabase config) (run config)

