{-# LANGUAGE QuasiQuotes #-}
module Main (
  main
) where

import Data.Maybe (fromJust)
import Data.Version (showVersion)
import Paths_sproxy2 (version) -- from cabal
import System.Environment (getArgs)
import Text.InterpolatedString.Perl6 (qc)
import qualified System.Console.Docopt.NoTH as O

import Sproxy.Server (server)

usage :: String
usage =  "sproxy2 " ++ showVersion version ++
  " - HTTP proxy for authenticating users via OAuth2" ++ [qc|

Usage:
  sproxy2 [options]

Options:
  -c, --config=FILE        Configuration file [default: sproxy.yml]
  -h, --help               Show this message

|]

main :: IO ()
main = do
  doco <- O.parseUsageOrExit usage
  args <- O.parseArgsOrExit doco =<< getArgs
  if args `O.isPresent` O.longOption "help"
  then putStrLn $ O.usage doco
  else do
    let configFile = fromJust . O.getArg args $ O.longOption "config"
    server configFile

