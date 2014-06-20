module Main where

import           Options.Applicative

import           Proxy
import           ConfigFile

main :: IO ()
main = do
  configFile <- execParser options
  withConfigFile configFile run
  where
    parser = strOption (
         long "config"
      <> noArgError ShowHelpText
      <> metavar "CONFIG"
      <> value "config/sproxy.yml"
      <> help "config file path"
      )
    options = info parser (fullDesc <> progDesc "sproxy: proxy for single sign-on")
