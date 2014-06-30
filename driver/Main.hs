module Main (main) where

import           Network.HTTP.SProxy
import           Options.Applicative

import           Authorize

main :: IO ()
main = do
  configFile <- execParser options
  withConfigFile configFile $ \config ->
    run config authorize
  where
    parser = strOption (
         long "config"
      <> noArgError ShowHelpText
      <> metavar "CONFIG"
      <> value "config/sproxy.yml"
      <> help "config file path"
      )
    options = info parser (fullDesc <> progDesc "bob-auth")
