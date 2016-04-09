module Main
(
  main
) where

import Options.Applicative

import Authorize (withDatabaseAuthorizeAction)
import ConfigFile (withConfigFile, ConfigFile(..))
import Proxy (run)

main :: IO ()
main = do
  configFile <- execParser options
  withConfigFile configFile $ \config ->
    withDatabaseAuthorizeAction (cfDatabase config) (run config)
  where
    parser = strOption (
         long "config"
      <> noArgError ShowHelpText
      <> metavar "CONFIG"
      <> value "config/sproxy.yml"
      <> help "config file path"
      )
    options = info parser (fullDesc <> progDesc "sproxy: proxy for single sign-on")
