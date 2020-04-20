module Main where

import System.Environment (getArgs)
import Pipeline

main :: IO ()
main = do
  config <- parseConfig
  run config

parseConfig :: IO Config
parseConfig = do
  [configSource, configOutput] <- getArgs
  pure Config {..}
  