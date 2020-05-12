{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable
import Pipeline
import System.Directory
import System.FilePath
import System.Process
import System.IO
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  let outDir = "interpreters/out"
      srcDir = "interpreters/src"
      configIntermediate = True
      configDebug = True
      configSelfTest = True
      configOutputDir = Just outDir
      configCustom = Nothing
  sources <- listDirectory srcDir
  for_ sources $ \file -> do
    let configSource = srcDir </> file
    maybeErr <- Pipeline.run Config {..}
    case maybeErr of
      Left err -> TIO.putStrLn err
      Right () -> pure ()
    
