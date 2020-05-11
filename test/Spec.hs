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
      configDumpAnf = True
      configDumpCps = True
      configSkipCps = False
      configSkipDefun = False
      configOutputDir = Just outDir
  sources <- listDirectory srcDir
  for_ sources $ \file -> do
    putStrLn $ "Testing " <> file
    let configSource = srcDir </> file
        res = outDir </> file
        anf = outDir </> takeBaseName file <> "-anf.rkt"
        cps = outDir </> takeBaseName file <> "-cps.rkt"
        config = Config {..}
    putStrLn =<< readProcess "raco" ["test", configSource] ""
    maybeErr <- Pipeline.run config
    case maybeErr of
      Left err -> TIO.putStrLn err
      Right () -> do
        putStrLn =<< readProcess "raco" ["test", anf] ""
        putStrLn =<< readProcess "raco" ["test", cps] ""
        putStrLn =<< readProcess "raco" ["test", res] ""
    
