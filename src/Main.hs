module Main where

import qualified Anf
import qualified Cfa
import qualified Cps
import qualified Defun
import qualified Parser
import Polysemy
import Pretty
import Syntax (FreshVar, runFreshVar)
import System.Environment (getArgs)

main :: IO ()
main = do
  [file] <- getArgs
  runTest file

runTest :: FilePath -> IO ()
runTest f = runM . runFreshVar . test $ f

test :: Members '[Embed IO, FreshVar] r => FilePath -> Sem r ()
test file = do
  pgm <- Parser.fromFile file
  pgm `seq` putTextLn "Parsed source"
  anf <- Anf.fromSource pgm
  anf `seq` putTextLn "Transformed to Anf"
  cps <- Cps.fromAnf anf
  cps `seq` putTextLn "Transformed to Cps"
  (labeled, analysis) <- Cfa.analyse cps
  defun <- Defun.fromLabeled labeled analysis
  defun `seq` putTextLn "Defunctionalized"
  pprint defun
-- cpsRes <- Eval.run cps
-- pprint cpsRes
-- if cpsRes == anfRes && anfRes == pRes
-- then pure ()
-- else throwError "Mismatched results"
