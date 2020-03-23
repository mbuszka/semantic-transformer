module Main where

import qualified Anf
-- import qualified Cfa
import qualified Cps
-- import qualified Defun
import qualified Parser
import qualified Eval
import Polysemy.Error
import Syntax
import System.Environment (getArgs)
import Util
import Pretty

main :: IO ()
main = do
  [file] <- getArgs
  runTest file

runTest :: FilePath -> IO ()
runTest f = do
  res <- runFinal . embedToFinal . errorToIOFinal . runFreshVar . test $ f
  case res of
    Left err -> pprint err
    Right () -> pure ()

runTests :: MonadIO m => Program Term -> m ()
runTests pgm = do
  putTextLn ""
  forM_ (Eval.tests pgm) \case 
    (name, res) -> do
      pprint' $
        "Running test:" <+> pretty name <+> "..." <> nested 2 (pretty res) <> hardline


test :: Members '[Embed IO, FreshVar, Error Err] r => FilePath -> Sem r ()
test file = do
  pgm <- Parser.fromFile file
  pgm `seq` putTextLn "Parsed source"
  pprint pgm
  runTests pgm
  anf <- Anf.fromSource pgm
  anf `seq` putTextLn "Transformed to Anf"
  pprint anf
  runTests (fmap Anf.toSource anf)
  cps <- Cps.fromAnf anf
  cps `seq` putTextLn "Transformed to Cps"
  pprint cps
  runTests cps
-- (labeled, analysis) <- Cfa.analyse cps
-- defun <- Defun.fromLabeled labeled analysis
-- defun `seq` putTextLn "Defunctionalized"
-- pprint defun
-- cpsRes <- Eval.run cps
-- pprint cpsRes
-- if cpsRes == anfRes && anfRes == pRes
-- then pure ()
-- else throwError "Mismatched results"
