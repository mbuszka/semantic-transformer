module Main where

import qualified Anf
import qualified Cps
import qualified Parser
import qualified Data.Text.IO as Text
import qualified Syntax.Labeled as Labeled
import Syntax
import Control.Monad.Except
import MyPrelude
import qualified Cfa
import qualified Defun
import System.Environment (getArgs)
import Polysemy


main :: IO ()
main = do
  [file] <- getArgs
  runTest file

putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . Text.putStrLn

runTest :: String -> IO ()
runTest f = runM . runFreshVar . test $ f

test :: Members '[Embed IO, FreshVar] r => String -> Sem r ()
test file = do
  pgm <- liftIO $ Text.readFile file
  putTextLn "File read"
  let p = Parser.run file $ pgm
  seq p (liftIO $ putStrLn "Parsed program")
  pprint p
  -- pRes <- Eval.run p
  -- pprint pRes
  anf <- Anf.fromSource p
  putTextLn "Transformed to ANF"
  pprint anf
  -- anfRes <- Eval.run anf
  -- pprint anfRes
  cps <- Cps.fromAnf anf
  putTextLn "Transformed to CPS"
  pprint cps
  (labeled, analysis) <- Cfa.analyse cps
  pprint analysis
  defun <- Defun.fromLabeled labeled analysis
  pprint defun
  -- cpsRes <- Eval.run cps
  -- pprint cpsRes
  -- if cpsRes == anfRes && anfRes == pRes
  -- then pure ()
  -- else throwError "Mismatched results"
  

