module Main where

import qualified Anf
import qualified Cps
-- import qualified Eval
import qualified Parser
import qualified Data.Text.IO as Text
import Syntax
import Control.Monad.Except
import MyPrelude
import qualified Cfa
import System.Environment (getArgs)


main :: IO ()
main = do
  [file] <- getArgs
  run file

run :: String -> IO ()
run f = do
  res <- runExceptT . runStxT . test $ f
  case res of
    Left err -> Text.putStrLn err
    Right () -> pure ()

putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . Text.putStrLn

test :: (MonadStx m, MonadIO m, MonadError Text m) => String -> m ()
test file = do
  pgm <- liftIO $ Text.readFile file
  putTextLn "File read"
  p <- Parser.run file $ pgm
  seq p (liftIO $ putStrLn "Parsed program")
  pprint p
  Cfa.analyse p >>= pprint
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
  Cfa.analyse cps >>= pprint
  -- cpsRes <- Eval.run cps
  -- pprint cpsRes
  -- if cpsRes == anfRes && anfRes == pRes
  -- then pure ()
  -- else throwError "Mismatched results"
  

