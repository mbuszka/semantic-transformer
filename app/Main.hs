{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Anf
import qualified Cps
import qualified Eval
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Parser
import Syntax
import Control.Monad.Except

main :: IO ()
main = return ()

run :: String -> IO ()
run f = do
  res <- runExceptT . runStxT . test $ f
  case res of
    Left err -> putTextLn err
    Right () -> pure ()

test :: (MonadStx m, MonadIO m, MonadError Text m) => String -> m ()
test file = do
  pgm <- readFileText file
  putTextLn "File read"
  let p = runStx . Parser.run file $ pgm
  seq p (putStrLn "Parsed program")
  pprint p
  pRes <- Eval.run p
  pprint pRes
  anf <- Anf.fromSource p
  putTextLn "Transformed to ANF"
  pprint anf
  anfRes <- Eval.run anf
  pprint anfRes
  cps <- Cps.fromAnf anf
  putTextLn "Transformed to CPS"
  pprint cps
  cpsRes <- Eval.run cps
  pprint cpsRes
  if cpsRes == anfRes && anfRes == pRes
  then pure ()
  else throwError "Mismatched results"
  

