module Main where

import           Options.Applicative
import           Parser
import           Syntax
import Eval
import Transform.Cps
import Bind
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Text.Prettyprint.Doc      ( pretty
                                                , vsep
                                                )

import Data.List.NonEmpty (NonEmpty(..))

data Repl = Repl { file :: String }

replParser :: Parser Repl
replParser = Repl <$> argument str (metavar "DEFNS")

opts = info
  (replParser <**> helper)
  (  fullDesc
  <> progDesc "Run repl initialized with definitions in DEFNS"
  <> header "semt -- a semantic transformer"
  )

main :: IO ()
main = do
  Repl file <- execParser opts
  pgm       <- readFile file
  case parseDefs pgm of
    Left  err  -> putStrLn err
    Right defs -> do
      putStrLn "Loaded definitions:"
      print $ vsep (map pretty defs)
      putStrLn "\nCps transformed"
      let cpsDefs = map (\t -> simplifyTop $ evalState (cpsTop t) 0) defs
      print $ vsep (map pretty cpsDefs)
      putStrLn "Awaiting input"
      step (buildEnv initial defs) (buildEnv (cpsEnv initial) cpsDefs)


step :: Env -> Env -> IO ()
step env cpsEnv = do
  putStr ">>> "
  s <- getLine
  case parseRepl s of
    Left  err  -> putStrLn err
    Right expr -> do
      putStrLn "echo"
      print $ pretty expr
      let cps  = simplify $ evalState (cpsTransform (Lambda ("x" :| []) (Scope (Var . B $ 0))) expr) 0
      let res = eval env expr id
      print res
      putStrLn "cps"
      print $ pretty cps
      print $ isCps cps
      print $ eval cpsEnv cps id
      step env cpsEnv
