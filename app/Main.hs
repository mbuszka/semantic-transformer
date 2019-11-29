module Main where

import           Options.Applicative
import           Parser
import           Syntax
import           Eval
import qualified Transform.Cps                 as Cps
import           Bind
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Text.Prettyprint.Doc      ( pretty
                                                , vsep
                                                )

import           Data.List.NonEmpty             ( NonEmpty(..) )

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
      mapM_ (\x -> pprintLn x >> putChar '\n') defs
      putStrLn "\nCps transformed"
      let cpsDefs = map Cps.top defs
      mapM_ (\x -> pprintLn x >> putChar '\n') cpsDefs
      putStrLn "Awaiting input"
      step (buildEnv initial defs) (buildEnv (Cps.env initial) cpsDefs)


step :: Env -> Env -> IO ()
step env cpsEnv = do
  putStr ">>> "
  s <- getLine
  case parseRepl s of
    Left  err  -> putStrLn err
    Right expr -> do
      putStrLn "echo"
      putStrLn . pprint $ expr
      let cps = Cps.repl expr
      let res = eval env expr id
      print res
      putStrLn "cps"
      putStrLn . pprint $ cps
      print $ eval cpsEnv cps id
      step env cpsEnv
