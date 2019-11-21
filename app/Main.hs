module Main where

import           Options.Applicative
import           Parser
import           Syntax


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
  pgm <- readFile file
  case parseDefs pgm of
    Left  err  -> putStrLn err
    Right defs -> do
      print defs
      step $ buildEnv defs
      
      
step :: Env -> IO ()
step e = do
  s <- getLine
  case parseRepl s of
    Left err -> putStrLn err
    Right expr -> do
      print expr
      let res = eval e expr id
      print res
      step e
