module Main where

import Pipeline
import Options.Applicative

main :: IO ()
main = do
  config <- execParser parser
  Pipeline.run config

parser :: ParserInfo Config
parser = info (parseConfig <**> helper) desc

parseConfig :: Parser Config
parseConfig = Config
  <$> argument str (metavar "FILE" <> help "Source file with the interpreter")
  <*> optional (strOption (short 'o' <> long "output" <> metavar "DIR" <> help "Output directory for generated files, defaults to ./out/"))
  <*> switch (long "dump-anf" <> help "Dump program after transformation to administrative normal form")
  <*> switch (long "dump-cps" <> help "Dump program after transformation to continuation passing style")

desc :: InfoMod a
desc = fullDesc
  <> progDesc "Transform an interpreter into an abstract machine"
  <> header "semt -- a semantic transformer"
  