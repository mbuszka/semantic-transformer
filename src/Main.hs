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
  <$> strOption (short 's' <> long "source" <> metavar "FILE" <> help "Source file with the interpreter")
  <*> strOption (short 'o' <> long "output" <> metavar "FILE" <> help "Desired output file for the abstract machine")
  <*> strOption (long "dump-dir" <> metavar "DIR" <> value "" <> showDefault <> help "Target directory for intermediate representation dumps")
  <*> switch (long "dump-anf" <> help "Dump program after transformation to administrative normal form")
  <*> switch (long "dump-cps" <> help "Dump program after transformation to continuation passing style")

desc :: InfoMod a
desc = fullDesc
  <> progDesc "Transform an interpreter into an abstract machine"
  <> header "semt -- a semantic transformer"
  