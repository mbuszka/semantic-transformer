module Main where

import Pipeline
import Options.Applicative
import qualified Data.Text.IO as TIO
import System.Exit


main :: IO ()
main = do
  config <- execParser parser
  res <- Pipeline.run config
  case res of
    Left err -> TIO.putStrLn err >> exitFailure
    Right () -> pure ()

parser :: ParserInfo Config
parser = info (parseConfig <**> helper) desc

parseConfig :: Parser Config
parseConfig = Config
  <$> argument str (metavar "FILE" <> help "Source file with the interpreter")
  <*> optional (strOption (short 'o' <> long "output" <> metavar "DIR" <> help "Output directory for generated files, defaults to ./out/"))
  <*> switch (long "intermediate" <> short 'i' <> help "Emit executable source files for each stage")
  <*> switch (long "debug" <> short 'd' <> help "Emit labeled source files for each stage")
  <*> switch (long "self-test" <> short 't' <> help "Run raco test on each intermediate file; implies --intermediate")
  <*> pure Nothing

desc :: InfoMod a
desc = fullDesc
  <> progDesc "Transform an interpreter into an abstract machine"
  <> header "semt -- a semantic transformer"
  