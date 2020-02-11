module Main where

import qualified Analysis.ControlFlow as Cfa
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Options.Applicative
import Parser
import qualified Syntax.Anf as Anf
import qualified Syntax.Denormalized as D
import Syntax.Surface
import qualified Transform.Cps as Cps

data Repl = Repl {file :: String}

replParser :: Parser Repl
replParser = Repl <$> argument str (metavar "DEFNS")

opts =
  info
    (replParser <**> helper)
    ( fullDesc
        <> progDesc "Process definitions in file DEFNS"
        <> header "semt -- a semantic transformer"
    )

instance Pretty a => Pretty (Set.Set a) where
  pretty = group . vsep . fmap pretty . Set.toList

main :: IO ()
main = do
  Repl file <- execParser opts
  pgm <- readFile file
  case program pgm of
    Left err -> putStrLn err
    Right p -> do
      putStrLn "Loaded definitions:"
      putStrLn $ pprintPgm p
      let anf = Anf.fromSurface p
          back = Anf.toSurface anf
      putStrLn "A-normal form:"
      putStrLn $ pprintPgm back
      let den = D.program anf
          s = Cfa.appClosures den
      putStrLn "Results of analysis"
      putStrLn $ renderString . layoutSmart defaultLayoutOptions $
        vsep
          (pretty <$> Map.toList s)
      putStrLn "Cps transformed"
      putStrLn . pprintPgm . Anf.toSurface . Cps.program $ anf
-- putStrLn "\nCps transformed"
-- let cpsDefs = map Cps.top defs
-- mapM_ (\x -> pprintLn x >> putChar '\n') cpsDefs
-- putStrLn "Awaiting input"
-- step (buildEnv initial defs) (buildEnv (Cps.env initial) cpsDefs)

-- step :: Env -> Env -> IO ()
-- step env cpsEnv = do
--   putStr ">>> "
--   s <- getLine
--   case parseRepl s of
--     Left  err  -> putStrLn err
--     Right expr -> do
--       putStrLn "echo"
--       putStrLn . pprint $ expr
--       let cps = Cps.repl expr
--       let res = eval env expr pure
--       print res
--       putStrLn "cps"
--       putStrLn . pprint $ cps
--       print $ eval cpsEnv cps pure
--       step env cpsEnv
