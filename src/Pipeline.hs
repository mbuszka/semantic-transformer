module Pipeline (Config (..), run) where

import Polysemy.Error
import Syntax
import Syntax.Source
import Syntax.Pretty
import Util
import qualified Data.Text.IO as TIO
import qualified Parser
import qualified Pipeline.Scope as Scope
import qualified Pipeline.Structure as Structure
import qualified Pipeline.Anf as Anf
import qualified Pipeline.Inline as Inline
import qualified Pipeline.Cps as Cps
import qualified Pipeline.Defun as Defun
import System.Directory
import System.FilePath
import Data.Bifunctor

data Config
  = Config
      { configSource :: FilePath,
        configOutputDir :: Maybe FilePath,
        configSkipCps :: Bool,
        configSkipDefun :: Bool,
        configDumpAnf :: Bool,
        configDumpCps :: Bool
      }

run :: Config -> IO (Either Text ())
run config = do
  res <-
    runFinal
      . embedToFinal
      . errorToIOFinal
      . runFreshVar
      . runFreshLabel
      $ runEffs config
  pure $ first pshow res

runEffs :: Members '[Embed IO, FreshVar, FreshLabel, Error Err] r => Config -> Sem r ()
runEffs Config {..} = do
  txt <- embed $ TIO.readFile configSource
  let out = fromMaybe "out" configOutputDir
      resFile = out </> takeFileName configSource
  SrcProgram {..} <- fromEither $ Parser.run configSource txt
  let srcName = takeBaseName configSource
  let printProgram :: Member (Embed IO) r => FilePath -> Program Term -> Sem r ()
      printProgram file pgm = do
        let p = pshow' $ prettyProgram pgm
        embed . TIO.writeFile file $ srcPrologue <> p <> srcEpilogue
      
      printDebug :: Member (Embed IO) r => FilePath -> Program Term -> Sem r ()
      printDebug file pgm = do
        let p = pshow' $ debugProgram pgm
        embed . TIO.writeFile file $ srcPrologue <> p <> srcEpilogue
  validated <- Structure.validate srcProgram
  scoped <- Scope.fromSource validated
  anf <- Anf.transform scoped
  when configDumpAnf do
    let anfFile = out </> srcName <> "-anf" <.> "rkt"
    mkdir out
    printProgram anfFile anf
  cps <- if configSkipCps then pure anf else Cps.fromAnf anf
  when configDumpCps do
    let cpsFile = out </> srcName <> "-cps" <.> "rkt"
    let cpsDbg = out </> srcName <> "-dbg-cps" <.> "rkt"
    mkdir out
    printProgram cpsFile cps
    printDebug cpsDbg cps
  def <- if configSkipDefun then pure cps else Defun.transform cps
  -- res <- traverse Inline.transform def
  mkdir out
  printProgram resFile def -- res
  pure ()

mkdir :: Member (Embed IO) r => FilePath -> Sem r ()
mkdir out = embed $ createDirectoryIfMissing True out