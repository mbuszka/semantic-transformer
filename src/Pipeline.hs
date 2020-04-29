module Pipeline (Config (..), run) where

import Polysemy.Error
import Syntax
import Syntax.Source
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

data Config
  = Config
      { configSource :: FilePath,
        configOutputDir :: Maybe FilePath,
        configDumpAnf :: Bool,
        configDumpCps :: Bool
      }

run :: Config -> IO ()
run config = do
  res <-
    runFinal
      . embedToFinal
      . errorToIOFinal
      . runFreshVar
      . runFreshLabel
      $ runEffs config
  case res of
    Left err -> pprint err
    Right () -> pure ()

runEffs :: Members '[Embed IO, FreshVar, FreshLabel, Error Err] r => Config -> Sem r ()
runEffs Config {..} = do
  txt <- embed $ TIO.readFile configSource
  let out = fromMaybe "out" configOutputDir
      resFile = out </> takeFileName configSource
  SrcProgram {..} <- fromEither $ Parser.run configSource txt
  let srcName = takeBaseName configSource
  validated <- Structure.validate srcProgram
  scoped <- Scope.fromSource validated
  anf <- Anf.transform scoped
  when configDumpAnf do
    let anfFile = out </> srcName <> "-anf" <.> "rkt"
    mkdir out
    embed $ TIO.writeFile anfFile (srcPrologue <> pshow anf <> srcEpilogue)
  cps <- Cps.fromAnf anf
  when configDumpCps do
    let cpsFile = out </> srcName <> "-cps" <.> "rkt"
    mkdir out
    embed $ TIO.writeFile cpsFile (srcPrologue <> pshow cps <> srcEpilogue)
  def <- Defun.transform cps
  res <- traverse Inline.transform def
  mkdir out
  embed $ TIO.writeFile resFile (srcPrologue <> pshow res <> srcEpilogue)
  pure ()

mkdir :: Member (Embed IO) r => FilePath -> Sem r ()
mkdir out = embed $ createDirectoryIfMissing True out