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
import qualified Syntax.Source as Source
-- import qualified Syntax.Scoped as Scoped
import System.FilePath
import Util.Pretty

data Config
  = Config
      { configSource :: FilePath,
        configOutput :: FilePath,
        configDumpDir :: FilePath,
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
  SrcProgram {..} <- fromEither $ Parser.run configSource txt
  let srcName = takeBaseName configSource
  validated <- Structure.validate srcProgram
  scoped <- Scope.fromSource validated
  anf <- Anf.transform scoped
  when configDumpAnf do
    let anfFile = configDumpDir </> srcName <> "-anf" <.> "rkt"
    embed $ TIO.writeFile anfFile (srcPrologue <> pshow anf <> srcEpilogue)
  cps <- Cps.fromAnf anf
  when configDumpCps do
    let cpsFile = configDumpDir </> srcName <> "-cps" <.> "rkt"
    embed $ TIO.writeFile cpsFile (srcPrologue <> pshow cps <> srcEpilogue)
  def <- Defun.transform cps
  res <- traverse Inline.transform def
  embed $ TIO.writeFile configOutput (srcPrologue <> pshow res <> srcEpilogue)
  pure ()
