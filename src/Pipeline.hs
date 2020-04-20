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
import qualified Pipeline.Cps as Cps
import qualified Pipeline.Defun as Defun
import qualified Syntax.Source as Source
import qualified Syntax.Scoped as Scoped

data Config
  = Config
      { configSource :: FilePath,
        configOutput :: FilePath
      }

run :: Config -> IO ()
run Config {..} = do
  res <-
    runFinal
      . embedToFinal
      . errorToIOFinal
      . runFreshVar
      $ test configSource configOutput
  case res of
    Left err -> pprint err
    Right () -> pure ()

test ::
  Members '[Embed IO, FreshVar, Error Err] r => FilePath -> FilePath -> Sem r ()
test file output = do
  txt <- embed $ TIO.readFile file
  SrcProgram {..} <- fromEither $ Parser.run file txt
  -- srcProgram `seq` putTextLn "Parsed source"
  validated <- Structure.validate srcProgram
  scoped <- Scope.checkProgram Source.unwrap Scoped.fromSource validated
  anf <- Anf.transform scoped
  cps <- Cps.fromAnf anf
  def <- Defun.transform cps
  embed $ TIO.writeFile output (srcPrologue <> pshow def <> srcEpilogue)
  pure ()
