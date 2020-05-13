module Pipeline (Config (..), run) where

import Data.Bifunctor
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO
import qualified Parser
import qualified Pipeline.Anf as Anf
import qualified Pipeline.Cps as Cps
import qualified Pipeline.InlineLet as InlineLet
import qualified Pipeline.Structure as Structure
import Polysemy.Error
import Syntax
import Syntax.Pretty
import Syntax.Source
import qualified Pipeline.Defun as Defun
import System.Directory
import System.FilePath
import System.IO (putStrLn)
import System.Process
import Common

data Transform = Anf | Cps | Defun | Inline
  deriving (Eq, Ord)

data Config = Config
  { configSource :: FilePath,
    configOutputDir :: Maybe FilePath,
    configIntermediate :: Bool,
    configDebug :: Bool,
    configSelfTest :: Bool,
    configCustom :: Maybe [Transform]
  }

data Runtime = Runtime
  { runtimeBaseName :: FilePath,
    runtimeOutputDir :: FilePath,
    runtimePgmPrefix :: Text,
    runtimePgmSuffix :: Text,
    runtimeStageCnt :: IORef (Map Transform Int),
    runtimePrintIntermediate :: Bool,
    runtimePrintDebug :: Bool,
    runtimeSelfTest :: Bool
  }

type Effs r = Members '[Embed IO, FreshVar, FreshLabel, Error Err] r

run :: Config -> IO (Either Text ())
run config = do
  res <-
    runFinal
      . embedToFinal
      . errorToIOFinal
      . runFreshVar
      . runFreshLabel
      $ start config
  pure $ first pshow res

printProgram :: Effs r => Runtime -> FilePath -> Program Term -> Sem r ()
printProgram Runtime {..} fileSuffix pgm = do
  let file = runtimeOutputDir </> runtimeBaseName <> fileSuffix <.> "rkt"
      p = pshow' $ prettyProgram pgm
  embed . TIO.writeFile file $ runtimePgmPrefix <> p <> runtimePgmSuffix

printDebug :: Effs r => Runtime -> FilePath -> Program Term -> Sem r ()
printDebug Runtime {..} fileSuffix pgm = do
  let file = runtimeOutputDir </> runtimeBaseName <> fileSuffix <.> "dbg"
      p = pshow' $ debugProgram pgm
  embed . TIO.writeFile file $ p

getSuffix :: Effs r => Runtime -> Transform -> Sem r FilePath
getSuffix r t = do
  cnts <- embed $ readIORef (runtimeStageCnt r)
  cnt <- Map.lookup t cnts & \case
    Nothing -> do
      embed $ modifyIORef (runtimeStageCnt r) (Map.insert t 1)
      pure ""
    Just n -> do
      embed $ modifyIORef (runtimeStageCnt r) (Map.insert t (n + 1))
      pure $ "-" <> show n
  let s = case t of
        Anf -> "-anf"
        Cps -> "-cps"
        Defun -> "-defun"
        Inline -> "-inline"
  pure $ s <> cnt

start :: Effs r => Config -> Sem r ()
start Config {..} = do
  txt <- embed $ TIO.readFile configSource
  let runtimeBaseName = takeBaseName configSource
      runtimeOutputDir = fromMaybe "out" configOutputDir
      runtimePrintIntermediate = configIntermediate || configSelfTest
      runtimePrintDebug = configDebug
      runtimeSelfTest = configSelfTest
  SrcProgram {..} <- fromEither $ Parser.run configSource txt
  let runtimePgmPrefix = srcPrologue
      runtimePgmSuffix = srcEpilogue
  runtimeStageCnt <- embed $ newIORef Map.empty
  validated <- Structure.validate srcProgram
  when (runtimeSelfTest) do
    embed $ putStrLn =<< readProcess "raco" ["test", configSource] ""
  let stages = fromMaybe [Anf, Cps, Defun, Inline] configCustom
  runStages Runtime {..} stages validated

apply :: Effs r => Transform -> Program Term -> Sem r (Program Term)
apply Anf p = Anf.transform p
apply Cps p = Cps.fromAnf p
apply Defun p = Defun.transform p
apply Inline p = traverse InlineLet.transform p

runStages :: Effs r => Runtime -> [Transform] -> Program Term -> Sem r ()
runStages r [] pgm = do
  mkdir (runtimeOutputDir r)
  printProgram r "" pgm
runStages r (t : ts) pgm = do
  pgm' <- apply t pgm
  when (runtimePrintDebug r || runtimePrintIntermediate r) do
    mkdir (runtimeOutputDir r)
    fileSuffix <- getSuffix r t
    when (runtimePrintDebug r) do
      printDebug r fileSuffix pgm'
    when (runtimePrintIntermediate r) do
      printProgram r fileSuffix pgm'
    when (runtimeSelfTest r) do
      let file = (runtimeOutputDir r) </> runtimeBaseName r <> fileSuffix <.> "rkt"
      embed $ putStrLn =<< readProcess "raco" ["test", file] ""
  runStages r ts pgm'

mkdir :: Member (Embed IO) r => FilePath -> Sem r ()
mkdir out = embed $ createDirectoryIfMissing True out