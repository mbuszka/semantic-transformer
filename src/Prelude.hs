module Prelude
  ( module Control.Applicative,
    module Control.Monad,
    module Control.Monad.IO.Class,
    module Data.Either,
    module Data.Foldable,
    module Data.Function,
    module Data.Functor,
    module Data.List,
    module Data.Map,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Semigroup,
    module Data.Set,
    module Data.Sequence,
    module Data.Text,
    module Data.Text.Prettyprint.Doc,
    module Data.Traversable,
    module Data.Tuple,
    module Data.Void,
    module GHC.Base,
    module GHC.IO,
    module GHC.Num,
    module GHC.Real,
    module GHC.Show,
    module Polysemy,
    error,
    pprint,
    pprint',
    pshow,
    pshow',
    putTextLn,
    readFile,
  )
where

import Control.Applicative
  ( Applicative ((*>), (<*), (<*>), pure),
    liftA2,
    liftA3,
  )
import Control.Monad ((<=<), (=<<), (>=>), mfilter, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (Either (..))
import Data.Foldable (Foldable (..), for_, toList, traverse_)
import Data.Function ((&), (.), const)
import Data.Functor (($>), (<$>), (<&>), Functor (..), void)
import Data.List (reverse, take, zip)
import Data.Map (Map)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup ((<>), Semigroup)
import Data.Sequence (Seq (..))
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc (Doc, Pretty (..))
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Traversable (Traversable (..), for)
import Data.Tuple (fst, snd, swap)
import Data.Void (Void)
import GHC.Base
  ( ($),
    (&&),
    Bool (..),
    Char,
    Eq ((/=), (==)),
    IO,
    Int,
    Maybe (..),
    Monad (..),
    Ord (..),
    Type,
    flip,
    not,
    otherwise,
    seq,
    (||),
  )
import qualified GHC.Base as Base
import GHC.IO (FilePath)
import GHC.Num ((*), (+), (-), Num)
import GHC.Real (Integral(..))
import GHC.Show (Show (show))
import GHC.Stack (HasCallStack)
import Polysemy

pprint' :: MonadIO m => Doc ann -> m ()
pprint' = liftIO . Text.putStrLn . pshow'

pprint :: (MonadIO m, Pretty a) => a -> m ()
pprint = pprint' . pretty

pshow' :: Doc ann -> Text
pshow' = renderStrict . PP.layoutPretty PP.defaultLayoutOptions

pshow :: Pretty a => a -> Text
pshow = pshow' . pretty

putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . Text.putStrLn

readFile :: MonadIO m => FilePath -> m Text
readFile = liftIO . Text.readFile

error :: HasCallStack => Text -> a
error = Base.error . Text.unpack
