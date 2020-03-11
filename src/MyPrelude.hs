module MyPrelude
  ( module X,
    pprint,
    pshow,
  )
where

import Control.Monad.IO.Class
import Control.Applicative as X (liftA2, liftA3)
import Data.Foldable as X (toList)
import Data.Functor as X (($>))
import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Text as X (Text)
import Data.Text.IO as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Void as X (Void)
import Prelude as X hiding (lookup)
import Control.Monad as X ((<=<), (>=>))

pprint :: (MonadIO m, Pretty a) => a -> m ()
pprint = liftIO . Text.putStrLn . pshow

pshow :: Pretty a => a -> Text
pshow = renderStrict . layoutSmart defaultLayoutOptions . pretty
