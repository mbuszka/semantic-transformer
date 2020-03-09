module Prelude (module Relude, pprint, pshow) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Relude hiding (group, many)

pprint :: (MonadIO m, Pretty a) => a -> m ()
pprint = putTextLn . pshow

pshow :: Pretty a => a -> Text
pshow = renderStrict . layoutSmart defaultLayoutOptions . pretty
