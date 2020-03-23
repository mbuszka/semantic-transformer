module Pretty
  ( aligned,
    aligned',
    body,
    nested,
    nested',
    rows,
    -- reexports
    (<+>),
    braces,
    brackets,
    escape,
    hardline,
    parens,
  )
where

import Data.Text.Prettyprint.Doc
import qualified Data.Text as Text

rows :: [Doc ann] -> Doc ann
rows = concatWith (\x y -> x <> hardline <> y)

body :: Doc ann -> Doc ann -> Doc ann
body params rest = group $ flatAlt normal compact
  where
    normal = nested 4 params <> nest 2 (hardline <> rest)
    compact = space <> params <+> rest

nested :: Int -> Doc ann -> Doc ann
nested n x = group (flatAlt (nest n (hardline <> group x)) (space <> x))

nested' :: Pretty a => Int -> [a] -> Doc ann
nested' _ [] = mempty
nested' n xs = nested n . sep . fmap pretty $ xs

aligned :: Pretty a => [a] -> Doc ann
aligned = align . sep . fmap pretty

aligned' :: [Doc ann] -> Doc ann
aligned' = align . sep

escape :: Text -> Doc ann
escape = pretty . show . Text.unpack