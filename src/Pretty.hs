module Pretty
  ( aligned,
    body,
    nested,
    nested',
    rows,
    -- reexports
    (<+>),
    braces,
    hardline,
    parens,
  )
where

import Data.Text.Prettyprint.Doc

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
nested' n = nested n . sep . fmap pretty

aligned :: Pretty a => [a] -> Doc ann
aligned = align . sep . fmap pretty