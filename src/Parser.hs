{-# LANGUAGE LambdaCase #-}

module Parser
  ( program )
where

import           Control.Applicative     hiding ( Const
                                                , many
                                                , some
                                                )
import           Control.Monad
import qualified Control.Monad.Combinators.NonEmpty
                                               as NE
import           Control.Monad.State
import           Data.Bifunctor
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Void
import           Syntax.Base
import           Syntax.Scope
import           Syntax.Surface
import           Text.Megaparsec         hiding ( State(..)
                                                , Label
                                                )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser a = ParsecT Void String (State Metadata) a

node :: Parser Label
node = state nextLabel

binder :: NonEmpty String -> Parser (Expr String) -> Parser (Scope Expr String)
binder xs expr = do
  s <- state $ nextScope (Just xs)
  bind s xs <$> expr

keywords :: [String]
keywords = ["def", "fun", "err", "match"]

symbol :: String -> Parser String
symbol = L.symbol space

keyword :: String -> Parser String
keyword s = lexeme $ try (string s <* notFollowedBy alphaNumChar)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

ident :: Parser String
ident =
  try
    $   mfilter (not . flip elem keywords)
    $   lexeme
    $   (:)
    <$> lowerChar
    <*> many alphaNumChar

cons :: Parser Cons
cons = lexeme $ MkCons <$> ((:) <$> upperChar <*> many alphaNumChar)

topLevel :: Parser (Def Expr String)
topLevel = keyword "def" >> do
  name <- ident
  args <- NE.some ident <* symbol "->"
  body <- binder args expr
  return $ Def name body

stringLiteral :: Parser String
stringLiteral = lexeme $ char '\"' *> manyTill L.charLiteral (char '\"')

expr :: Parser (Expr String)
expr = exprs >>= \case
  x                  :| []       -> pure x
  (Const l (Cons c)) :| (y : ys) -> pure $ CApp l c (y :| ys)
  x                  :| (y : ys) -> do
    l <- node
    pure $ App l x (y :| ys)

exprs :: Parser (NonEmpty (Expr String))
exprs = NE.some atom

atom :: Parser (Expr String)
atom = choice
  [ Var <$> node <*> ident
  , Const <$> node <*> constant
  , parens expr
  , keyword "err" >> (Err <$> node <*> lexeme stringLiteral)
  , keyword "fun" >> do
    args <- NE.some ident <* symbol "->"
    l    <- node
    body <- binder args expr
    return $ Lambda l body
  , Parser.match
  ]

match :: Parser (Expr String)
match = keyword "match" >> (Case <$> node <*> expr <*> many Parser.pattern)

pattern :: Parser (Pattern Expr String)
pattern = symbol "|" >> choice
  [ try $ do
    tag  <- cons
    args <- NE.some ident
    symbol "->"
    body <- binder args expr
    return $ PatCons tag body
  , PatConst <$> constant <*> (symbol "->" >> expr)
  , PatWild <$> (symbol "_" >> symbol "->" >> expr)
  ]

constant :: Parser Const
constant =
  choice [Int <$> lexeme L.decimal, String <$> stringLiteral, Cons <$> cons]

program :: String -> Either String (Program Expr)
program s = case e of
  Left  err -> Left $ errorBundlePretty err
  Right v   -> Right $ Program v m
 where
  a      = runParserT (many topLevel <* eof) "stdin" s
  (e, m) = runState a initMetadata
