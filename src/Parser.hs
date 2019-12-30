{-# LANGUAGE LambdaCase #-}

module Parser
  ( program
  )
where

import           Control.Applicative     hiding ( Const
                                                , many
                                                , some
                                                )
import           Control.Monad
import qualified Control.Monad.Combinators.NonEmpty
                                               as NE
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Foldable
import           Data.Void
import           Syntax.Base
import           Syntax.Surface
import           Text.Megaparsec         hiding ( State(..)
                                                , ELabel
                                                )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser a = ParsecT Void String (ReaderT Env (State Metadata)) a

type Env = Map String Var

node :: Parser ELabel
node = state nextLabel

withBindings :: SLabel -> NonEmpty String -> Env -> Env
withBindings s xs e =
  let vs = zip (toList xs) (map (Local s) [0 ..])
  in  Map.union (Map.fromList vs) e

binder :: NonEmpty String -> Parser Expr -> Parser (Scope Expr)
binder xs expr = do
  s <- state $ nextScope (Just xs)
  Scope s (length xs) <$> local (withBindings s xs) expr

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

var :: Parser Var
var = do
  s <- ident
  e <- ask
  return $ Map.findWithDefault (Global s) s e

cons :: Parser Tag
cons = lexeme $ MkTag <$> ((:) <$> upperChar <*> many alphaNumChar)

topLevel :: Parser (Def Expr)
topLevel = keyword "def" >> do
  name <- ident
  args <- NE.some ident <* symbol "->"
  body <- binder args expr
  return $ Def name body

stringLiteral :: Parser String
stringLiteral = lexeme $ char '\"' *> manyTill L.charLiteral (char '\"')

expr :: Parser Expr
expr = exprs >>= \case
  x                  :| []       -> pure x
  (Constant l (Tag c)) :| (y : ys) -> pure $ CApp l c (y :| ys)
  x                  :| (y : ys) -> do
    l <- node
    pure $ App l x (y :| ys)

exprs :: Parser (NonEmpty Expr)
exprs = NE.some atom

atom :: Parser Expr
atom = choice
  [ Var <$> node <*> var
  , Constant <$> node <*> constant
  , parens expr
  , keyword "err" >> (Err <$> node <*> lexeme stringLiteral)
  , keyword "fun" >> do
    args <- NE.some ident <* symbol "->"
    l    <- node
    body <- binder args expr
    return $ Lambda l body
  , Parser.match
  ]

match :: Parser Expr
match = keyword "match" >> (Case <$> node <*> expr <*> many Parser.pattern)

pattern :: Parser (Pattern Expr)
pattern = symbol "|" >> choice
  [ try $ do
    tag  <- cons
    args <- NE.some ident
    symbol "->"
    body <- binder args expr
    return $ PatConstructor tag body
  , PatConst <$> constant <*> (symbol "->" >> expr)
  , PatWild <$> (symbol "_" >> symbol "->" >> expr)
  ]

constant :: Parser Constant
constant =
  choice [Int <$> lexeme L.decimal, String <$> stringLiteral, Tag <$> cons]

program :: String -> Either String (Program Expr)
program s = case e of
  Left  err -> Left $ errorBundlePretty err
  Right v   -> Right $ Program v m
 where
  a      = runParserT (many topLevel <* eof) "stdin" s
  (e, m) = runState (runReaderT a Map.empty) initMetadata
