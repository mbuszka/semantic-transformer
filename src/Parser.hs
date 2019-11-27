{-# LANGUAGE LambdaCase #-}

module Parser
  ( parseDefs
  , parseRepl
  )
where

import           Control.Applicative     hiding ( many
                                                , some
                                                , Const
                                                )
import           Control.Monad
import qualified Control.Monad.Combinators.NonEmpty
                                               as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Bifunctor
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Syntax

type Parser a = Parsec Void String a

keywords :: [String]
keywords = ["def", "fun", "err", "match"]

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

topLevel :: Parser (TopLevel String)
topLevel = keyword "def" >> do
  name <- ident
  args <- NE.some ident <* symbol "->"
  body <- bind args <$> expr
  return $ DefFun name args body

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

-- caseExpr :: Parser (CaseExpr String)
-- caseExpr = parens $ parens (CaseExpr <$> cIdent <*> many ident) <*> expr

expr :: Parser (Expr String)
expr = fmap
  (\case
    x      :| [] -> x
    x :| y :  ys -> App x (y :| ys)
  )
  exprs


exprs :: Parser (NonEmpty (Expr String))
exprs = NE.some atom

atom :: Parser (Expr String)
atom = choice
  [ Var <$> ident
  , Const <$> constant
  , parens expr
  , keyword "err" >> (Err <$> lexeme stringLiteral)
  , keyword "fun" >> do
    args <- NE.some ident <* symbol "->"
    body <- bind args <$> expr
    return $ Lambda args body
  , Parser.match
  ]

match :: Parser (Expr String)
match = keyword "match" >> (Case <$> expr <*> many Parser.pattern)


pattern :: Parser (Pattern Expr String)
pattern = symbol "|" >> choice
  [ try $ do
    tag  <- cons
    args <- NE.some ident
    symbol "->"
    body <- bind args <$> expr
    return $ PatCons tag args body
  , PatConst <$> constant <*> (symbol "->" >> expr)
  , PatWild <$> (symbol "_" >> symbol "->" >> expr)
  ]

constant :: Parser Const
constant =
  choice [Int <$> lexeme L.decimal, String <$> stringLiteral, Cons <$> cons]

defs :: Parser [TopLevel String]
defs = many topLevel

parseDefs :: String -> Either String [TopLevel String]
parseDefs s = first errorBundlePretty $ parse (defs <* eof) "stdin" s

parseRepl :: String -> Either String (Expr String)
parseRepl s = first errorBundlePretty $ parse (expr <* eof) "repl" s
