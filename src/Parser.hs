module Parser
  ( parseDefs
  , parseRepl
  )
where

import           Control.Applicative     hiding ( many
                                                , some
                                                )
import           Control.Monad
import qualified Control.Monad.Combinators.NonEmpty
                                               as NE

import           Data.Bifunctor
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Syntax

type Parser a = Parsec Void String a

symbol :: String -> Parser String
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

ident :: Parser String
ident = lexeme $ (:) <$> lowerChar <*> many alphaNumChar

cIdent :: Parser String
cIdent = lexeme $ (:) <$> upperChar <*> many alphaNumChar

topLevel :: Parser (TopLevel String)
topLevel =
  parens $ symbol "def" >> DefFun <$> ident <*> parens (NE.some ident) <*> expr

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

caseExpr :: Parser (CaseExpr String)
caseExpr = parens $ parens (CaseExpr <$> cIdent <*> many ident) <*> expr

expr :: Parser (Expr String)
expr = choice
  [ Var <$> ident
  , Syntax.Const . IConst <$> lexeme L.decimal
  , parens $ choice
    [ symbol "err" >> (Err <$> lexeme stringLiteral)
    , symbol "fun" >> Lambda <$> parens (NE.some ident) <*> expr
    , symbol "case" >> Case <$> expr <*> many caseExpr <*> optional
      (symbol "else" >> expr)
    , AppConstructor <$> cIdent <*> many expr
    , App <$> expr <*> NE.some expr
    ]
  ]

defs :: Parser [TopLevel String]
defs = many topLevel

parseDefs :: String -> Either String [TopLevel String]
parseDefs s = first errorBundlePretty $ parse defs "stdin" s

parseRepl :: String -> Either String (Expr String)
parseRepl s = first errorBundlePretty $ parse expr "repl" s
