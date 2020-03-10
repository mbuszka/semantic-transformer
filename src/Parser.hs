{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Parser
  ( run,
  )
where

import Control.Monad.Writer
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Set as Set
import Syntax
import Text.Megaparsec hiding (State (..))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import MyPrelude

-- type Parser a = ParsecT Void Text (State Metadata) a

type Parser m = (MonadParsec Void Text m, MonadStx m)

keywords = ["def", "@no-cps", "fun", "case", "let", "_", "def-data", "error"]

symbol :: Parser m => Text -> m Text
symbol = L.symbol space

lexeme :: Parser m => m a -> m a
lexeme = L.lexeme space

keyword :: Parser m => Text -> m Text
keyword s = lexeme $ try (string s <* notFollowedBy alphaNumChar)

parens :: Parser m => m a -> m a
parens = between (symbol "(") (symbol ")")

braces :: Parser m => m a -> m a
braces = between (symbol "{") (symbol "}")

ident :: Parser m => m Text
ident = mfilter (not . flip elem keywords) . lexeme $ txt
  where
    txt = takeWhile1P (Just "identifier") pred
    pred '_' = True
    pred '-' = True
    pred c = Char.isLetter c

var :: Parser m => m Var
var = mkVar <$> ident

parseTerm :: Parser m => m Term
parseTerm = try (parens (choice exprs)) <|> try cons <|> err <|> v
  where
    v = mkTerm . Var =<< var
    exprs = [abs, let', case', app]
    abs = keyword "fun" >> do
      xs <- parens (many var)
      body <- parseTerm
      mkTerm (Abs $ Scope xs body)
    let' = keyword "let" >> do
      v <- var
      lhs <- parseTerm
      rhs <- parseTerm
      mkTerm (Let lhs (Scope [v] rhs))
    app = mkTerm =<< liftA2 App parseTerm (many parseTerm)
    case' = mkTerm =<< keyword "case" *> liftA2 Case parseTerm parsePatterns
    cons = mkTerm =<< braces (liftA2 Cons ident (many parseTerm))
    err = keyword "error" >> mkTerm Error

parsePatterns :: Parser m => m (Patterns Term)
parsePatterns = Patterns <$> many parseCase

parseCase :: Parser m => m (Pattern (), Scope Term)
parseCase = parens $ do
  (p, xs) <- extractNames <$> pattern
  t <- parseTerm
  pure (p, Scope xs t)
  where
    pattern =
      choice
        [ braces (PCons <$> ident <*> many pattern),
          keyword "_" >> pure PWild,
          PVar . mkVar <$> ident
        ]

annot :: Parser m => m Annot
annot = keyword "@no-cps" >> pure NoCps

parseDef :: Parser m => m (Def Term)
parseDef = parens $ do
  x <- keyword "def" >> var
  as <- Set.fromList <$> many annot
  xs <- parens (many var)
  t <- parseTerm
  pure $ Def as x (Scope xs t)

parseProgram :: Parser m => m (Program Term)
parseProgram = do
  dt <- parseData
  defs <- many parseDef
  pure $ Program defs dt

parseData :: Parser m => m DataDecl
parseData = parens $ do
  name <- keyword "def-data" >> ident
  records <- many parseRecord
  pure $ DataDecl name records

parseRecord :: Parser m => m Record
parseRecord = braces $ do
  name <- ident
  elems <- many ident
  pure $ Record name elems

run :: (Monad m, MonadStx m) => String -> Text -> m (Program Term)
run f txt = do
  e <- runParserT (parseProgram <* eof) f txt
  case e of
    Left err -> error . errorBundlePretty $ err
    Right t -> pure t
