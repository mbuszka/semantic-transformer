module Parser
  ( fromFile,
  )
where

import Control.Monad.Writer
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import Syntax
import Syntax.Term
import Text.Megaparsec hiding (State (..))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = Parsec Void Text a

keywords :: [Text]
keywords = ["def", "@no-cps", "fun", "case", "let", "_", "def-data", "error"]

symbol :: Text -> Parser Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

keyword :: Text -> Parser Text
keyword s = lexeme $ try (string s <* notFollowedBy alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

ident :: Parser Text
ident = mfilter (not . flip elem keywords) . lexeme $ txt
  where
    txt = takeWhile1P (Just "identifier") p
    p '_' = True
    p '-' = True
    p c = Char.isLetter c

tag :: Parser Tag
tag = SrcTag <$> ident

var :: Parser Var
var = mkVar <$> ident

mkTerm :: TermF Term -> Parser Term
mkTerm = pure . Term

parseTerm :: Parser Term
parseTerm = try (parens (choice exprs)) <|> try cons <|> err <|> variable
  where
    variable = mkTerm . Var =<< var
    exprs = [lam, let', case', app]
    lam = keyword "fun" >> do
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
    cons = mkTerm . Cons =<< parseRecord parseTerm
    err = keyword "error" >> mkTerm Error

parsePatterns :: Parser (Patterns Term)
parsePatterns = Patterns <$> many parseCase

parseCase :: Parser (Pattern (), Scope Term)
parseCase = parens $ do
  (p, xs) <- extractNames <$> pattern
  t <- parseTerm
  pure (p, Scope xs t)
  where
    pattern =
      choice
        [ PCons <$> parseRecord pattern,
          keyword "_" >> pure PWild,
          PVar . mkVar <$> ident
        ]

annot :: Parser Annot
annot = keyword "@no-cps" >> pure NoCps

parseDef :: Parser (Def Term)
parseDef = parens $ do
  x <- keyword "def" >> var
  as <- Set.fromList <$> many annot
  xs <- parens (many var)
  t <- parseTerm
  pure $ Def as x (Scope xs t)

parseProgram :: Parser (Program Term)
parseProgram = do
  dt <- parseData
  defs <- many parseDef
  pure $ Program defs dt

parseData :: Parser DataDecl
parseData = parens $ do
  name <- keyword "def-data" >> ident
  records <- many $ parseRecord ident
  pure $ DataDecl name records

parseRecord :: Parser a -> Parser (Record a)
parseRecord subterm = braces $ do
  name <- tag
  subterms <- many subterm
  pure $ Record name subterms

fromFile :: MonadIO m => FilePath -> m (Program Term)
fromFile f = do
  pgm <- readFile f
  pure case runParser (parseProgram <* eof) f pgm of
    Left err -> error . Text.pack . errorBundlePretty $ err
    Right t -> t
