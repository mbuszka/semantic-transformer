{-# LANGUAGE TupleSections #-}

module Parser
  ( fromFile,
  )
where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Polysemy.Error hiding (try)
import Syntax
import Text.Megaparsec hiding (State (..))
import Text.Megaparsec.Char
-- import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L
import Util

type Parser a = Parsec Void Text a

data TopLevel
  = TDef Loc (Var, Def Term)
  | TData Loc (Text, [Record Text])
  | TTest TestCase

keywords :: [Text]
keywords =
  [ "_",
    "case",
    "def",
    "def-data",
    "panic",
    "fun",
    "let"
  ]

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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

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

typed :: Parser (Var, Maybe Tp)
typed = fmap (,Nothing) var <|> brackets do
  v <- var
  tp <- ident
  pure (v, Just (Tp tp))

mkTerm :: TermF Term -> Parser Term
mkTerm = pure . Term

parseTerm :: Parser Term
parseTerm = parens expr <|> cons <|> err <|> variable
  where
    variable = mkTerm . Var =<< var
    expr = choice [lam, let', case', app]
    lam = keyword "fun" >> do
      xs <- parens (many typed)
      body <- parseTerm
      mkTerm (Abs $ Scope xs body)
    let' = keyword "let" >> do
      v <- typed
      lhs <- parseTerm
      rhs <- parseTerm
      mkTerm (Let lhs (Scope [v] rhs))
    app = mkTerm =<< liftA2 App parseTerm (many parseTerm)
    case' = mkTerm =<< keyword "case" *> liftA2 Case parseTerm parsePatterns
    cons = mkTerm . Cons =<< parseRecord parseTerm
    err = keyword "panic" >> mkTerm Panic

parseRecord :: Parser a -> Parser (Record a)
parseRecord subterm = braces $ do
  name <- tag
  subterms <- many subterm
  pure $ Record name subterms

parsePatterns :: Parser (Patterns Term)
parsePatterns = Patterns <$> many parseCase

parseCase :: Parser (Pattern (), Scope Term)
parseCase = parens $ do
  (p, xs) <- extractNames <$> pattern
  t <- parseTerm
  pure (p, Scope (fmap (,Nothing) xs) t)
  where
    pattern =
      (PCons <$> parseRecord pattern)
        <|> (keyword "_" >> pure PWild)
        <|> (PVar . mkVar <$> ident)

annot :: Parser Annot
annot = empty

parseDef :: Parser (Var, Def Term)
parseDef = do
  x <- keyword "def" >> var
  as <- Set.fromList <$> many annot
  xs <- parens (many typed)
  t <- parseTerm
  pure (x, Def as (Scope xs t))

parseData :: Parser (Text, [Record Text])
parseData = do
  name <- keyword "def-data" >> ident
  records <- many $ parseRecord ident
  pure (name, records)

stringLiteral :: Parser Text
stringLiteral =
  lexeme $ fmap Text.pack $ char '\"' *> manyTill L.charLiteral (char '\"')

numberLiteral :: Parser Int
numberLiteral = lexeme $ L.signed (pure ()) L.decimal

parseValue :: Parser Value
parseValue =
  fmap Number numberLiteral
    <|> fmap String stringLiteral
    <|> fmap Struct (parseRecord parseValue)

parseTestCase :: Parser TestCase
parseTestCase = do
  keyword "def-test"
  desc <- stringLiteral
  inputs <- parens (many parseValue)
  output <- parseValue
  pure $ TestCase desc inputs output

parseTopLevel :: Parser TopLevel
parseTopLevel = do
  SourcePos _ row col <- getSourcePos
  let loc = Loc (unPos row) (unPos col)
  parens $
    (TData loc <$> parseData)
      <|> (TTest <$> parseTestCase)
      <|> (TDef loc <$> parseDef)

validateProgram :: forall r. Member (Error Err) r => [TopLevel] -> Sem r (Program Term)
validateProgram ts = do
  (defs, types, tests, main) <- aux Map.empty Map.empty [] Nothing ts
  pure $
    Program
      { pgmDefinitions = defs,
        pgmDatatypes = types,
        pgmTests = tests,
        pgmMain = main
      }
  where
    aux defs types test main ts = case (main, ts) of
      (Nothing, []) -> throw @Err @r (ModuleError "No main in file")
      (Just m, []) -> pure (defs, types, test, m)
      (Nothing, TDef _ (SrcVar "main", def) : ts') ->
        aux defs types test (Just def) ts'
      (Just _, TDef l (SrcVar "main", _) : _) ->
        throw (ScopeError l "Redefinition of main")
      (_, TDef l (v, def) : ts') -> case Map.lookup v defs of
        Nothing -> aux (Map.insert v def defs) types test main ts'
        Just _ -> throw (ScopeError l "Redefinition of a top-level function")
      (_, TData l (n, d) : ts') -> case Map.lookup n types of
        Nothing -> aux defs (Map.insert n d types) test main ts'
        Just _ -> throw (ScopeError l "Redefinition of a data type")
      (_, TTest t : ts') -> aux defs types (t : test) main ts'

fromFile :: forall r. Members '[Embed IO, Error Err] r => FilePath -> Sem r (Program Term)
fromFile f = do
  pgm <- readFile f
  case runParser (many parseTopLevel <* eof) f pgm of
    Left err -> throw . ParseError . Text.pack . errorBundlePretty $ err
    Right t -> validateProgram t
