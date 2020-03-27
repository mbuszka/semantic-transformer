module Parser
  ( fromFile,
  )
where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Polysemy.Error hiding (try)
-- import Text.Megaparsec.Debug

import qualified ScopeCheck
import Syntax
import Text.Megaparsec hiding (State (..))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Util

type Parser a = Parsec Void Text a

data TopLevel
  = TDef Loc (Var, Def Located)
  | TDecl Loc (Tp, [Tp])
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

parseType :: Parser Tp
parseType =
  record
    <|> ( ident <&> \case
            "integer" -> TInt
            "string" -> TStr
            "boolean" -> TBool
            t -> TData t
        )
  where
    record = braces (liftA2 TRecord tag (many parseType))

typed :: Parser (Var, Maybe Tp)
typed = fmap (,Nothing) var <|> brackets do
  v <- var
  tp <- parseType
  pure (v, Just tp)

mkTerm :: Parser (TermF Located) -> Parser Located
mkTerm p = do
  SourcePos _ row col <- getSourcePos
  let loc = Loc (unPos row) (unPos col)
  Located loc <$> p

parseTerm :: Parser Located
parseTerm = mkTerm (parens expr <|> cons <|> err <|> variable)
  where
    variable = Var <$> var
    cons = Cons <$> parseValue parseTerm
    expr = choice [lam, let', case', app]
    lam = keyword "fun" >> do
      xs <- parens (many typed)
      Abs . Scope xs <$> parseTerm
    let' = keyword "let" >> do
      v <- typed
      lhs <- parseTerm
      rhs <- parseTerm
      pure $ Let lhs (Scope [v] rhs)
    app = liftA2 App parseTerm (many parseTerm)
    case' = keyword "case" *> liftA2 Case parseTerm parsePatterns
    err = keyword "panic" $> Panic

parsePatterns :: Parser (Patterns Located)
parsePatterns = Patterns <$> many parseCase

parseCase :: Parser (Pattern (), Scope Located)
parseCase = parens $ do
  (p, xs) <- extractNames <$> pattern
  t <- parseTerm
  pure (p, scope xs t)
  where
    pattern =
      choice
        [ PWild <$ keyword "_",
          PVar <$> var,
          PCons <$> parseValue pattern
        ]

annot :: Parser Annot
annot = empty

parseDef :: Parser (Var, Def Located)
parseDef = do
  x <- keyword "def" >> var
  as <- Set.fromList <$> many annot
  xs <- parens (many typed)
  t <- parseTerm
  pure (x, Def as (Scope xs t))

parseDecl :: Parser (Tp, [Tp])
parseDecl = do
  name <- keyword "def-data" >> parseType
  tps <- many $ parseType
  pure (name, tps)

stringLiteral :: Parser Text
stringLiteral =
  lexeme $ fmap Text.pack $ char '\"' *> manyTill L.charLiteral (char '\"')

numberLiteral :: Parser Int
numberLiteral = lexeme $ L.signed (pure ()) L.decimal

parseValue :: Parser v -> Parser (ValueF v)
parseValue subTerm =
  choice
    [ Number <$> numberLiteral,
      String <$> stringLiteral,
      Boolean True <$ keyword "#t",
      Boolean False <$ keyword "#f",
      braces (Record <$> tag <*> many subTerm)
    ]

parseTestCase :: Parser TestCase
parseTestCase = do
  desc <- keyword "def-test" >> stringLiteral
  inputs <- parens (many value)
  output <- value
  pure $ TestCase desc inputs output
  where
    value = Value <$> parseValue value

parseTopLevel :: Parser TopLevel
parseTopLevel = do
  SourcePos _ row col <- getSourcePos
  let loc = Loc (unPos row) (unPos col)
  parens $
    choice
      [ TDecl loc <$> parseDecl,
        TTest <$> parseTestCase,
        TDef loc <$> parseDef
      ]

validateProgram ::
  forall r. Member (Error Err) r => [TopLevel] -> Sem r (Program Located)
validateProgram topLevels = do
  (defs, types, tests, main) <- aux Map.empty Map.empty [] Nothing topLevels
  pure $
    Program
      { programDefinitions = defs,
        programDatatypes = types,
        programTests = reverse tests,
        programMain = main
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
      (_, TDecl l (n, d) : ts') -> case Map.lookup n types of
        Nothing -> aux defs (Map.insert n d types) test main ts'
        Just _ -> throw (ScopeError l "Redefinition of a data type")
      (_, TTest t : ts') -> aux defs types (t : test) main ts'

fromFile :: Members '[Embed IO, Error Err] r => FilePath -> Sem r (Program Term)
fromFile f = do
  program <- readFile f
  case runParser (many parseTopLevel <* eof) f program of
    Left err -> throw . ParseError . Text.pack . errorBundlePretty $ err
    Right t -> validateProgram t >>= ScopeCheck.fromSource
