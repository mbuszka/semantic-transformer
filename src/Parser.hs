module Parser
  ( fromFile,
  )
where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Polysemy.Error hiding (try)
import qualified ScopeCheck
import Syntax hiding (mkTerm)
import Text.Megaparsec hiding (State (..))
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Util

fromFile :: Members '[Embed IO, Error Err] r => FilePath -> Sem r (Program Term)
fromFile f = do
  program <- readFile f
  case runParser (many parseTopLevel <* eof) f program of
    Left err -> throw . ParseError . Text.pack . errorBundlePretty $ err
    Right t -> validateProgram t >>= ScopeCheck.fromSource

-- Parser

type Parser a = Parsec Void Text a

data TopLevel
  = TDef Loc (Var, Def Term)
  | TDecl Loc (Tp, [Tp])
  | TTest TestCase

tag :: Parser Tag
tag = SrcTag <$> text

var :: Parser Var
var = mkVar <$> ident

parseType :: Parser Tp
parseType =
  record
    <|> ( text <&> \case
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

mkTerm :: Parser (TermF Term) -> Parser Term
mkTerm p = do
  SourcePos _ row col <- getSourcePos
  let loc = Loc (unPos row) (unPos col)
  Term (Just loc) <$> p

parseTerm :: Parser Term
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

parsePatterns :: Parser (Patterns Term)
parsePatterns = Patterns <$> many parseCase

parseCase :: Parser (Pattern (), Scope Term)
parseCase = parens $ do
  (p, xs) <- extractNames <$> pattern
  t <- parseTerm
  pure (p, scope xs t)
  where
    pattern =
      choice
        [ PWild <$ keyword "_",
          PCons <$> parseValue pattern,
          typed <&> \case (v, tp) -> PVar v tp
        ]

annot :: Parser Annot
annot = empty

parseDef :: Parser (Var, Def Term)
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
  forall r. Member (Error Err) r => [TopLevel] -> Sem r (Program Term)
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
        throw (ScopeError (Just l) "Redefinition of main")
      (_, TDef l (v, def) : ts') -> case Map.lookup v defs of
        Nothing -> aux (Map.insert v def defs) types test main ts'
        Just _ -> throw (ScopeError (Just l) "Redefinition of a top-level function")
      (_, TDecl l (n, d) : ts') -> case Map.lookup n types of
        Nothing -> aux defs (Map.insert n d types) test main ts'
        Just _ -> throw (ScopeError (Just l) "Redefinition of a data type")
      (_, TTest t : ts') -> aux defs types (t : test) main ts'

-- Lexer

keywords :: Set Text
keywords =
  Set.fromList
    [ "_",
      "case",
      "def",
      "def-data",
      "panic",
      "fun",
      "let"
    ]

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

ident :: Parser Text
ident = mfilter (not . flip elem keywords) $ text

keyword :: Text -> Parser Text
keyword s = lexeme $ try (string s <* notFollowedBy (satisfy identRest))

text :: Parser Text
text =
  lexeme (Text.cons <$> satisfy identFirst <*> takeWhileP Nothing identRest)

stringLiteral :: Parser Text
stringLiteral =
  lexeme $ fmap Text.pack $ char '\"' *> manyTill L.charLiteral (char '\"')

numberLiteral :: Parser Int
numberLiteral = lexeme $ L.signed (pure ()) L.decimal

-- Helpers

symbol :: Text -> Parser Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

space :: Parser ()
space = L.space space1 (L.skipLineComment ";") empty

identFirst :: Char -> Bool
identFirst c = case c of
  '_' -> True
  '-' -> True
  '+' -> True
  '/' -> True
  '*' -> True
  _ -> Char.isAlpha c

identRest :: Char -> Bool
identRest c = identFirst c || Char.isDigit c
