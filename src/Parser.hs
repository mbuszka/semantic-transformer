module Parser
  ( run,
  )
where

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import Syntax
import Syntax.Source
import Text.Megaparsec hiding (State (..))
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Util

run :: FilePath -> Text -> Either Err SrcProgram
run fileName program = case runParser parseProgram fileName program of
  Left err -> Left . ParseError . Text.pack . errorBundlePretty $ err
  Right p -> Right p

-- Parser

begin :: Parser Text
begin = do
  b <- string "; begin interpreter" <* takeWhileP Nothing (/= '\n') <* space
  pure $ b <> "\n\n"

end :: Parser Text
end = do
  e <- string "; end interpreter" <* takeWhileP Nothing (/= '\n')
  pure $ "\n" <> e

parseProgram :: Parser SrcProgram
parseProgram = do
  (cs, b) <- manyTill_ anySingle begin
  let srcPrologue = (Text.pack cs <> b)
  (srcProgram, e) <- manyTill_ parseTopLevel end
  srcEpilogue <- (e <>) <$> takeRest
  pure (SrcProgram {..})

type Parser a = Parsec Void Text a

tag :: Parser Tag
tag = MkTag <$> text

var :: Parser Var
var = MkVar <$> ident

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

mkTerm :: Parser (TermF SrcTerm) -> Parser SrcTerm
mkTerm p = do
  SourcePos _ row col <- getSourcePos
  let srcLoc = Loc (unPos row) (unPos col)
  srcTerm <- p
  pure $ SrcTerm {..}

parseTerm :: Parser SrcTerm
parseTerm = mkTerm (parens expr <|> cons <|> err <|> variable)
  where
    variable = Var <$> var
    cons = Cons <$> parseValue parseTerm
    expr = choice [lam, let', case', app]
    lam = keyword "lambda" >> do
      xs <- parens (many typed)
      Abs . Scope xs <$> parseTerm
    let' = keyword "let" >> do
      v <- typed
      lhs <- parseTerm
      rhs <- parseTerm
      pure $ Let lhs (Scope [v] rhs)
    app = liftA2 App parseTerm (many parseTerm)
    case' = keyword "match" *> liftA2 Case parseTerm parsePatterns
    err = keyword "error" >> stringLiteral $> Panic

parsePatterns :: Parser (Patterns SrcTerm)
parsePatterns = Patterns <$> many parseCase

parseCase :: Parser (Pattern (), Scope SrcTerm)
parseCase = parens $ do
  (p, xs) <- extractNames <$> pattern
  t <- parseTerm
  pure (p, scope xs t)
  where
    pattern =
      choice
        [ PWild <$ keyword "_",
          PCons <$> parseValue pattern,
          v
        ]
    v = (PVar <$> var <*> pure Nothing) <|> brackets do
      t <- parseType
      v <- var
      pure $ PVar v (Just t)

parseValue :: Parser v -> Parser (ValueF v)
parseValue subTerm =
  choice
    [ Number <$> try numberLiteral,
      String <$> stringLiteral,
      Boolean True <$ keyword "#t",
      Boolean False <$ keyword "#f",
      braces (Record <$> tag <*> many subTerm)
    ]

annot :: Parser Annot
annot = empty

parseDef :: Parser (Var, Def SrcTerm)
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

parseTopLevel :: Parser TopLevel
parseTopLevel = do
  SourcePos _ row col <- getSourcePos
  let loc = Loc (unPos row) (unPos col)
  parens $
    choice
      [ TDecl loc <$> parseDecl,
        TDef loc <$> parseDef
      ]

-- Lexer

keywords :: Set Text
keywords =
  Set.fromList
    [ "_",
      "match",
      "def",
      "def-data",
      "error",
      "lambda",
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
space = L.space space1 (L.skipLineComment ";;") empty

identFirst :: Char -> Bool
identFirst c = case c of
  '_' -> True
  '-' -> True
  '+' -> True
  '/' -> True
  '*' -> True
  '?' -> True
  _ -> Char.isAlpha c

identRest :: Char -> Bool
identRest c = identFirst c || Char.isDigit c
