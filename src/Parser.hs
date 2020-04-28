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

parseField :: Parser StructField
parseField =
  choice
    [ FieldName <$> var,
      FieldType <$> tp,
      brackets $ FieldBoth <$> tp <*> var
    ]

parseStruct :: Parser DefStruct
parseStruct =
  braces $ DefStruct <$> tp <*> many parseField

parseType :: Parser (Either Tp DefStruct)
parseType = (Left <$> tp) <|> (Right <$> parseStruct)

typed :: Parser (Var, Maybe Tp)
typed = fmap (,Nothing) var <|> brackets do
  t <- tp
  v <- var
  pure (v, Just t)

mkTerm :: Parser (TermF SrcTerm) -> Parser SrcTerm
mkTerm p = do
  SourcePos _ row col <- getSourcePos
  let srcLoc = Loc (unPos row) (unPos col)
  srcTerm <- p
  pure $ SrcTerm {..}

parseTerm :: Parser SrcTerm
parseTerm = mkTerm (parens expr <|> cons <|> variable)
  where
    variable = Var <$> var
    cons = Cons <$> parseValue parseTerm
    expr = choice [lam, let', case', err, app]
    lam = keyword "lambda" >> do
      as <- Set.fromList <$> many annot
      xs <- parens (many typed)
      Abs (transformAnnots as) . Scope xs <$> parseTerm
    let' = keyword "let" >> do
      x <- var
      lhs <- parseTerm
      rhs <- parseTerm
      pure $ Let (LetAnnot { letGenerated = False}) x lhs rhs
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
          PVar <$> var,
          brackets $ PType <$> tp <*> var
        ]

parseValue :: Parser v -> Parser (ValueF v)
parseValue subTerm =
  choice
    [ Number <$> try numberLiteral,
      String <$> stringLiteral,
      Boolean True <$ keyword "#t",
      Boolean False <$ keyword "#f",
      braces (Record <$> tp <*> many subTerm)
    ]

annot :: Parser Annot
annot = keyword "#:atomic" >> pure Atomic

parseFun :: Parser (DefFun SrcTerm)
parseFun = do
  x <- keyword "def" >> var
  as <- Set.fromList <$> many annot
  xs <- parens (many typed)
  t <- parseTerm
  pure $ DefFun x (transformAnnots as) (Scope xs t)

transformAnnots :: Set Annot -> FunAnnot
transformAnnots as = FunAnnot {funAtomic = Set.member Atomic as}

parseData :: Parser DefData
parseData = do
  name <- keyword "def-data" >> tp
  tps <- many $ parseType
  pure $ DefData name tps

parseTopLevel :: Parser TopLevel
parseTopLevel = do
  SourcePos _ row col <- getSourcePos
  let loc = Loc (unPos row) (unPos col)
  parens $
    choice
      [ TData loc <$> parseData,
        TFun loc <$> parseFun,
        TStruct loc <$> (keyword "def-struct" >> parseStruct)
      ]

-- Lexer

keywords :: Set Text
keywords =
  Set.fromList
    [ "_",
      "match",
      "def",
      "def-data",
      "def-struct",
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

var :: Parser Var
var = MkVar <$> lexeme (mfilter (not . flip elem keywords) $ text)
  where
    text = Text.cons <$> satisfy identFirst <*> takeWhileP Nothing identRest

tp :: Parser Tp
tp = MkTp <$> lexeme text
  where
    text = Text.cons <$> satisfy Char.isAsciiUpper <*> takeWhileP Nothing identRest

keyword :: Text -> Parser Text
keyword s = lexeme $ try (string s <* notFollowedBy (satisfy identRest))
  

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
