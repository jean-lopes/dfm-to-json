{-# LANGUAGE OverloadedStrings #-}
module Parser ( parseDFM )
where
import qualified AST
import           Control.Monad                      (void)
import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import           Data.Char                          (chr, isHexDigit)
import           Data.List.NonEmpty                 (NonEmpty (..))
import qualified Data.List.NonEmpty                 as NonEmpty
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Void                          (Void)
import qualified Numeric
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as Lexer

type Parser = Parsec Void Text

reserved :: [Text]
reserved = [ "inherited", "inline", "object", "end", "true", "false" ]

whitespace :: Parser ()
whitespace = Lexer.space spaceConsumer singleLineComment multilineComment
  where
    spaceConsumer = void spaceChar
    singleLineComment = Lexer.skipLineComment "//"
    multilineComment = Lexer.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

symbol :: Text -> Parser Text
symbol = Lexer.symbol whitespace

symbol' :: Text -> Parser Text
symbol' = Lexer.symbol' whitespace

name :: Parser Text
name = (lexeme . try) (ident >>= check)
  where
    first = char '_' <|> letterChar
    other = Text.pack <$> many (char '_' <|> alphaNumChar)
    ident = Text.cons <$> first <*> other
    check x = if x `elem` reserved
      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
      else return x

qualifiedName :: Parser Text
qualifiedName = Text.append <$> name <*> option "" xs
  where
    dot = lexeme $ char '.'
    xs = Text.concat <$> (many $ Text.cons <$> dot <*> name)

true :: Parser Bool
true = True <$ symbol' "true"

false :: Parser Bool
false = False <$ symbol' "false"

boolean :: Parser Bool
boolean =  true <|> false

nat :: Parser String
nat = some digitChar

sign :: Parser String
sign = (:[]) <$> oneOf [ '+', '-' ]

int :: Parser String
int =  (++) <$> option "" sign <*> nat

natural :: Parser Int
natural = lexeme $ read <$> nat

dbl :: Parser String
dbl = concat <$> sequence [int, dec, ex]
  where
    dec = option "" $ (:) <$> char '.' <*> nat
    ex = option "" $ (:) <$> oneOf ['e', 'E'] <*> int

hexLit :: Parser Int
hexLit = fst . head . Numeric.readHex <$> (some hexDigitChar)

hex :: Parser Int
hex = symbol "$" *> hexLit

integer :: Parser Int
integer = lexeme $ hex <|> read <$> int

double :: Parser Double
double = lexeme $ read <$> dbl

closed :: Parser open -> Parser p -> Parser close -> Parser p
closed open p close = open *> p <* close

ctlChr :: Parser Char
ctlChr = lexeme $ chr <$ symbol "#" <*> natural

ctlStr :: Parser Text
ctlStr = Text.pack <$> some ctlChr

litChr :: Parser Char
litChr = satisfy (/='\'') <|> string "''" *> pure '\''

litStr :: Parser Text
litStr = lexeme $ Text.pack <$> closed (char '\'') (many litChr) (char '\'')

sglStr :: Parser Text
sglStr = ctlStr <|> litStr

str :: Parser Text
str = Text.concat . NonEmpty.toList <$> values
  where
    s = Text.concat <$> some sglStr
    values = s `NonEmpty.sepBy1` plus
    plus = symbol "+"

image :: Parser AST.Value
image = AST.Image . Text.concat <$> (some . lexeme) p
  where
    p = takeWhile1P (Just "image data") isHexDigit

squareBracket :: Parser AST.Value
squareBracket = AST.SquareBracket <$> closed (symbol "[") values (symbol "]")
  where
    values = value `sepBy`comma
    comma = symbol ","

roundBracket :: Parser AST.Value
roundBracket = AST.RoundBracket <$> closed (symbol "(") values (symbol ")")
  where
    values = value `sepBy` plus
    plus = symbol "+"

curlyBracket :: Parser AST.Value
curlyBracket = closed (symbol "{") (try image <|> values) (symbol "}")
  where
    values = AST.CurlyBracket <$> NonEmpty.some value

angleBracket :: Parser AST.Value
angleBracket = AST.AngleBracket <$> closed (symbol "<") (many item) (symbol ">")

value :: Parser AST.Value
value
  =   AST.Boolean <$> boolean
  <|> AST.Double <$> double
  <|> AST.Integer <$> integer
  <|> AST.String <$> str
  <|> AST.Name <$> qualifiedName
  <|> squareBracket
  <|> roundBracket
  <|> curlyBracket
  <|> angleBracket

property :: Parser AST.Property
property = AST.Property <$>  qualifiedName <*> (equal *> value)
  where
    equal = lexeme $ char '='

properties :: Parser (NonEmpty AST.Property)
properties = (:|) <$> property <*> many property

item :: Parser AST.Item
item = AST.Item <$> name <*> properties <* symbol' "end"

typ :: Parser (Text, Maybe Int)
typ = lexeme $ do
  void $ symbol ":"
  n <- name
  i <- optional $ closed (symbol "[") integer (symbol "]")
  return (n, i)

object :: Parser AST.Object
object
  =   AST.Object
  <$> (symbol' "object" <|> symbol' "inherited" <|> symbol' "inline")
  <*> name
  <*> optional typ
  <*> many property
  <*> many object
  <*  symbol' "end"

parseFile :: Parser p -> Parser p
parseFile p = whitespace *> p <* eof

parseDFM :: FilePath -> Text -> Either Text AST.Object
parseDFM fileName source
  = case parse (parseFile object) fileName source of
    Left  err -> Left . Text.pack $ parseErrorPretty err
    Right ast -> Right $ ast
