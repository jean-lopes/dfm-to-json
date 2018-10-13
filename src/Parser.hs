{-# LANGUAGE OverloadedStrings #-}
module Parser
where
import qualified AST
import           Control.Monad (void)
import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import           Data.Char (chr, isHexDigit)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup (sconcat)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Void (Void)
import qualified Numeric
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

reserved :: [Text]
reserved = [ "object", "end", "true", "false" ]

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
      then fail $ "keyword " ++ show x ++ "cannot be an identifier"
      else return x

qualifiedName :: Parser Text
qualifiedName = Text.append <$> name <*> option "" xs
  where
    dot = lexeme $ char '.'
    xs = Text.cons <$> dot <*> name

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

closed :: Char -> Parser a -> Char -> Parser a
closed o p c = open *> p <* close
  where
    open = lexeme $ char' o
    close = lexeme $ char' c

ctlChr :: Parser Char
ctlChr = lexeme $ chr <$ symbol "#" <*> natural

ctlStr :: Parser Text
ctlStr = Text.pack <$> some ctlChr

litChr :: Parser Char
litChr = satisfy (/='\'') <|> symbol "''" *> pure '\''

litStr :: Parser Text
litStr = lexeme $ Text.pack <$> closed '\'' (many litChr) '\''

sglStr :: Parser Text
sglStr = ctlStr <|> litStr

str :: Parser Text
str = Text.concat <$> some sglStr

image :: Parser AST.Value
image = AST.Image . Text.concat <$> (some . lexeme) p
  where
    p = takeWhile1P (Just "image data") isHexDigit

squareBracket :: Parser AST.Value
squareBracket = AST.SquareBracket <$> closed '[' values ']'
  where
    values = value `sepBy`comma
    comma = symbol ","

roundBracket :: Parser AST.Value
roundBracket = AST.RoundBracket <$> closed '(' values ')'
  where
    values = value `NonEmpty.sepBy1` plus
    plus = symbol "+"

curlyBracket :: Parser AST.Value
curlyBracket = closed '{' (try image <|> values) '}'
  where
    values = AST.CurlyBracket <$> NonEmpty.some value

angleBracket :: Parser AST.Value
angleBracket = AST.AngleBracket <$> closed '<' items '>'
  where
    items = NonEmpty.some item

value :: Parser AST.Value
value
  =   AST.Boolean <$> boolean
  <|> AST.Integer <$> integer
  <|> AST.Double <$> double
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

object :: Parser AST.Object
object
  =   AST.Object 
  <$  symbol' "object" 
  <*> name 
  <*  symbol ":" 
  <*> name 
  <*> properties 
  <*> many object 
  <*  symbol' "end"

dfm :: Parser AST.Object
dfm = whitespace *> object <* eof

parseDFM :: String -> Text -> Either String AST.Object
parseDFM fileName source = case parse dfm fileName source of 
  Left err -> Left $ parseErrorPretty err
  Right ast -> Right $ ast
