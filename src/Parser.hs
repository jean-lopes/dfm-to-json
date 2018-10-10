{-# LANGUAGE OverloadedStrings #-}
module Parser
where
import Text.Megaparsec (Parsec, (<|>), parseTest)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Char as Char
import qualified AST
import qualified Control.Applicative.Combinators as Combinators
import Data.Void (Void)
import Control.Monad (void)
import Data.Text (Text)
import qualified Numeric
import qualified Data.Text as Text

type Parser = Parsec Void Text

whitespace :: Parser ()
whitespace = Lexer.space spaceConsumer singleLineComment multilineComment
  where 
    spaceConsumer = void Char.spaceChar
    singleLineComment = Lexer.skipLineComment "//"
    multilineComment = multilineComment1 <|> multilineComment2
    multilineComment1 = Lexer.skipBlockComment "{"  "}"
    multilineComment2 = Lexer.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

symbol :: Text -> Parser Text
symbol = Lexer.symbol whitespace

symbol' :: Text -> Parser Text
symbol' = Lexer.symbol' whitespace

ident :: Parser Text
ident = lexeme $ Text.cons <$> first <*> rest
  where
    first = Char.char '_' <|> Char.letterChar
    others = Combinators.many $ first <|> Char.digitChar
    rest = Text.pack <$> others

identifier :: Parser AST.Identifier
identifier = AST.Identifier <$> ident

natStr :: Parser String
natStr = Combinators.some Char.digitChar

natural :: Parser Int
natural = lexeme $ read <$> natStr

sign :: Num a => Parser (a -> a)
sign = lexeme $ Combinators.option id (plus <|> minus)
  where
    plus = Char.char '+' *> pure id
    minus = Char.char '-' *> pure (* (-1))

integer :: Parser Int
integer = sign <*> natural

hexStr :: Parser String
hexStr = Combinators.some Char.hexDigitChar

hexLit :: Parser Int
hexLit = fst . head . Numeric.readHex <$> hexStr -- XXX

hexadecimal :: Parser Int
hexadecimal = symbol "$" *> hexLit    

doubleStr :: Parser String
doubleStr = (++) <$> natStr <*> Combinators.option "0" end
  where
    end = (++) . (:[]) <$> Char.char '.' <*> natStr

double :: Parser Double
double = sign <*> (read <$> doubleStr)

true :: Parser AST.Value
true = AST.True <$ symbol' "true"

false :: Parser AST.Value
false = AST.False <$ symbol' "false"

boolean :: Parser AST.Value
boolean = true <|> false

name :: Parser AST.Value
name = AST.Name <$> identifier

closed :: Char -> Parser a -> Char -> Parser a
closed o p c = open *> p <* close
  where
    open = lexeme $ Char.char' o
    close = lexeme $ Char.char' c

squareBracket :: Parser AST.Value
squareBracket = AST.SquareBracket <$> closed '[' values ']'
  where
    values = value `Combinators.sepBy`comma
    comma = symbol ","

value :: Parser AST.Value
value
  = AST.Integer <$> integer

