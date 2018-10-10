{-# LANGUAGE DuplicateRecordFields #-}
module AST
where
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

newtype DFM = DFM { unDFM :: Object }

data Object = Object
  { name :: Identifier
  , typ :: Identifier
  , properties :: NonEmpty Property
  , objects :: [Object]
  } deriving Show

data Property = Property
  { name :: Identifier
  , value :: Value
  } deriving Show

data Item = Item 
  { name :: Identifier
  , properties :: [Property] 
  } deriving Show

data Value 
  = Integer Int
  | Double Double
  | True
  | False 
  | String Text
  | Name Identifier
  | SquareBracket [Value]
  | RoundBracket (NonEmpty Value)
  | CurlyBracket (NonEmpty Value)
  | AngleBracket (NonEmpty Item)
  deriving Show

newtype Identifier = Identifier { unIdentifier :: Text } 
  deriving Show