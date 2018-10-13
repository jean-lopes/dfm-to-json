{-# LANGUAGE OverloadedStrings #-}
module AST
where
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import           Data.Aeson.Encoding.Internal (pair)
import           Data.Aeson.TH
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty)
import           Data.Semigroup
import qualified Data.Text as Text
import           Data.Text (Text)

data Object = Object
  { objectName :: Text
  , objectType :: Text
  , objectProperties :: NonEmpty Property
  , objectObjects :: [Object]
  } deriving Show

data Property = Property
  { propertyName :: Text
  , propertyValue :: Value
  } deriving Show

data Item = Item 
  { itemName :: Text
  , itemProperties :: NonEmpty Property
  } deriving Show

data Value 
  = Boolean Bool
  | Integer Int
  | Double Double
  | String Text
  | Name Text
  | Image Text
  | SquareBracket [Value]
  | RoundBracket (NonEmpty Value)
  | CurlyBracket (NonEmpty Value)
  | AngleBracket (NonEmpty Item)
  deriving Show

instance Aeson.ToJSON Object where
  toJSON (Object name typeName properties objects)
    = Aeson.object
      [ "name"       .= name 
      , "type"       .= typeName
      , "properties" .= mapProperties properties
      , "objects"    .= fmap Aeson.toJSON objects ]

instance Aeson.ToJSON Item where
  toJSON (Item name properties)
    = Aeson.object 
      [ "name" .= name
      , "properties" .= mapProperties properties ]

instance Aeson.ToJSON Value where
  toJSON (Boolean x) = Aeson.toJSON x
  toJSON (Integer x) = Aeson.toJSON x
  toJSON (Double x) = Aeson.toJSON x
  toJSON (String x) = Aeson.toJSON x
  toJSON (Name x) = Aeson.toJSON x
  toJSON (Image x) = Aeson.toJSON x
  toJSON (SquareBracket xs) = Aeson.toJSON xs
  toJSON (RoundBracket xs) = Aeson.toJSON xs
  toJSON (CurlyBracket xs) = Aeson.toJSON xs
  toJSON (AngleBracket xs) = mapItems xs

mapProperties :: NonEmpty Property -> Aeson.Value
mapProperties = object . NonEmpty.toList . fmap f
  where
    f = \(Property name value) -> (name, value)

mapItems :: NonEmpty Item -> Aeson.Value
mapItems xs = Aeson.object [ "items" .= fmap Aeson.toJSON xs ] 

object :: [(Text, Value)] -> Aeson.Value
object = Aeson.object . fmap (\(k, v) -> Text.toLower k .= v)