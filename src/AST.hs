{-# LANGUAGE OverloadedStrings #-}
module AST
where
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as Text
import           Data.Text (Text)

data DFM = DFM 
  { dfmName   :: FilePath
  , dfmObject :: Object
  } deriving Show

data Object = Object
  { objectKind :: Text
  , objectName :: Text
  , objectType :: Maybe (Text, Maybe Int)
  , objectProperties :: [Property]
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
  | RoundBracket [Value]
  | CurlyBracket (NonEmpty Value)
  | AngleBracket [Item]
  deriving Show

instance Aeson.ToJSON DFM where
  toJSON (DFM dfmPath obj)
    = Aeson.object
      [ "file" .= dfmPath
      , "code" .= obj ]

instance Aeson.ToJSON Object where
  toJSON (Object kind name typeName properties objects)
    = Aeson.object
      [ "kind"       .= kind
      , "name"       .= name 
      , "type"       .= fmap typeNameToJSON typeName
      , "properties" .= (object $ fmap toTuple properties)
      , "objects"    .= fmap Aeson.toJSON objects ]

instance Aeson.ToJSON Item where
  toJSON (Item name properties)
    = Aeson.object 
      [ "name" .= name
      , "properties" .=
        ( object 
        . NonEmpty.toList 
        . fmap toTuple
        ) properties 
      ]

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
  toJSON (AngleBracket xs) = Aeson.object [ "items" .= fmap Aeson.toJSON xs ] 

typeNameToJSON :: (Text, Maybe Int) -> Aeson.Value
typeNameToJSON (name, maybeIndex)
  = Aeson.object 
    [ "name"  .= name
    , "index" .= maybeIndex ]

toTuple :: Property -> (Text, Value)
toTuple (Property name value) = (name, value)

object :: [(Text, Value)] -> Aeson.Value
object = Aeson.object . fmap (\(k, v) -> Text.toLower k .= v)