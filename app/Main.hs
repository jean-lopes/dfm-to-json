{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Version
import           Options.Applicative.Simple
import           Parser
import           System.FilePath
import           Text.ParserCombinators.ReadP

data Layout
  = Compacted
  | Formatted
  deriving Show

data Args = Args
  { layout :: Layout
  , filePath :: FilePath
  } deriving Show

config :: Config
config = defConfig
  { confCompare = keyOrder 
    [ "name"
    , "type"
    , "properties"
    , "objects"
    ]
  }

getConfig :: Layout -> Config
getConfig Compacted = config { confIndent = Spaces 0 }
getConfig Formatted = config { confIndent = Spaces 2 }

version :: String
version = $(simpleVersion (fst . last $ readP_to_S parseVersion CURRENT_PACKAGE_VERSION))

parseOptions :: IO (Args, ())
parseOptions = simpleOptions
  version
  empty
  "Converts a Delphi Form File (DFM) to JSON"
  ( Args 
    <$> flag Formatted Compacted
      (  long    "compact" 
      <> short   'c' 
      <> help    "Compacted JSON output" )
    <*> strArgument
      (  help    "DFM for conversion" 
      <> metavar "FILE")
  )
  empty

main :: IO ()
main = do
  (args, ()) <- parseOptions  
  let cfg = getConfig $ layout args
      dfmPath = filePath args
      fileName = dropExtension $ takeFileName dfmPath
      jsonFileName = fileName <.> "json"
      jsonPath = replaceFileName dfmPath jsonFileName
  src <- Text.readFile dfmPath 
  case parseDFM dfmPath src of
    Left msg -> putStrLn msg
    Right ast 
      -> ByteString.writeFile jsonPath 
      $ encodePretty' cfg . toJSON 
      $ ast