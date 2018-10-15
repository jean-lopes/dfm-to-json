{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where
import           Control.Monad (filterM)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as ByteString
import           Data.Char hiding (Format)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Version
import           Options.Applicative.Simple
import           Parser
import           System.Directory
import           System.FilePath
import           Text.ParserCombinators.ReadP

data Layout
  = Compact
  | Format
  deriving Show

data Args = Args
  { layout :: Layout
  , target :: FilePath
  } deriving Show

config :: Config
config = defConfig
  { confCompare = keyOrder 
    [ "file"
    , "code"
    , "kind"
    , "name"
    , "type"
    , "index"
    , "properties"
    , "objects"
    ]
  }

getConfig :: Layout -> Config
getConfig Compact = config { confIndent = Spaces 0 }
getConfig Format = config { confIndent = Spaces 2 }

version :: String
version = $(simpleVersion (fst . last $ readP_to_S parseVersion CURRENT_PACKAGE_VERSION))

parseOptions :: IO (Args, ())
parseOptions = simpleOptions
  version
  empty
  "Converts a Delphi Form File (DFM) to JSON"
  ( Args 
    <$> flag Format Compact
      (  long    "compact" 
      <> short   'c' 
      <> help    "Compact JSON output" )    
    <*> strArgument
      (  help    "FILE or DIRECTORY for conversion" 
      <> metavar "PATH") )
  empty

process :: Config -> FilePath -> IO ()
process cfg file = do
  Text.putStrLn $ "Reading: " `Text.append` (Text.pack file)
  src <- Text.readFile file
  case parseDFM file src of
    Left err  -> Text.putStrLn err
    Right ast -> writeJSON cfg (jsonPathFor file) ast

jsonPathFor :: FilePath -> FilePath
jsonPathFor dfmPath = replaceFileName dfmPath jsonFileName
  where
    fileName = dropExtension $ takeFileName dfmPath
    jsonFileName = fileName <.> "json"

paths :: FilePath -> IO [FilePath]
paths input
  =   canonicalizePath input 
  >>= doesFileExist
  >>= \isFile -> if isFile 
    then return [input]
    else listDirectory input 
      >>= mapM (canonicalizePath . (input++))
      >>= filterM isDFM

isDFM :: FilePath -> IO Bool
isDFM = return . (==".dfm") . map toLower . takeExtension

writeJSON :: ToJSON a => Config -> FilePath -> a -> IO ()
writeJSON cfg filePath obj
  = ByteString.writeFile filePath
  . encodePretty' cfg
  . toJSON
  $ obj

main :: IO ()
main = do
  (args, ()) <- parseOptions
  let cfg = getConfig . layout $ args 
  filePaths <- paths . target $ args
  mapM_ (process cfg) filePaths