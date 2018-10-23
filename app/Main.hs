{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where
import qualified AST
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy         as ByteString
import           Data.Char                    hiding (Format)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           Data.Version
import           Options.Applicative.Simple
import           Parser
import           Pipes
import qualified Pipes.Prelude                as Pipes
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

baseConfig :: Config
baseConfig = defConfig
  { confCompare = keyOrder
    [ "file", "code" , "kind"      , "name"
    , "type", "index", "properties", "objects"
    ]
  }

newConfig :: Layout -> Config
newConfig Compact = baseConfig { confIndent = Spaces 0 }
newConfig Format  = baseConfig { confIndent = Spaces 2 }

version :: String
version = $(simpleVersion $ fst . last $ readP_to_S parseVersion CURRENT_PACKAGE_VERSION)

parseOptions :: IO (Args, ())
parseOptions = simpleOptions version empty
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

isExt :: String -> FilePath -> Bool
isExt ext = (=='.':ext) . map toLower . takeExtension

writeJSON :: ToJSON a => Config -> (FilePath, a) -> IO ()
writeJSON cfg (filePath, obj)
  = ByteString.writeFile (filePath -<.> "json")
  . encodePretty' cfg
  . toJSON
  $ obj

readDFM :: FilePath -> IO (FilePath, Text)
readDFM p = do
  Text.putStrLn . Text.pack $ "Reading: " ++ p
  src <- Text.readFile p
  return (p, src)

paths :: FilePath -> Producer FilePath IO ()
paths path = do
  isFile <- lift $ doesFileExist path
  if isFile
    then yield path
    else do
      ps <- lift $ listDirectory path
      mapM_ (paths . (path </>)) ps
  return ()

parse :: Pipe (FilePath, Text) (FilePath, AST.Object) IO ()
parse = forever $ do
  (path, src) <- await
  case parseDFM path src of
    Left  err -> lift $ Text.putStrLn err
    Right ast -> yield (path, ast)

process :: FilePath -> Config -> IO ()
process tgt cfg = do
  p <- canonicalizePath tgt
  setCurrentDirectory p
  runEffect
    $   paths p
    >-> Pipes.filter (isExt "dfm")
    >-> Pipes.map (makeRelative p)
    >-> Pipes.mapM readDFM
    >-> parse
    >-> Pipes.mapM_ (writeJSON cfg)

main :: IO ()
main = do
  (args, ()) <- parseOptions
  let cfg = newConfig . layout $ args
      tgt = target args
  if isValid tgt
    then process tgt cfg
    else putStrLn $ "Invalid path: " ++ tgt
