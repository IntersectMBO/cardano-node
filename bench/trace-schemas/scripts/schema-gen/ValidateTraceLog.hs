{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM, forM_, unless, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.List (foldl', isSuffixOf, sortOn)
import qualified Data.Text as T
import Data.Word (Word8)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removePathForcibly
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>), takeBaseName)
import System.Process (proc, readCreateProcessWithExitCode, readProcess)

defaultTraceSchema :: FilePath
defaultTraceSchema = "bench/trace-schemas/TraceMessage.schema.json"

defaultMessagesDir :: FilePath
defaultMessagesDir = "bench/trace-schemas/messages"

data Config = Config
  { cfgLogFile :: FilePath
  , cfgTraceSchema :: FilePath
  , cfgMessagesDir :: FilePath
  }

data LogEntry = LogEntry
  { leLineNo :: Int
  , leNamespace :: Maybe String
  , leInstancePath :: FilePath
  }

data ExtractedLog = ExtractedLog
  { elSkippedPreamble :: Int
  , elEntries :: [LogEntry]
  , elMalformedLines :: [Int]
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cfgLogFile = ""
    , cfgTraceSchema = defaultTraceSchema
    , cfgMessagesDir = defaultMessagesDir
    }

main :: IO ()
main = do
  config <- parseArgs defaultConfig =<< getArgs
  checkInputs config
  namespaceSchemas <- loadNamespaceSchemas (cfgMessagesDir config)

  withTempDir "cardano-trace-log-validate" $ \tempDir -> do
    extracted <- extractLogEntries (cfgLogFile config) tempDir
    let entries = elEntries extracted

    when (null entries) $ do
      putStrLn "No JSON trace messages found in the log file."
      exitFailure

    putStrLn $
      "Skipped "
        <> show (elSkippedPreamble extracted)
        <> " non-log line(s) before the first JSON trace message."

    unless (null (elMalformedLines extracted)) $
      putStrLn $
        "Malformed post-preamble line(s): "
          <> commaList (map show (elMalformedLines extracted))

    envelopeOk <-
      validateBatches
        "Validating common trace envelope..."
        (validatorArgs ["--schemafile", cfgTraceSchema config])
        (map leInstancePath entries)

    let (knownEntries, unknownEntries) = partitionKnown namespaceSchemas entries
    namespaceOk <- validateKnownNamespaces knownEntries
    unknownOk <- reportUnknownNamespaces unknownEntries

    let overallOk =
          envelopeOk
            && namespaceOk
            && unknownOk
            && null (elMalformedLines extracted)

    if overallOk
      then do
        putStrLn $
          "Validated "
            <> show (length entries)
            <> " trace message(s) from "
            <> cfgLogFile config
        exitSuccess
      else exitFailure

parseArgs :: Config -> [String] -> IO Config
parseArgs config args = go config args
 where
  go cfg [] =
    if null (cfgLogFile cfg)
      then do
        putStrLn "Missing required argument: --log-file PATH"
        printHelp
        exitFailure
      else pure cfg
  go cfg ("--log-file" : path : rest) = go cfg {cfgLogFile = path} rest
  go cfg ("--trace-schema" : path : rest) = go cfg {cfgTraceSchema = path} rest
  go cfg ("--messages-dir" : path : rest) = go cfg {cfgMessagesDir = path} rest
  go _ ["--help"] = printHelp >> exitSuccess
  go _ ["-h"] = printHelp >> exitSuccess
  go _ unknown = do
    putStrLn $ "Unrecognized arguments: " <> unwords unknown
    printHelp
    exitFailure

printHelp :: IO ()
printHelp =
  putStrLn $
    unlines
      [ "Usage: runghc -package-env - bench/trace-schemas/scripts/schema-gen/ValidateTraceLog.hs --log-file PATH [options]"
      , ""
      , "Options:"
      , "  --log-file PATH      Path to a cardano-node stdout/stderr log file."
      , "  --trace-schema PATH  Path to TraceMessage.schema.json."
      , "  --messages-dir PATH  Directory containing namespace message schemas."
      ]

checkInputs :: Config -> IO ()
checkInputs config = do
  logExists <- doesFileExist (cfgLogFile config)
  unless logExists $ do
    putStrLn $ "Log file not found: " <> cfgLogFile config
    exitFailure

  traceSchemaExists <- doesFileExist (cfgTraceSchema config)
  unless traceSchemaExists $ do
    putStrLn $ "Trace schema not found: " <> cfgTraceSchema config
    exitFailure

  messagesDirExists <- doesDirectoryExist (cfgMessagesDir config)
  unless messagesDirExists $ do
    putStrLn $ "Messages directory not found: " <> cfgMessagesDir config
    exitFailure

loadNamespaceSchemas :: FilePath -> IO (Map.Map String FilePath)
loadNamespaceSchemas root = do
  schemaFiles <- listJsonFilesRecursive root
  pairs <- fmap concat $ forM schemaFiles $ \path -> do
    bytes <- BL.readFile path
    case A.eitherDecode bytes :: Either String A.Value of
      Left err -> do
        putStrLn $ "Failed to parse schema file " <> path <> ": " <> err
        exitFailure
      Right (A.Object obj) ->
        case KM.lookup "ns" obj of
          Just (A.String ns) -> pure [(T.unpack ns, path)]
          _ -> pure []
      Right _ -> pure []

  let grouped = Map.fromListWith (++) [(ns, [path]) | (ns, path) <- pairs]
  let duplicates = Map.toList (Map.filter ((> 1) . length) grouped)
  unless (null duplicates) $ do
    putStrLn "Duplicate namespaces found in message schemas:"
    forM_ duplicates $ \(ns, paths) ->
      putStrLn $ "  " <> ns <> " -> " <> commaList paths
    exitFailure

  pure (Map.map head grouped)

extractLogEntries :: FilePath -> FilePath -> IO ExtractedLog
extractLogEntries logPath tempDir = do
  bytes <- BS.readFile logPath
  go False 1 (BS8.lines bytes) 0 [] []
 where
  go _ _ [] skipped entries malformed =
    pure
      ExtractedLog
        { elSkippedPreamble = skipped
        , elEntries = reverse entries
        , elMalformedLines = reverse malformed
        }
  go started lineNo (line : rest) skipped entries malformed =
    case decodeLogLine line of
      Just value -> do
        instancePath <- writeInstance tempDir lineNo value
        let entry =
              LogEntry
                { leLineNo = lineNo
                , leNamespace = extractNamespace value
                , leInstancePath = instancePath
                }
        go True (lineNo + 1) rest skipped (entry : entries) malformed
      Nothing
        | not started -> go False (lineNo + 1) rest (skipped + 1) entries malformed
        | BS.all isIgnorableWhitespace line -> go True (lineNo + 1) rest skipped entries malformed
        | otherwise -> go True (lineNo + 1) rest skipped entries (lineNo : malformed)

decodeLogLine :: BS.ByteString -> Maybe A.Value
decodeLogLine line =
  case A.decodeStrict' line of
    Just value@(A.Object _) -> Just value
    _ -> Nothing

extractNamespace :: A.Value -> Maybe String
extractNamespace (A.Object obj) =
  case KM.lookup "ns" obj of
    Just (A.String ns) -> Just (T.unpack ns)
    _ -> Nothing
extractNamespace _ = Nothing

writeInstance :: FilePath -> Int -> A.Value -> IO FilePath
writeInstance tempDir lineNo value = do
  let path = tempDir </> ("line-" <> padLineNo lineNo <> ".json")
  BL.writeFile path (A.encode value)
  pure path

padLineNo :: Int -> String
padLineNo n =
  let s = show n
      width = 8
   in replicate (max 0 (width - length s)) '0' <> s

partitionKnown ::
  Map.Map String FilePath ->
  [LogEntry] ->
  (Map.Map FilePath [LogEntry], [LogEntry])
partitionKnown namespaceSchemas =
  foldl' step (Map.empty, [])
 where
  step (known, unknown) entry =
    case leNamespace entry >>= (`Map.lookup` namespaceSchemas) of
      Just schemaPath ->
        (Map.insertWith (++) schemaPath [entry] known, unknown)
      Nothing -> (known, entry : unknown)

validateKnownNamespaces :: Map.Map FilePath [LogEntry] -> IO Bool
validateKnownNamespaces groups = do
  results <-
    forM (sortOn fst (Map.toList groups)) $ \(schemaPath, groupEntries) -> do
      let namespaceLabel = maybe (takeBaseName schemaPath) id (leNamespace =<< safeHead groupEntries)
      let header =
            "Validating namespace "
              <> namespaceLabel
              <> " (" <> show (length groupEntries) <> " message(s))..."
      validateBatches header (validatorArgs ["--schemafile", schemaPath]) (map leInstancePath groupEntries)
  pure (and results)

reportUnknownNamespaces :: [LogEntry] -> IO Bool
reportUnknownNamespaces [] = pure True
reportUnknownNamespaces entries = do
  putStrLn "Namespaces not found in message schemas:"
  let grouped =
        Map.fromListWith (++)
          [ (renderNamespace (leNamespace entry), [leLineNo entry])
          | entry <- entries
          ]
  forM_ (sortOn fst (Map.toList grouped)) $ \(ns, lineNos) ->
    putStrLn $
      "  "
        <> ns
        <> " at line(s) "
        <> commaList (map show (sortOn id lineNos))
  pure False

renderNamespace :: Maybe String -> String
renderNamespace (Just ns) = ns
renderNamespace Nothing = "<missing ns>"

validateBatches :: String -> [String] -> [FilePath] -> IO Bool
validateBatches _ _ [] = pure True
validateBatches header baseArgs files = do
  putStrLn header
  results <- forM (chunksOf 200 files) $ \batch -> runValidator (baseArgs <> batch)
  pure (and results)

runValidator :: [String] -> IO Bool
runValidator args = do
  (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode (proc "nix" args) ""
  unless (null stdoutText) (putStr stdoutText)
  unless (null stderrText) (putStr stderrText)
  pure (exitCode == ExitSuccess)

validatorArgs :: [String] -> [String]
validatorArgs args =
  [ "run"
  , "nixpkgs#check-jsonschema"
  , "--"
  ]
    <> args

listJsonFilesRecursive :: FilePath -> IO [FilePath]
listJsonFilesRecursive root = do
  entries <- listDirectory root
  fmap concat $
    forM entries $ \name -> do
      let path = root </> name
      isDir <- doesDirectoryExist path
      if isDir
        then listJsonFilesRecursive path
        else pure [path | ".json" `isSuffixOf` path]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (prefix, suffix) = splitAt n xs
   in prefix : chunksOf n suffix

commaList :: [String] -> String
commaList = foldr join ""
 where
  join item "" = item
  join item acc = item <> ", " <> acc

isIgnorableWhitespace :: Word8 -> Bool
isIgnorableWhitespace c = c == 32 || c == 9 || c == 13

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  tmp <- fmap trimTrailingNewline $ readProcess "mktemp" ["-d", "/tmp/" <> prefix <> ".XXXXXX"] ""
  createDirectoryIfMissing True tmp
  bracket (pure tmp) removePathForcibly action

trimTrailingNewline :: String -> String
trimTrailingNewline = reverse . dropWhile (== '\n') . reverse

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead [] = Nothing
