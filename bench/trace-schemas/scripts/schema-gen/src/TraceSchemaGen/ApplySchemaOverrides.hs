{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TraceSchemaGen.ApplySchemaOverrides
  ( Config (..)
  , defaultConfig
  , defaultRoot
  , findDestructiveOps
  , listOverrideFiles
  , main
  , mergePatch
  , normalizePatch
  , overrideRelToTarget
  , parseArgs
  , processOverride
  , readJsonFile
  , stripPrefixPath
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Control.Monad (forM, unless, when)
import Data.List (isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
  ( (</>)
  , joinPath
  , replaceFileName
  , splitDirectories
  , takeDirectory
  , takeFileName
  )

defaultRoot :: FilePath
defaultRoot = "bench/trace-schemas"

data Config = Config
  { cfgRoot :: FilePath
  , cfgCheck :: Bool
  , cfgVerbose :: Bool
  , cfgAllowDestructive :: Bool
  }
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { cfgRoot = defaultRoot
    , cfgCheck = False
    , cfgVerbose = False
    , cfgAllowDestructive = False
    }

main :: IO ()
main = do
  config <- parseArgs defaultConfig =<< getArgs
  let overridesRoot = cfgRoot config </> "overrides"
  overridesExists <- doesDirectoryExist overridesRoot
  if not overridesExists
    then do
      when (cfgVerbose config) $
        putStrLn $ "No overrides directory at " <> overridesRoot
      exitSuccess
    else do
      overrideFiles <- sort <$> listOverrideFiles overridesRoot
      results <- forM overrideFiles (processOverride config)
      let changedTargets = [target | (target, True) <- results]
      when (cfgCheck config && not (null changedTargets)) $ do
        putStrLn "Schema overrides are not applied (or generated files were edited directly):"
        mapM_ (\fp -> putStrLn $ "  " <> fp) changedTargets
        exitFailure
      when (cfgVerbose config) $ do
        putStrLn $ "Processed " <> show (length overrideFiles) <> " override file(s)."
        putStrLn $ "Updated " <> show (length changedTargets) <> " target schema file(s)."
      exitSuccess

parseArgs :: Config -> [String] -> IO Config
parseArgs = go
 where
  go cfg [] = pure cfg
  go cfg ("--root" : root : rest) = go cfg {cfgRoot = root} rest
  go cfg ("--check" : rest) = go cfg {cfgCheck = True} rest
  go cfg ("--verbose" : rest) = go cfg {cfgVerbose = True} rest
  go cfg ("--allow-destructive" : rest) = go cfg {cfgAllowDestructive = True} rest
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
      [ "Usage: runghc bench/trace-schemas/scripts/schema-gen/ApplySchemaOverrides.hs [options]"
      , ""
      , "Options:"
      , "  --root PATH          trace-schemas root path (default: bench/trace-schemas)"
      , "  --check              Dry-run; fail if any target file would change"
      , "  --verbose            Print each override mapping"
      , "  --allow-destructive  Allow overrides that delete or replace existing fields"
      ]

listOverrideFiles :: FilePath -> IO [FilePath]
listOverrideFiles root = do
  entries <- listDirectory root
  fmap concat $
    forM entries $ \name -> do
      let path = root </> name
      isDir <- doesDirectoryExist path
      if isDir
        then listOverrideFiles path
        else pure [path | ".override.json" `isSuffixOf` name]

processOverride :: Config -> FilePath -> IO (FilePath, Bool)
processOverride config overrideFile = do
  let overridesRoot = cfgRoot config </> "overrides"
  rel <- stripPrefixPath overridesRoot overrideFile
  targetRel <- overrideRelToTarget rel
  let targetFile = cfgRoot config </> targetRel

  targetExists <- doesFileExist targetFile
  unless targetExists $
    failWith $
      "Override target does not exist: " <> targetFile
        <> " (from " <> overrideFile <> ")"

  targetValue <- readJsonFile targetFile
  overrideValue <- readJsonFile overrideFile
  patchValue <- normalizePatch overrideFile overrideValue

  unless (cfgAllowDestructive config) $ do
    let destructiveOps = findDestructiveOps targetValue patchValue
    unless (null destructiveOps) $
      failWith $
        "Override " <> overrideFile <> " contains destructive operations:\n"
          <> unlines (map ("  " <>) destructiveOps)
          <> "Pass --allow-destructive to permit destructive overrides."

  let mergedValue = mergePatch targetValue patchValue
  let changed = mergedValue /= targetValue

  when (cfgVerbose config) $
    putStrLn $ overrideFile <> " -> " <> targetFile

  when (changed && not (cfgCheck config)) $ do
    createDirectoryIfMissing True (takeDirectory targetFile)
    BL.writeFile targetFile (AP.encodePretty mergedValue)

  pure (targetFile, changed)

readJsonFile :: FilePath -> IO A.Value
readJsonFile fp = do
  result <- A.eitherDecodeFileStrict' fp
  case result of
    Left err -> failWith $ "Invalid JSON in " <> fp <> ": " <> err
    Right v -> pure v

normalizePatch :: FilePath -> A.Value -> IO A.Value
normalizePatch source v@(A.Object o) =
  case KM.lookup (K.fromString "patch") o of
    Nothing -> pure v
    Just patch@(A.Object _) -> pure patch
    Just _ ->
      failWith $
        "Override \"patch\" must be an object in " <> source
normalizePatch source _ =
  failWith $ "Override must be a JSON object: " <> source

findDestructiveOps :: A.Value -> A.Value -> [String]
findDestructiveOps target patch = goObj "" targetObj patchObj
  where
    targetObj = case target of { A.Object o -> o; _ -> KM.empty }
    patchObj = case patch of { A.Object o -> o; _ -> KM.empty }

    goObj path tObj pObj = concatMap (checkKey path tObj) (KM.toList pObj)

    checkKey path tObj (k, pv) =
      let kStr = K.toString k
          keyPath = if null path then kStr else path <> "." <> kStr
       in case (KM.lookup k tObj, pv) of
            (Nothing, _) -> []
            (Just _, A.Null) -> [keyPath <> ": field deletion"]
            (Just (A.Object tv), A.Object pv') -> goObj keyPath tv pv'
            (Just _, _) -> [keyPath <> ": field replacement"]

mergePatch :: A.Value -> A.Value -> A.Value
mergePatch _ A.Null = A.Null
mergePatch _ patch@(A.Bool _) = patch
mergePatch _ patch@(A.String _) = patch
mergePatch _ patch@(A.Number _) = patch
mergePatch _ patch@(A.Array _) = patch
mergePatch target (A.Object patchObj) =
  let targetObj =
        case target of
          A.Object o -> o
          _ -> KM.empty
   in A.Object (mergeObject targetObj patchObj)

mergeObject :: A.Object -> A.Object -> A.Object
mergeObject = KM.foldrWithKey step
 where
  step key value acc =
    case value of
      A.Null -> KM.delete key acc
      A.Object _ ->
        let existing = fromMaybe A.Null (KM.lookup key acc)
            merged = mergePatch existing value
         in KM.insert key merged acc
      _ -> KM.insert key value acc

stripPrefixPath :: FilePath -> FilePath -> IO FilePath
stripPrefixPath prefix full =
  case stripPrefixList (splitDirectories prefix) (splitDirectories full) of
    Just suffix -> pure (joinPath suffix)
    Nothing ->
      failWith $
        "Path " <> full <> " is not under " <> prefix

overrideRelToTarget :: FilePath -> IO FilePath
overrideRelToTarget rel = do
  let name = takeFileName rel
  let suffix = ".override.json" :: String
  unless (suffix `isSuffixOf` name) $
    failWith $ "Override must end with .override.json: " <> rel
  let targetName = take (length name - length suffix) name <> ".json"
  pure (replaceFileName rel targetName)

stripPrefixList :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefixList [] ys = Just ys
stripPrefixList _ [] = Nothing
stripPrefixList (x : xs) (y : ys)
  | x == y = stripPrefixList xs ys
  | otherwise = Nothing

failWith :: String -> IO a
failWith msg = putStrLn ("ERROR: " <> msg) >> exitFailure
