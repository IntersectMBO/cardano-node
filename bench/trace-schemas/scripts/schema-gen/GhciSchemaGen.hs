{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Vector as V
import Data.Char (isAlphaNum, isLower, isUpper, isSpace, toLower)
import Data.List (elemIndex, isPrefixOf, isSuffixOf, sortOn)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.Process
import System.Exit
import System.IO
import Data.IORef
import Control.Exception (catch, IOException)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Monad (forM, guard, unless, when)

--------------------------------------------------------------------------------
-- Entry point / high-level flow
--------------------------------------------------------------------------------

rootDirs :: [FilePath]
rootDirs =
  [ "cardano-node/src"
  , "cardano-submit-api/src"
  , "cardano-tracer/src"
  , "../ouroboros-network/ouroboros-network/lib"
  , "../ouroboros-network/ouroboros-network/tracing"
  , "../ouroboros-network/ouroboros-network/protocols/lib"
  , "../ouroboros-network/ouroboros-network/api/lib"
  , "../ouroboros-network/network-mux/src"
  , "../ouroboros-network/cardano-diffusion/lib"
  , "../ouroboros-network/cardano-diffusion/tracing"
  , "../ouroboros-network/cardano-diffusion/protocols/lib"
  , "../ouroboros-network/cardano-diffusion/api/lib"
  , "../ouroboros-network/cardano-diffusion/subscription"
  , "../ouroboros-consensus/ouroboros-consensus/src/ouroboros-consensus"
  , "../ouroboros-consensus/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol"
  , "../ouroboros-consensus/ouroboros-consensus-diffusion/src/ouroboros-consensus-diffusion"
  , "../ouroboros-consensus/ouroboros-consensus-cardano/src"
  , "../hermod-tracing/trace-dispatcher/src"
  , "trace-forward/src"
  , "trace-resources/src"
  ]

type ConstructorName = String
type TypeName = String
type NamespaceParts = [String]
type DetailLevel = String
type HelperFieldMap = Map.Map String (Map.Map String String)

diffusionHandshakeCtor :: ConstructorName
diffusionHandshakeCtor = "__DiffusionHandshakeAnyMessageAndAgency__"

serialisedBlockFetchMsgBlockCtor :: ConstructorName
serialisedBlockFetchMsgBlockCtor = "__SerialisedBlockFetchMsgBlock__"

connectionManagerStateCtor :: ConstructorName
connectionManagerStateCtor = "TrState"

muxStateCtor :: ConstructorName
muxStateCtor = "TraceState"

connectionManagerUnexpectedlyFalseAssertionCtor :: ConstructorName
connectionManagerUnexpectedlyFalseAssertionCtor = "__ConnectionManagerUnexpectedlyFalseAssertion__"

inboundGovernorUnexpectedlyFalseAssertionCtor :: ConstructorName
inboundGovernorUnexpectedlyFalseAssertionCtor = "__InboundGovernorUnexpectedlyFalseAssertion__"

detailLevels :: [DetailLevel]
detailLevels = ["Minimal", "Normal", "Detailed", "Maximum"]

data Config = Config
  { cfgPruneStaleProperties :: Bool
  }

main :: IO ()
main = do
  warningsRef <- newIORef []
  let emitWarning msg = do
        hPutStrLn stderr msg
        modifyIORef' warningsRef (<> [msg])
  config <- parseArgs =<< getArgs
  let nsFile = "bench/trace-schemas/newNamespaces.txt"
  putStrLn $ "Reading namespaces from " <> nsFile
  namespaces <- filter (not . null) . map T.unpack . T.lines <$> T.readFile nsFile
  putStrLn $ "Loaded " <> show (length namespaces) <> " namespace(s)"

  validatedRootDirs <- validateSourceDirs emitWarning rootDirs
  putStrLn "Collecting Haskell source files..."
  hsFiles <- collectTargets validatedRootDirs
  putStrLn $ "Found " <> show (length hsFiles) <> " candidate source file(s)"

  -- Build constructor -> namespace map from namespaceFor clauses
  putStrLn "Building namespace map..."
  let sources = map (\fp -> (fp, readFileSafe fp)) hsFiles
  let nsMap = foldl' mergeNs Map.empty [ normalizeNamespaceMap fp (parseNamespaceMap src) | (fp, src) <- sources ]
  -- Parse forMachine clauses and map their field bindings to variables
  putStrLn "Parsing forMachine clauses..."
  let helperFieldMaps = map (parseObjectHelperMap . snd) sources
  let clausesByFile =
        [ (fp, parseForMachineClauses src, helpers)
        | ((fp, src), helpers) <- zip sources helperFieldMaps
        ]

  let fieldVarMap =
        foldl'
          (Map.unionWith (Map.unionWith Map.union))
          Map.empty
          [ normalizeFieldVarMap fp (parseFieldVarMap helpers clauses)
          | (fp, clauses, helpers) <- clausesByFile
          ]

  -- Ask GHCi for variable types used in forMachine patterns
  putStrLn "Querying GHCi for field types..."
  varTypesMaps <- forM (zip [1 :: Int ..] clausesByFile) $ \(idx, (fp, clauses, _helpers)) -> do
    putStrLn $
      "[ghci " <> show idx <> "/" <> show (length clausesByFile) <> "] "
        <> fp
    normalizeVarTypesMap fp <$> ghciTypesForFile fp clauses
  let varTypes = foldl' (Map.unionWith Map.union) Map.empty varTypesMaps

  let msgOutDir = "bench/trace-schemas/messages"
  let typeOutDir = "bench/trace-schemas/types"
  createDirectoryIfMissing True msgOutDir
  createDirectoryIfMissing True typeOutDir

  putStrLn "Generating schema files..."
  mapM_
    (\(idx, ns) -> do
        putStrLn $
          "[schema " <> show idx <> "/" <> show (length namespaces) <> "] "
            <> ns
        updateSchemaForNamespaceWithWarning emitWarning config msgOutDir typeOutDir nsMap fieldVarMap varTypes ns)
    (zip [1 :: Int ..] namespaces)
  putStrLn "Schema generation complete."
  warnings <- readIORef warningsRef
  unless (null warnings) $ do
    let uniqueWarnings = nubPreserve warnings
    putStrLn ""
    putStrLn $
      "Schema generation warnings summary (" <> show (length uniqueWarnings) <> " unique warning(s)):"
    mapM_ putStrLn uniqueWarnings
  putStrLn $
    "Schema generation problems: " <> show (length (nubPreserve warnings))

parseArgs :: [String] -> IO Config
parseArgs args =
  case args of
    [] -> pure (Config False)
    ["--prune-stale-properties"] -> pure (Config True)
    ["--help"] -> printHelp >> exitSuccess
    ["-h"] -> printHelp >> exitSuccess
    _ -> do
      putStrLn $ "Unrecognized arguments: " <> unwords args
      printHelp
      exitFailure

printHelp :: IO ()
printHelp =
  putStrLn $
    unlines
      [ "Usage: runghc bench/trace-schemas/scripts/schema-gen/GhciSchemaGen.hs [--prune-stale-properties]"
      , ""
      , "Options:"
      , "  --prune-stale-properties  Remove data.properties not supported by current inference."
      ]

-- Utilities

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs = go z xs where go acc [] = acc; go acc (y:ys) = let acc' = f acc y in acc' `seq` go acc' ys

validateSourceDirs :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
validateSourceDirs emitWarning dirs = do
  dirPresence <- forM dirs $ \dir -> do
    exists <- doesDirectoryExist dir
    pure (dir, exists)
  let missingDirs = [dir | (dir, False) <- dirPresence]
      existingDirs = [dir | (dir, True) <- dirPresence]
  mapM_ (\dir -> emitWarning $ "Warning: source directory missing: " <> dir) missingDirs
  when (null existingDirs) $ do
    hPutStrLn stderr "Error: no configured source directories exist for schema generation."
    exitFailure
  pure existingDirs

-- Read file for quick text parsing; tolerate missing files.
readFileSafe :: FilePath -> String
readFileSafe fp = unsafePerformIO (readFile fp `catch` (\(_e :: IOException) -> pure ""))

collectTargets :: [FilePath] -> IO [FilePath]
collectTargets roots = do
  files <- fmap concat $ mapM listHsFiles roots
  fmap catMaybes $ mapM (\fp -> do
    content <- T.readFile fp
    if "forMachine" `T.isInfixOf` content || "namespaceFor" `T.isInfixOf` content
      then pure (Just fp)
      else pure Nothing) files

listHsFiles :: FilePath -> IO [FilePath]
listHsFiles root = do
  exists <- doesDirectoryExist root
  if not exists then pure [] else go root
  where
    go dir = do
      entries <- listDirectory dir
      fmap concat $ mapM (\e -> do
        let p = dir </> e
        isDir <- doesDirectoryExist p
        if isDir then go p else pure [p | ".hs" `isSuffixOf` p]) entries

-- Namespace mapping

mergeNs :: Map.Map ConstructorName [NamespaceParts]
        -> Map.Map ConstructorName [NamespaceParts]
        -> Map.Map ConstructorName [NamespaceParts]
mergeNs = Map.unionWith (++)

-- Parse namespaceFor clauses and collect constructor -> namespace mappings.
parseNamespaceMap :: String -> Map.Map ConstructorName [NamespaceParts]
parseNamespaceMap src = go Nothing (lines src) Map.empty
  where
    typeConstructors = parseTypeConstructors src

    startsHeader line =
      "namespaceFor" `isPrefixOf` trim line

    go _ [] acc = acc
    go currentType allLines@(l:ls) acc
      | startsMetaTraceInstance l =
          let (_hdr, rest, nextType) = spanMetaTraceInstanceHeader allLines
          in go nextType rest acc
      | startsTopLevelDecl l =
          go Nothing ls acc
      | startsHeader l =
          let (headerLines, afterHeader) = spanUntilHeaderEnd [l] ls
              (bodyLines, rest) = spanNamespaceBody afterHeader
              clauseLines = headerLines ++ bodyLines
              header = unwords headerLines
              bodyAfterLambda = dropLeadingLambdaCase bodyLines
              acc' =
                if "\\case" `isInfix` header || isLambdaCaseBody bodyLines
                  then foldl' insertNamespaceClause acc (parseNamespaceLambdaClauses bodyAfterLambda)
                  else if hasBranchArrows bodyLines
                    then case ctorsAfter "namespaceFor" (takeWhile (/= '=') (unwords headerLines)) of
                      [] -> acc
                      ctors -> foldl' insertNamespaceClause acc (parseNamespaceCaseBranches ctors bodyLines)
                  else case extractNamespaceClause currentType typeConstructors (unwords clauseLines) of
                    Just clauseInfo -> insertNamespaceClause acc clauseInfo
                    Nothing -> acc
          in go currentType rest acc'
      | otherwise = go currentType ls acc

    spanUntilHeaderEnd acc [] = (reverse acc, [])
    spanUntilHeaderEnd acc (x:xs)
      | any ("=" `isInfix`) acc = (reverse acc, x:xs)
      | otherwise = spanUntilHeaderEnd (x:acc) xs

    spanNamespaceBody [] = ([], [])
    spanNamespaceBody allLines@(x:xs)
      | startsTopLevelMethod x = ([], allLines)
      | otherwise =
          let (body, rest) = span (not . startsTopLevelMethod) xs
          in (x : body, rest)

    startsTopLevelMethod line =
      let t = trim line
      in any (`isPrefixOf` t)
           [ "namespaceFor"
           , "severityFor"
           , "privacyFor"
           , "detailsFor"
           , "documentFor"
           , "allNamespaces"
           , "metricsDocFor"
           ]

    startsTopLevelDecl line =
      let t = trim line
      in not (null line)
         && not (isSpace (head line))
         && not ("--" `isPrefixOf` t)
         && any (`isPrefixOf` t)
              [ "instance"
              , "data "
              , "newtype "
              , "type "
              , "class "
              ]

    startsMetaTraceInstance line =
      let t = trim line
      in "instance" `isPrefixOf` t && "MetaTrace" `isInfix` t

    spanMetaTraceInstanceHeader :: [String] -> ([String], [String], Maybe TypeName)
    spanMetaTraceInstanceHeader [] = ([], [], Nothing)
    spanMetaTraceInstanceHeader (x:xs) = finish [x] xs
      where
        finish acc [] =
          let hdr = reverse acc
          in (hdr, [], parseMetaTraceInstanceType (unwords hdr))
        finish acc rest@(y:ys)
          | any ("where" `isInfix`) acc =
              let hdr = reverse acc
              in (hdr, rest, parseMetaTraceInstanceType (unwords hdr))
          | otherwise = finish (y:acc) ys

    extractNamespaceClause currentType ctorMap clause = do
      guard ("namespaceFor" `isInfix` clause)
      let lhs = takeWhile (/= '=') clause
      parts <- extractNamespaceParts clause
      let ctors = ctorsAfter "namespaceFor" lhs
      let inferredCtors =
            case ctors of
              [] | isWildcardNamespaceLhs lhs ->
                    maybe [] (\ty -> singletonConstructors ty ctorMap) currentType
              [] | isListEmptyNamespaceLhs lhs ->
                    [listEmptyCtor]
              _ -> ctors
      guard (not (null inferredCtors))
      pure (inferredCtors, parts)

parseMetaTraceInstanceType :: String -> Maybe TypeName
parseMetaTraceInstanceType hdr = do
  (_, rest) <- breakSub "MetaTrace" hdr
  let tyExpr = trim . takeWhile (/= '=') . fst $ breakOn "where" (stripOuterParens (trim rest))
  guard (not (null tyExpr))
  guard (head tyExpr /= '[')
  firstCtorToken tyExpr

isWildcardNamespaceLhs :: String -> Bool
isWildcardNamespaceLhs lhs =
  let marker = ("namespaceFor" :: String)
      after = trim (drop (length marker) lhs)
  in after == "_"

isListEmptyNamespaceLhs :: String -> Bool
isListEmptyNamespaceLhs lhs =
  let marker = ("namespaceFor" :: String)
      after = trim (drop (length marker) lhs)
  in after == "[]"

listEmptyCtor :: ConstructorName
listEmptyCtor = "__list_empty__"

singletonConstructors :: TypeName -> Map.Map TypeName [ConstructorName] -> [ConstructorName]
singletonConstructors ty ctorMap =
  case Map.lookup ty ctorMap of
    Just [ctor] -> [ctor]
    _ -> []

parseTypeConstructors :: String -> Map.Map TypeName [ConstructorName]
parseTypeConstructors src = go (lines src) Map.empty
  where
    go [] acc = acc
    go allLines@(l:ls) acc
      | startsTypeDecl l =
          let (declLines, rest) = spanTypeDecl allLines
              acc' = case parseTypeDecl (unwords declLines) of
                Just (ty, ctors) | not (null ctors) -> Map.insert ty ctors acc
                _ -> acc
          in go rest acc'
      | otherwise = go ls acc

    startsTypeDecl line =
      let t = trim line
      in not (null line)
         && not (isSpace (head line))
         && (("data " `isPrefixOf` t) || ("newtype " `isPrefixOf` t))

    spanTypeDecl [] = ([], [])
    spanTypeDecl (x:xs) = step [x] xs
      where
        step acc [] = (reverse acc, [])
        step acc rest@(y:ys)
          | any hasDeclBody acc && startsNextTopLevel y = (reverse acc, rest)
          | otherwise = step (y:acc) ys

        hasDeclBody line =
          "=" `isInfix` line || " where" `isInfix` line

        startsNextTopLevel line =
          let t = trim line
          in not (null line)
             && not (isSpace (head line))
             && not ("--" `isPrefixOf` t)

    parseTypeDecl decl = do
      let t = trim decl
      keyword <- if ("data " :: String) `isPrefixOf` t
                   then Just ("data " :: String)
                   else if ("newtype " :: String) `isPrefixOf` t
                     then Just ("newtype " :: String)
                     else Nothing
      let afterKeyword = trim (drop (length keyword) t)
          tyName = takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') afterKeyword
      guard (not (null tyName))
      (_, rhsText0) <- breakSub "=" afterKeyword
      let rhsText = trim rhsText0
      let ctorSegments = splitTopLevelBars rhsText
          ctors = mapMaybe firstCtorToken ctorSegments
      guard (not (null ctors))
      pure (tyName, nubPreserve ctors)

splitTopLevelBars :: String -> [String]
splitTopLevelBars s = go 0 0 0 "" [] s
  where
    go _ _ _ cur acc [] = reverse (trim cur : acc)
    go paren bracket brace cur acc (c:cs)
      | c == '(' = go (paren + 1) bracket brace (cur ++ [c]) acc cs
      | c == ')' = go (paren - 1) bracket brace (cur ++ [c]) acc cs
      | c == '[' = go paren (bracket + 1) brace (cur ++ [c]) acc cs
      | c == ']' = go paren (bracket - 1) brace (cur ++ [c]) acc cs
      | c == '{' = go paren bracket (brace + 1) (cur ++ [c]) acc cs
      | c == '}' = go paren bracket (brace - 1) (cur ++ [c]) acc cs
      | c == '|' && paren == 0 && bracket == 0 && brace == 0 =
          go paren bracket brace "" (trim cur : acc) cs
      | otherwise = go paren bracket brace (cur ++ [c]) acc cs

extractNamespaceParts :: String -> Maybe NamespaceParts
extractNamespaceParts clause =
  case extractNsPrependInnerParts clause ++ extractNamespaceLiteralParts clause of
    [] -> Nothing
    parts -> Just parts

extractNsPrependInnerParts :: String -> NamespaceParts
extractNsPrependInnerParts clause =
  case breakOn "nsPrependInner" clause of
    (_, "") -> []
    (_, rhs) -> take 1 (quotedStrings rhs)

extractNamespaceLiteralParts :: String -> NamespaceParts
extractNamespaceLiteralParts clause =
  case breakOn "Namespace" clause of
    (_, "") -> []
    (_, rhs) -> quotedStrings rhs

insertNamespaceClause :: Map.Map ConstructorName [NamespaceParts]
                      -> ([ConstructorName], NamespaceParts)
                      -> Map.Map ConstructorName [NamespaceParts]
insertNamespaceClause acc (ctors, parts) =
  foldl' (\m ctor -> Map.insertWith (++) ctor [parts] m) acc ctors

isLambdaCaseBody :: [String] -> Bool
isLambdaCaseBody (l:_) = "\\case" `isInfix` trim l
isLambdaCaseBody _ = False

dropLeadingLambdaCase :: [String] -> [String]
dropLeadingLambdaCase (l:ls)
  | "\\case" `isInfix` trim l = ls
dropLeadingLambdaCase ls = ls

hasBranchArrows :: [String] -> Bool
hasBranchArrows = any ("->" `isInfix`)

parseNamespaceLambdaClauses :: [String] -> [([ConstructorName], NamespaceParts)]
parseNamespaceLambdaClauses = mapMaybe parseBranch . go
  where
    go [] = []
    go (l:ls)
      | "->" `isInfix` l =
          let (pat0, rhs0) = case breakSub "->" l of
                Just (a, b) -> (trim a, trim b)
                Nothing -> ("", "")
              (more, rest) = spanBranchLines ls
              body = unwords ([rhs0 | not (null rhs0)] ++ more)
          in (pat0, body) : go rest
      | otherwise = go ls

    spanBranchLines = step []
      where
        step acc [] = (reverse acc, [])
        step acc allLines@(x:xs)
          | isBranchStart x = (reverse acc, allLines)
          | otherwise = step (x:acc) xs

    isBranchStart l = "->" `isInfix` l && not (null l) && isSpace (head l)

    parseBranch (pat, body) = do
      let parts = quotedStrings body
      let ctors = ctorTokens pat
      if null parts || null ctors then Nothing else Just (ctors, parts)

parseNamespaceCaseBranches :: [ConstructorName] -> [String] -> [([ConstructorName], NamespaceParts)]
parseNamespaceCaseBranches ctors = mapMaybe parseBranch . go
  where
    go [] = []
    go (l:ls)
      | "->" `isInfix` l =
          let (_pat0, rhs0) = case breakSub "->" l of
                Just (a, b) -> (trim a, trim b)
                Nothing -> ("", "")
              (more, rest) = spanBranchLines ls
              body = unwords ([rhs0 | not (null rhs0)] ++ more)
          in body : go rest
      | otherwise = go ls

    spanBranchLines = step []
      where
        step acc [] = (reverse acc, [])
        step acc allLines@(x:xs)
          | isBranchStart x = (reverse acc, allLines)
          | otherwise = step (x:acc) xs

    isBranchStart l = "->" `isInfix` l && not (null l) && isSpace (head l)

    parseBranch body = do
      let parts = quotedStrings body
      if null parts then Nothing else Just (ctors, parts)


firstCtorAfter :: String -> String -> Maybe String
firstCtorAfter marker line =
  case ctorsAfter marker line of
    (x:_) -> Just x
    [] -> Nothing

ctorsAfter :: String -> String -> [String]
ctorsAfter marker line =
  case T.splitOn (T.pack marker) (T.pack line) of
    (_:rest:_) -> ctorTokens (T.unpack rest)
    _ -> ctorTokens line

firstCtorToken :: String -> Maybe String
firstCtorToken s =
  case normalizedCtorTokens s of
    [] -> Nothing
    xs -> Just (selectCtorToken xs)

ctorTokens :: String -> [String]
ctorTokens s = nubPreserve (normalizedCtorTokens s)

quotedStrings :: String -> [String]
quotedStrings [] = []
quotedStrings ('"':xs) =
  let (a, b) = break (== '"') xs
  in a : quotedStrings (drop 1 b)
quotedStrings (_:xs) = quotedStrings xs

-- forMachine parsing

data Clause = Clause
  { clauseLevel :: String
  , clauseType :: Maybe TypeName
  , clauseCtor :: Maybe ConstructorName
  , clausePattern :: String
  , clauseBody :: [String]
  }

parseObjectHelperMap :: String -> HelperFieldMap
parseObjectHelperMap src =
  Map.fromList
    [ (name, fields)
    | (name, body) <- helperBodies (lines src)
    , let fields = parseHelperFields body
    , not (Map.null fields)
    ]
 where
  helperNames =
    Set.fromList
      [ trim (takeWhile (\c -> isAlphaNum c || c == '_') l)
      | l <- lines src
      , "::" `isInfix` l
      , "Aeson.Object" `isInfix` l
      ]

  helperBodies [] = []
  helperBodies (l:ls)
    | Just name <- helperDefName l
    , name `Set.member` helperNames
    , "= \\case" `isInfix` l =
        let (body, rest) = spanIndented ls
        in (name, body) : helperBodies rest
    | otherwise = helperBodies ls

  helperDefName l =
    let t = trim l
        nm = takeWhile (\c -> isAlphaNum c || c == '_') t
    in if null nm then Nothing else Just nm

  spanIndented = go []
   where
    go acc [] = (reverse acc, [])
    go acc allLines@(x:xs)
      | isTopLevelDecl x = (reverse acc, allLines)
      | otherwise = go (x:acc) xs

  isTopLevelDecl l =
    not (null l)
      && not (isSpace (head l))
      && not ("--" `isPrefixOf` trim l)

  parseHelperFields body =
    foldl' (Map.unionWith preferSpecific) Map.empty (mapMaybe parseBranch (helperBranches body))

  helperBranches [] = []
  helperBranches (l:ls)
    | "->" `isInfix` l =
        let (pat0, rhs0) = case breakSub "->" l of
              Just (a, b) -> (trim a, trim b)
              Nothing -> ("", "")
            (more, rest) = spanBranchLines ls
            body = [rhs0 | not (null rhs0)] ++ more
        in Clause "" Nothing Nothing pat0 body : helperBranches rest
    | otherwise = helperBranches ls

  spanBranchLines = go []
   where
    go acc [] = (reverse acc, [])
    go acc allLines@(x:xs)
      | isBranchStart x = (reverse acc, allLines)
      | isTopLevelDecl x = (reverse acc, allLines)
      | otherwise = go (x:acc) xs

  isBranchStart l = "->" `isInfix` l && not (null l) && isSpace (head l)

  parseBranch cl =
    let vars = Set.fromList (extractVars (clausePattern cl))
        pairs = mapMaybe (parseFieldLine vars) (collectFieldEntries (clauseBody cl))
    in if null pairs then Nothing else Just (Map.fromListWith preferSpecific pairs)

  preferSpecific old new
    | old == literalStringVar = new
    | otherwise = old

-- Extract forMachine function clauses with their patterns and bodies.
parseForMachineClauses :: String -> [Clause]
parseForMachineClauses src = go Nothing (lines src)
  where
    typeConstructors = parseTypeConstructors src

    startsHeader line =
      let t = trim line
      in "forMachine" `isPrefixOf` t || "forMachineGov" `isPrefixOf` t
    hasEquals line = "=" `isInfix` line
    go _ [] = []
    go currentType allLines@(l:ls)
      | startsLogFormattingInstance l =
          let (_hdr, rest, nextType) = spanInstanceHeader "LogFormatting" allLines
          in go nextType rest
      | startsTopLevelDeclForMachine l =
          go Nothing ls
      | startsHeader l =
          let (headerLines, afterHeader) = spanUntilHeaderEnd [l] ls
              header = unwords headerLines
              (lvlTok, pat) = parseHeader header
              (body, rest) = span (not . startsHeader) afterHeader
              bodyAfterLambda = dropLeadingLambdaCase body
              mkClause pat' body' = Clause lvlTok currentType (inferClauseCtor typeConstructors currentType pat') pat' body'
          in if "\\case" `isInfix` header || isLambdaCaseBody body
               then parseLambdaCaseClauses lvlTok currentType typeConstructors bodyAfterLambda ++ go currentType rest
               else expandNestedCaseClauses lvlTok currentType typeConstructors pat body ++ go currentType rest
      | otherwise = go currentType ls

    spanUntilHeaderEnd acc [] = (reverse acc, [])
    spanUntilHeaderEnd acc (x:xs)
      | any hasEquals acc = (reverse acc, x:xs)
      | otherwise = spanUntilHeaderEnd (x:acc) xs

    startsLogFormattingInstance line =
      let t = trim line
      in "instance" `isPrefixOf` t && "LogFormatting" `isInfix` t

    startsTopLevelDeclForMachine line =
      let t = trim line
      in not (null line)
         && not (isSpace (head line))
         && not ("--" `isPrefixOf` t)
         && any (`isPrefixOf` t)
              [ "instance"
              , "data "
              , "newtype "
              , "type "
              , "class "
              ]

parseLambdaCaseClauses :: String -> Maybe TypeName -> Map.Map TypeName [ConstructorName] -> [String] -> [Clause]
parseLambdaCaseClauses lvl currentType typeConstructors = go
 where
  go [] = []
  go (l:ls)
    | "->" `isInfix` l =
        let (pat0, rhs0) = case breakSub "->" l of
              Just (a, b) -> (trim a, trim b)
              Nothing -> ("", "")
            (more, rest) = spanBranchLines ls
            body = [rhs0 | not (null rhs0)] ++ more
        in expandNestedCaseClauses lvl currentType typeConstructors pat0 body ++ go rest
    | otherwise = go ls

  spanBranchLines = step []
   where
    step acc [] = (reverse acc, [])
    step acc allLines@(x:xs)
      | isBranchStart x = (reverse acc, allLines)
      | otherwise = step (x:acc) xs

  isBranchStart l = "->" `isInfix` l && not (null l) && isSpace (head l)

expandNestedCaseClauses :: String -> Maybe TypeName -> Map.Map TypeName [ConstructorName] -> String -> [String] -> [Clause]
expandNestedCaseClauses lvl currentType typeConstructors pat body =
  case splitCaseOnBoundVar (Set.fromList (extractVars pat)) body of
    Just (prefix, branchLines) ->
      Clause lvl currentType (inferClauseCtor typeConstructors currentType pat) pat body
        : concatMap descend (parseLambdaCaseClauses lvl currentType typeConstructors branchLines)
      where
        descend cl = expandNestedCaseClauses lvl currentType typeConstructors (clausePattern cl) (prefix ++ clauseBody cl)
    Nothing ->
      [Clause lvl currentType (inferClauseCtor typeConstructors currentType pat) pat body]

splitCaseOnBoundVar :: Set.Set String -> [String] -> Maybe ([String], [String])
splitCaseOnBoundVar boundVars = go []
  where
    go _ [] = Nothing
    go acc (line:rest) =
      case splitCaseLine boundVars line of
        Just prefixLine ->
          let prefix = reverse acc ++ [prefixLine | not (null (trim prefixLine))]
          in Just (prefix, rest)
        Nothing -> go (line:acc) rest

splitCaseLine :: Set.Set String -> String -> Maybe String
splitCaseLine boundVars line =
  listToMaybe
    [ trim prefix
    | var <- Set.toList boundVars
    , let needle = "case " <> var <> " of"
    , Just (prefix, _) <- [breakSub needle line]
    ]

parseHeader :: String -> (String, String)
parseHeader line =
  let ws = words line
      keyword = case () of
        _ | "forMachineGov" `isInfix` line -> "forMachineGov"
          | otherwise -> "forMachine"
      lvl = case dropWhile (/= keyword) ws of
        (_:l:_) -> l
        _ -> "_"
      pat = case breakSub keyword line of
        Just (_, rest) ->
          let rhs = dropWhile isSpace rest
              afterLvl = dropWhile isSpace (drop (length lvl) rhs)
          in stripOuterParens (trim (takeWhile (/= '=') afterLvl))
        Nothing -> ""
  in (lvl, pat)

spanInstanceHeader :: String -> [String] -> ([String], [String], Maybe TypeName)
spanInstanceHeader className [] = ([], [], Nothing)
spanInstanceHeader className (x:xs) = finish [x] xs
  where
    finish acc [] =
      let hdr = reverse acc
      in (hdr, [], parseInstanceTypeForClass className (unwords hdr))
    finish acc rest@(y:ys)
      | any ("where" `isInfix`) acc =
          let hdr = reverse acc
          in (hdr, rest, parseInstanceTypeForClass className (unwords hdr))
      | otherwise = finish (y:acc) ys

parseInstanceTypeForClass :: String -> String -> Maybe TypeName
parseInstanceTypeForClass className hdr = do
  (_, rest) <- breakSub className hdr
  let tyExpr = trim . takeWhile (/= '=') . fst $ breakOn "where" (stripOuterParens (trim rest))
  guard (not (null tyExpr))
  guard (head tyExpr /= '[')
  firstCtorToken tyExpr

stripOuterParens :: String -> String
stripOuterParens s
  | headMay s == Just '('
  , lastMay s == Just ')'
  , parensWrapWhole s = trim (init (tail s))
  | otherwise = s

parensWrapWhole :: String -> Bool
parensWrapWhole = go 0
  where
    go _ [] = True
    go depth (c:cs)
      | c == '(' = go (depth + 1) cs
      | c == ')' =
          let depth' = depth - 1
          in depth' >= 0
             && (depth' /= 0 || null cs)
             && go depth' cs
      | otherwise = go depth cs

-- Variables that appear in a forMachine pattern.
extractVars :: String -> [String]
extractVars s = filter (not . null) $ filter isVarToken (tokenize s)
  where
    isVarToken (c:_) = isLower c || c == '_'
    isVarToken _ = False

tokenize :: String -> [String]
tokenize = filter (not . null) . splitTokens
  where
    splitTokens [] = []
    splitTokens xs =
      let (tok, rest) = span isTokenChar xs
          rest' = dropWhile (not . isTokenChar) rest
      in tok : splitTokens rest'
    isTokenChar c = isAlphaNum c || c == '_' || c == '\'' || c == '.'

ctorFromPattern :: String -> Maybe String
ctorFromPattern pat = case normalizedCtorTokens pat of
  [] -> Nothing
  xs -> Just (selectCtorToken xs)
  where
    isCtorToken (c:_) = isUpper c
    isCtorToken _ = False

normalizeCtorToken :: String -> String
normalizeCtorToken tok =
  case reverse (splitOn "." tok) of
    (x:_) -> x
    [] -> tok

normalizedCtorTokens :: String -> [String]
normalizedCtorTokens =
  map normalizeCtorToken . filter isCtorToken . tokenize
  where
    isCtorToken (c:_) = isUpper c
    isCtorToken _ = False

nubPreserve :: Ord a => [a] -> [a]
nubPreserve = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs

selectCtorToken :: [String] -> String
selectCtorToken [] = ""
selectCtorToken [x] = x
selectCtorToken xs@(x:_)
  | x `elem` ["AnyMessageAndAgency", "AnyMessage"] = last xs
  | otherwise = x

inferClauseCtor :: Map.Map TypeName [ConstructorName] -> Maybe TypeName -> String -> Maybe ConstructorName
inferClauseCtor typeConstructors currentType pat =
  case ctorFromPattern pat of
    Just ctor -> Just ctor
    Nothing
      | trim pat == "[]" -> Just listEmptyCtor
    Nothing
      | isVariableOnlyPattern pat ->
          currentType >>= \ty ->
            case Map.lookup ty typeConstructors of
              Just [ctor] -> Just ctor
              _ -> Nothing
      | otherwise -> Nothing

isVariableOnlyPattern :: String -> Bool
isVariableOnlyPattern pat =
  case filter (/= "_") (extractVars pat) of
    [_] -> null (normalizedCtorTokens pat)
    _ -> False

-- Map JSON field keys to the pattern variable they come from.
parseFieldVarMap :: HelperFieldMap -> [Clause] -> Map.Map ConstructorName (Map.Map DetailLevel (Map.Map String String))
parseFieldVarMap helperMap clauses = foldl' step Map.empty clauses
  where
    step acc cl =
      case clauseCtor cl of
        Nothing -> acc
        Just ctor ->
          let vars = Set.fromList (extractVars (clausePattern cl))
              lvls = case clauseLevel cl of
                "DMinimal" -> ["Minimal"]
                "DNormal" -> ["Normal"]
                "DDetailed" -> ["Detailed"]
                "DMaximum" -> ["Maximum"]
                _ -> detailLevels
              sanitizedBody = stripCommentBlocks (clauseBody cl)
              directPairs = mapMaybe (parseFieldLine vars) (collectFieldEntries sanitizedBody)
              helperPairs = concatMap helperPairsForLine sanitizedBody
              pairs = Map.toList (Map.fromListWith preferSpecific (directPairs ++ helperPairs))
              addOne m (k,v) = foldl' (\mm lvl -> Map.insertWith (Map.union) lvl (Map.singleton k v) mm) m lvls
          in Map.insertWith (Map.unionWith Map.union) ctor (foldl' addOne Map.empty pairs) acc

    helperPairsForLine line =
      concatMap helperPairsForName (tokenize line)

    helperPairsForName name =
      maybe [] Map.toList (Map.lookup name helperMap)

    preferSpecific old new
      | old == literalStringVar = new
      | old == renderedStringVar && new /= renderedStringVar = new
      | otherwise = old

collectFieldEntries :: [String] -> [String]
collectFieldEntries = finalize . foldl' step []
 where
  step :: [String] -> String -> [String]
  step [] line
    | ".=" `isInfix` line = [line]
    | otherwise = []
  step acc@(cur:rest) line
    | fieldEntryNeedsContinuation cur = (cur <> " " <> trim line) : rest
    | ".=" `isInfix` line = line : acc
    | otherwise = acc

  finalize = concatMap splitFieldFragments . reverse . filter (".=" `isInfix`)

  splitFieldFragments :: String -> [String]
  splitFieldFragments =
    filter (".=" `isInfix`) . map trim . splitTopLevelCommasGeneral . stripOuterList . trim

  stripOuterList s
    | headMay s == Just '[' && lastMay s == Just ']' = init (tail s)
    | otherwise =
        case outerBracketSpan s of
          Just (i, j)
            | trim (take i s) /= ""
            , j == length s - 1 ->
                trim (take (j - i - 1) (drop (i + 1) s))
          _ -> s

  outerBracketSpan :: String -> Maybe (Int, Int)
  outerBracketSpan s =
    case elemIndex '[' s of
      Nothing -> Nothing
      Just start -> matchBracket start 0 start
    where
      matchBracket _ _ i | i >= length s = Nothing
      matchBracket start depth i =
        case s !! i of
          '[' ->
            let depth' = depth + 1
            in matchBracket start depth' (i + 1)
          ']' ->
            let depth' = depth - 1
            in if depth' == 0
                 then Just (start, i)
                 else matchBracket start depth' (i + 1)
          _ -> matchBracket start depth (i + 1)

fieldEntryNeedsContinuation :: String -> Bool
fieldEntryNeedsContinuation = hasOpenBalance . trim
  where
    hasOpenBalance s =
      case go 0 0 0 s of
        (paren, bracket, brace) -> paren > 0 || bracket > 0 || brace > 0

    go paren bracket brace [] = (paren, bracket, brace)
    go paren bracket brace (c:cs)
      | c == '(' = go (paren + 1) bracket brace cs
      | c == ')' = go (paren - 1) bracket brace cs
      | c == '[' = go paren (bracket + 1) brace cs
      | c == ']' = go paren (bracket - 1) brace cs
      | c == '{' = go paren bracket (brace + 1) cs
      | c == '}' = go paren bracket (brace - 1) cs
      | otherwise = go paren bracket brace cs

stripCommentBlocks :: [String] -> [String]
stripCommentBlocks = snd . foldl' step (0 :: Int, [])
  where
    step (depth, acc) line =
      let (depth', cleaned) = stripLine depth line
      in (depth', acc ++ [cleaned])

    stripLine depth [] = (depth, [])
    stripLine depth ('{':'-':cs) = stripLine (depth + 1) cs
    stripLine depth ('-':'}':cs)
      | depth > 0 = stripLine (depth - 1) cs
    stripLine depth ('-':'-':_)
      | depth == 0 = (depth, [])
    stripLine depth (c:cs)
      | depth > 0 = stripLine depth cs
      | otherwise =
          let (depth', rest) = stripLine depth cs
          in (depth', c : rest)

splitTopLevelCommasGeneral :: String -> [String]
splitTopLevelCommasGeneral s = go 0 0 0 "" [] s
  where
    go _ _ _ cur acc [] = reverse (trim cur : acc)
    go paren bracket brace cur acc (c:cs)
      | c == '(' = go (paren + 1) bracket brace (cur ++ [c]) acc cs
      | c == ')' = go (paren - 1) bracket brace (cur ++ [c]) acc cs
      | c == '[' = go paren (bracket + 1) brace (cur ++ [c]) acc cs
      | c == ']' = go paren (bracket - 1) brace (cur ++ [c]) acc cs
      | c == '{' = go paren bracket (brace + 1) (cur ++ [c]) acc cs
      | c == '}' = go paren bracket (brace - 1) (cur ++ [c]) acc cs
      | c == ',' && paren == 0 && bracket == 0 && brace == 0 =
          go paren bracket brace "" (trim cur : acc) cs
      | otherwise = go paren bracket brace (cur ++ [c]) acc cs

parseFieldLine :: Set.Set String -> String -> Maybe (String, String)
parseFieldLine vars line = do
  key <- parseQuotedKey line
  rhs <- parseDotEqRhs line
  case extractForwardedVar vars rhs of
    Just v -> Just (key, v)
    Nothing ->
      case inferRhsMarker rhs of
        Just marker -> Just (key, marker)
        Nothing -> do
          let rhsTokens = Set.fromList (tokenize rhs)
          let hits = filter (`Set.member` rhsTokens) (Set.toList vars)
          case hits of
            [v] -> Just (key, v)
            _ ->
              if isStringLiteral rhs
                then Just (key, literalStringVar)
                else Nothing

extractForwardedVar :: Set.Set String -> String -> Maybe String
extractForwardedVar vars rhs =
  listToMaybe
    [ v
    | v <- Set.toList vars
    , isDirectForwardTo v (trim rhs)
    ]

isDirectForwardTo :: String -> String -> Bool
isDirectForwardTo var rhs =
  any (`matchesCall` rhs)
    [ "forMachine"
    , "forMachineGov"
    , "toObject"
    , "toJSON"
    ]
  where
    matchesCall fn s =
      case words s of
        [] -> False
        (w:rest)
          | w == fn ->
              case reverse rest of
                (lastArg:_) -> normalizeForwardedArg lastArg == var
                _ -> False
          | otherwise -> False

normalizeForwardedArg :: String -> String
normalizeForwardedArg =
  trim . dropTrailingPunctuation . takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'' || c == '.')

dropTrailingPunctuation :: String -> String
dropTrailingPunctuation = reverse . dropWhile (`elem` (")],}" :: String)) . reverse

-- Special marker for string literals (e.g. "kind" .= String "...").
literalStringVar :: String
literalStringVar = "__literal_string__"

renderedStringVar :: String
renderedStringVar = "__rendered_string__"

integerExprVar :: String
integerExprVar = "__integer_expr__"

numberExprVar :: String
numberExprVar = "__number_expr__"

booleanExprVar :: String
booleanExprVar = "__boolean_expr__"

objectExprVar :: String
objectExprVar = "__object_expr__"

arrayExprVar :: String
arrayExprVar = "__array_expr__"

stringArrayExprVar :: String
stringArrayExprVar = "__string_array_expr__"

-- Heuristic detection for String-literal RHS.
isStringLiteral :: String -> Bool
isStringLiteral s =
  "String \"" `isInfix` s || "String '" `isInfix` s

inferRhsMarker :: String -> Maybe String
inferRhsMarker rhs
  | isStringLiteral rhs = Just literalStringVar
  | "Number (fromIntegral" `isInfix` rhs = Just integerExprVar
  | "Number (fromRational" `isInfix` rhs = Just numberExprVar
  | "Number " `isPrefixOf` trim rhs = Just numberExprVar
  | "String " `isPrefixOf` trim rhs = Just renderedStringVar
  | "forMachine " `isInfix` rhs = Just objectExprVar
  | "toObject " `isInfix` rhs = Just objectExprVar
  | "Aeson.object" `isInfix` rhs = Just objectExprVar
  | "object [" `isInfix` rhs = Just objectExprVar
  | "mconcat [" `isInfix` rhs = Just objectExprVar
  | "toJSON [" `isInfix` rhs = Just arrayExprVar
  | "toJSONList (map show" `isInfix` rhs = Just stringArrayExprVar
  | "toJSONList (map showT" `isInfix` rhs = Just stringArrayExprVar
  | "toJSONList (map textShow" `isInfix` rhs = Just stringArrayExprVar
  | "toJSONList (map (show" `isInfix` rhs = Just stringArrayExprVar
  | "toJSONList " `isInfix` rhs = Just arrayExprVar
  | "toJSON (map show" `isInfix` rhs = Just stringArrayExprVar
  | "toJSON (map showT" `isInfix` rhs = Just stringArrayExprVar
  | "toJSON (map textShow" `isInfix` rhs = Just stringArrayExprVar
  | "toJSON (map (show" `isInfix` rhs = Just stringArrayExprVar
  | "toJSON (map (" `isInfix` rhs = Just arrayExprVar
  | "toJSON (Set.toList" `isInfix` rhs = Just arrayExprVar
  | "toJSON (Map.keys" `isInfix` rhs = Just arrayExprVar
  | "toJSON (Map.elems" `isInfix` rhs = Just arrayExprVar
  | "toJSON True" `isInfix` rhs = Just booleanExprVar
  | "toJSON False" `isInfix` rhs = Just booleanExprVar
  | "toJSON (" `isInfix` rhs && any (`isInfix` rhs) [" is", "==", "/=", "&&", "||", " not "] = Just booleanExprVar
  | "String (" `isInfix` rhs = Just renderedStringVar
  | "textShow" `isInfix` rhs = Just renderedStringVar
  | "showT" `isInfix` rhs = Just renderedStringVar
  | "renderHeaderHash" `isInfix` rhs = Just renderedStringVar
  | "renderChainHash" `isInfix` rhs = Just renderedStringVar
  | "renderPoint" `isInfix` rhs = Just renderedStringVar
  | "toJSON (unBlockNo" `isInfix` rhs = Just integerExprVar
  | "toJSON (unSlotNo" `isInfix` rhs = Just integerExprVar
  | "toJSON (fromIntegral" `isInfix` rhs = Just integerExprVar
  | "toJSON (" `isInfix` rhs && any (`isInfix` rhs) ["Word", "Int", "SlotNo", "BlockNo", "EpochNo", "length ", "length(", "fragmentLength"] = Just integerExprVar
  | "toJSON (" `isInfix` rhs && any (`isInfix` rhs) ["Double", "Float", "NominalDiffTime", "DiffTime"] = Just numberExprVar
  | otherwise = Nothing

parseQuotedKey :: String -> Maybe String
parseQuotedKey s = case dropWhile (/= '"') s of
  [] -> Nothing
  (_:rest) -> Just (takeWhile (/= '"') rest)

parseDotEqRhs :: String -> Maybe String
parseDotEqRhs s =
  case breakSub ".=" s of
    Nothing -> Nothing
    Just (_, rhs) -> Just (dropWhile (== ' ') rhs)

breakSub :: String -> String -> Maybe (String, String)
breakSub needle hay =
  let (a, b) = breakOn needle hay
  in if null b then Nothing else Just (a, drop (length needle) b)

breakOn :: String -> String -> (String, String)
breakOn needle hay =
  case T.breakOn (T.pack needle) (T.pack hay) of
    (a, b) -> (T.unpack a, T.unpack b)

-- GHCI type extraction

type VarTypes = Map.Map ConstructorName (Map.Map String String)

-- For each file, ask GHCi for the types of variables extracted from forMachine clauses.
ghciTypesForFile :: FilePath -> [Clause] -> IO VarTypes
ghciTypesForFile fp clauses = do
  let imports = extractImports (readFileSafe fp)
  let queries = mapMaybe buildQuery clauses
  if null queries then pure Map.empty else do
    out <- runGhci imports (map (fst . snd) queries)
    let outputs = splitOutputs out (length queries)
    let pairs = zip queries outputs
    pure $ foldl' mergeVarTypes Map.empty (map parseQueryResult pairs)
  where
    mergeVarTypes = Map.unionWith Map.union

extractImports :: String -> [String]
extractImports src =
  [ stripComment l
  | l <- lines src
  , let l' = dropWhile (== ' ') l
  , "import " `isPrefixOf` l'
  ]
  where
    stripComment l = takeWhile (/= '-') l

buildQuery :: Clause -> Maybe (ConstructorName, (String, [String]))
buildQuery cl = do
  ctor <- clauseCtor cl
  let vars = filter (/= "_") (extractVars (clausePattern cl))
  if null vars then Nothing else do
    let body = if length vars == 1 then head vars else "(" ++ commaSep vars ++ ")"
    let expr = "(\\(" ++ clausePattern cl ++ ") -> " ++ body ++ ")"
    let cmd = ":t " ++ expr
    Just (ctor, (cmd, vars))

commaSep :: [String] -> String
commaSep = foldr1 (\a b -> a ++ ", " ++ b)

runGhci :: [String] -> [String] -> IO String
runGhci imports cmds = do
  let ghciCmd = ["cabal", "repl", "cardano-node", "--repl-options=-ignore-dot-ghci", "--repl-options=-v0"]
  let marker i = "__CMD_" ++ show i ++ "__"
  let cmdLines =
        [":set -XGADTs -XTypeFamilies -XDataKinds -XTypeOperators -XRankNTypes -XScopedTypeVariables"]
        ++ imports
        ++ concat [ [cmd, ":! echo " ++ marker i] | (i, cmd) <- zip [0..] cmds ]
        ++ [":quit"]
  (code, out, err) <- readCreateProcessWithExitCode (proc (head ghciCmd) (tail ghciCmd)) (unlines cmdLines)
  let combined = out ++ err
  case code of
    ExitSuccess -> pure combined
    _ -> pure combined

splitOutputs :: String -> Int -> [String]
splitOutputs out n = map snd (take n (splitMarkers out))
  where
    splitMarkers s =
      let ls = lines s
          go _ [] acc = reverse acc
          go cur (l:rest) acc
            | "__CMD_" `isPrefixOf` l =
                go "" rest ((l, cur) : acc)
            | otherwise = go (cur ++ l ++ "\n") rest acc
      in go "" ls []

parseQueryResult :: ((ConstructorName, (String, [String])), String) -> VarTypes
parseQueryResult ((ctor, (_cmd, vars)), out) =
  let isErr = "error:" `isInfix` out
      varTypes = if isErr
        then parseBindings out
        else parseSignature out vars
  in case varTypes of
      Nothing -> Map.empty
      Just vt -> Map.singleton ctor vt

parseSignature :: String -> [String] -> Maybe (Map.Map String String)
parseSignature out vars = do
  sig <- findSigBlock out
  let parts = parseSigTypes sig
  let pairs = zip vars parts
  pure $ Map.fromList pairs

findSigBlock :: String -> Maybe String
findSigBlock out =
  case break (isInfix "::") (lines out) of
    (_, []) -> Nothing
    (_, sigStart:rest) ->
      let sigLines = sigStart : takeWhile isSigContinuation rest
      in Just (unwords (map trim sigLines))

isSigContinuation :: String -> Bool
isSigContinuation l =
  case l of
    [] -> False
    (c:_) ->
      isSpace c
        && let t = trim l
           in not (null t)
              && not ("ghci>" `isPrefixOf` t)
              && not ("__CMD_" `isPrefixOf` t)

parseSigTypes :: String -> [String]
parseSigTypes line =
  case breakSub "::" line of
    Nothing -> []
    Just (_, rhs0) ->
      let rhs = dropWhile (== ' ') rhs0
          tailTy = last (splitOn "->" rhs)
      in parseTupleTypes (trim tailTy)

parseTupleTypes :: String -> [String]
parseTupleTypes s
  | headMay s == Just '(' && lastMay s == Just ')' && ',' `elem` s =
      splitTopLevelCommas (init (tail s))
  | otherwise = [s]

splitTopLevelCommas :: String -> [String]
splitTopLevelCommas s = go 0 "" [] s
  where
    go _ cur acc [] = reverse (trim cur : acc)
    go depth cur acc (c:cs)
      | c == '(' = go (depth + 1) (cur ++ [c]) acc cs
      | c == ')' = go (depth - 1) (cur ++ [c]) acc cs
      | c == ',' && depth == 0 = go depth "" (trim cur : acc) cs
      | otherwise = go depth (cur ++ [c]) acc cs

-- Parse "x :: T" bindings (and constraints) emitted by GHCi on errors.
parseBindings :: String -> Maybe (Map.Map String String)
parseBindings out =
  let constraints = parseConstraints out
      bindings = mapMaybe parseBindingLine (lines out)
      applyConstraints ty = foldl' (\t (v, rep) -> replaceToken v rep t) ty constraints
      fixed = [ (name, applyConstraints ty) | (name, ty) <- bindings ]
  in if null fixed then Nothing else Just (Map.fromList fixed)

parseConstraints :: String -> [(String, String)]
parseConstraints out = mapMaybe parseConstraintLine (lines out)
  where
    parseConstraintLine l =
      case breakSub "~" l of
        Nothing -> Nothing
        Just (lhs, rhs) ->
          let v = lastWord lhs
              r = headWord rhs
          in if null v || null r then Nothing else Just (v, r)

-- Parse a single binding line "x :: T".
parseBindingLine :: String -> Maybe (String, String)
parseBindingLine l =
  case breakSub "::" l of
    Nothing -> Nothing
    Just (lhs, rhs) ->
      let name = trim (lastWord lhs)
          ty = trim (takeWhile (/= '(') rhs)
      in if null name || null ty then Nothing else Just (name, ty)

replaceToken :: String -> String -> String -> String
replaceToken tok rep s = unwords (map (\w -> if w == tok then rep else w) (words s))

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just (last xs)

splitOn :: String -> String -> [String]
splitOn needle hay = map T.unpack (T.splitOn (T.pack needle) (T.pack hay))

trim :: String -> String
trim = T.unpack . T.strip . T.pack

lastWord :: String -> String
lastWord s = case words s of
  [] -> ""
  xs -> last xs

headWord :: String -> String
headWord s = case words s of
  [] -> ""
  (x:_) -> x

isInfix :: String -> String -> Bool
isInfix needle hay = T.isInfixOf (T.pack needle) (T.pack hay)

-- Schema update

type FieldVarMap = Map.Map ConstructorName (Map.Map DetailLevel (Map.Map String String))

-- Update a single namespace schema on disk.
updateSchemaForNamespace :: Config
                         -> FilePath
                         -> FilePath
                         -> Map.Map ConstructorName [NamespaceParts]
                         -> FieldVarMap
                         -> VarTypes
                         -> String
                         -> IO ()
updateSchemaForNamespace = updateSchemaForNamespaceWithWarning (\_ -> pure ())

updateSchemaForNamespaceWithWarning :: (String -> IO ())
                         -> Config
                         -> FilePath
                         -> FilePath
                         -> Map.Map ConstructorName [NamespaceParts]
                         -> FieldVarMap
                         -> VarTypes
                         -> String
                         -> IO ()
updateSchemaForNamespaceWithWarning emitWarning config msgOutDir typeOutDir nsMap fieldVarMap varTypes ns = do
  let parts = splitDot ns
  let ctor =
        case fallbackCtorForNamespace parts of
          Just c -> Just c
          Nothing ->
            case findCtor nsMap parts of
              Just c -> Just c
              Nothing -> Nothing
  case ctor of
    Nothing ->
      emitWarning $
        "Warning: no source found for namespace: " <> ns
    Just c
      | Map.notMember c fieldVarMap && Map.notMember c varTypes ->
          emitWarning $
            "Warning: namespace resolved to constructor without parsed source/GHCi data: "
              <> ns <> " -> " <> c
    _ -> pure ()
  let out = msgOutDir </> foldr (</>) (last parts ++ ".schema.json") (init parts)
  let histOutDir = "bench/trace-schemas/messages-hist"
  let histOut = histOutDir </> foldr (</>) (last parts ++ ".schema.json") (init parts)
  exists <- doesFileExist out
  schema <- if exists
    then do
      bs <- BL.readFile out
      case A.decode bs of
        Just v -> pure v
        Nothing -> pure (baseSchema ns)
    else pure (baseSchema ns)
  schema' <- updateSchema config typeOutDir ctor fieldVarMap varTypes schema
  -- If we couldn't infer any data properties, fall back to messages-hist.
  schema'' <- mergeHistDataIfEmpty histOut schema'
  createDirectoryIfMissing True (takeDirectory out)
  BL.writeFile out (AP.encodePretty schema'')

fallbackCtorForNamespace :: [String] -> Maybe ConstructorName
fallbackCtorForNamespace parts
  | ["Net", "Handshake"] `isListPrefixOf` parts = Just diffusionHandshakeCtor
  | ["BlockFetch", "Decision"] `isListPrefixOf` parts
  , lastMay parts == Just "EmptyPeersFetch" =
      Just listEmptyCtor
  | ["Net", "ConnectionManager"] `isListPrefixOf` parts
  , lastMay parts == Just "UnexpectedlyFalseAssertion" =
      Just connectionManagerUnexpectedlyFalseAssertionCtor
  | ["Net", "InboundGovernor"] `isListPrefixOf` parts
  , lastMay parts == Just "UnexpectedlyFalseAssertion" =
      Just inboundGovernorUnexpectedlyFalseAssertionCtor
  | ["Net", "ConnectionManager"] `isListPrefixOf` parts
  , lastMay parts == Just "State" = Just connectionManagerStateCtor
  | ["Net", "Mux"] `isListPrefixOf` parts
  , lastMay parts == Just "State" = Just muxStateCtor
  | ["BlockFetch"] `isListPrefixOf` parts
  , "Serialised" `elem` parts
  , lastMay parts == Just "Block" = Just serialisedBlockFetchMsgBlockCtor
  | otherwise = Nothing

isListPrefixOf :: Eq a => [a] -> [a] -> Bool
isListPrefixOf xs ys = xs == take (length xs) ys

normalizeNamespaceMap :: FilePath -> Map.Map ConstructorName [NamespaceParts] -> Map.Map ConstructorName [NamespaceParts]
normalizeNamespaceMap fp
  | takeFileName fp == "Diffusion.hs" =
      renameCtor "AnyMessageAndAgency" diffusionHandshakeCtor
  | takeFileName fp == "P2P.hs" =
      insertAliasFrom "TrUnexpectedlyFalseAssertion" connectionManagerUnexpectedlyFalseAssertionCtor
      . insertAliasFrom "TrUnexpectedlyFalseAssertion" inboundGovernorUnexpectedlyFalseAssertionCtor
  | otherwise = id

normalizeFieldVarMap :: FilePath -> FieldVarMap -> FieldVarMap
normalizeFieldVarMap fp
  | takeFileName fp == "Diffusion.hs" =
      renameCtor "AnyMessageAndAgency" diffusionHandshakeCtor
  | takeFileName fp == "P2P.hs" =
      splitUnexpectedlyFalseAssertion
  | takeFileName fp == "NodeToNode.hs" =
      splitSerialisedBlockFetchMsgBlock
  | otherwise = id

normalizeVarTypesMap :: FilePath -> VarTypes -> VarTypes
normalizeVarTypesMap fp
  | takeFileName fp == "Diffusion.hs" =
      renameCtor "AnyMessageAndAgency" diffusionHandshakeCtor
  | takeFileName fp == "P2P.hs" =
      insertAliasFrom "TrUnexpectedlyFalseAssertion" connectionManagerUnexpectedlyFalseAssertionCtor
      . insertAliasFrom "TrUnexpectedlyFalseAssertion" inboundGovernorUnexpectedlyFalseAssertionCtor
  | takeFileName fp == "NodeToNode.hs" =
      insertAliasFrom "MsgBlock" serialisedBlockFetchMsgBlockCtor
  | otherwise = id

renameCtor :: ConstructorName -> ConstructorName -> Map.Map ConstructorName a -> Map.Map ConstructorName a
renameCtor from to m =
  case Map.lookup from m of
    Just v -> Map.insert to v (Map.delete from m)
    Nothing -> m

insertAliasFrom :: ConstructorName -> ConstructorName -> Map.Map ConstructorName a -> Map.Map ConstructorName a
insertAliasFrom from to m =
  case Map.lookup from m of
    Just v -> Map.insert to v m
    Nothing -> m

splitSerialisedBlockFetchMsgBlock :: FieldVarMap -> FieldVarMap
splitSerialisedBlockFetchMsgBlock m =
  case Map.lookup "MsgBlock" m of
    Nothing -> m
    Just byLevel ->
      let serialisedKeys = Set.fromList ["agency", "bytes", "kind"]
          regular = Map.map (Map.filterWithKey (\k _ -> k /= "bytes")) byLevel
          serialised = Map.map (Map.filterWithKey (\k _ -> Set.member k serialisedKeys)) byLevel
      in Map.insert serialisedBlockFetchMsgBlockCtor serialised (Map.insert "MsgBlock" regular m)

splitUnexpectedlyFalseAssertion :: FieldVarMap -> FieldVarMap
splitUnexpectedlyFalseAssertion m =
  case Map.lookup "TrUnexpectedlyFalseAssertion" m of
    Nothing -> m
    Just byLevel ->
      let connectionManagerKeys = Set.fromList ["kind", "info"]
          inboundGovernorKeys = Set.fromList ["kind", "remoteSt"]
          connectionManager =
            Map.map (Map.filterWithKey (\k _ -> Set.member k connectionManagerKeys)) byLevel
          inboundGovernor =
            Map.map (Map.filterWithKey (\k _ -> Set.member k inboundGovernorKeys)) byLevel
      in Map.insert connectionManagerUnexpectedlyFalseAssertionCtor connectionManager
         (Map.insert inboundGovernorUnexpectedlyFalseAssertionCtor inboundGovernor m)

-- Merge "data" from messages-hist when we couldn't infer any properties.
mergeHistDataIfEmpty :: FilePath -> A.Value -> IO A.Value
mergeHistDataIfEmpty histOut v@(A.Object o) = do
  hasProps <- pure (hasDataProps o)
  if hasProps
    then pure v
    else do
      histExists <- doesFileExist histOut
      if not histExists
        then pure v
        else do
          bs <- BL.readFile histOut
          case A.decode bs of
            Just (A.Object ho) ->
              case KM.lookup (K.fromString "data") ho of
                Just d -> pure (A.Object (KM.insert (K.fromString "data") d o))
                Nothing -> pure v
            _ -> pure v
mergeHistDataIfEmpty _ v = pure v

-- True if data.properties exists and is non-empty.
hasDataProps :: A.Object -> Bool
hasDataProps o =
  case KM.lookup (K.fromString "data") o of
    Just (A.Object d) ->
      case KM.lookup (K.fromString "properties") d of
        Just (A.Object props) -> not (null (KM.toList props))
        _ -> False
    _ -> False

baseSchema :: String -> A.Value
baseSchema ns = A.object ["ns" A..= ns, "data" A..= A.object []]

updateSchema :: Config
             -> FilePath
             -> Maybe ConstructorName
             -> FieldVarMap
             -> VarTypes
             -> A.Value
             -> IO A.Value
updateSchema _ _ Nothing _ _ v = pure v
updateSchema config typeOutDir (Just ctor) fieldVarMap varTypes (A.Object o) = do
  case KM.lookup (K.fromString "variants") o of
    Just (A.Array arr) -> do
      updated <- V.mapM (updateVariant config typeOutDir ctor fieldVarMap varTypes) arr
      pure (A.Object (KM.insert (K.fromString "variants") (A.Array updated) o))
    _ -> do
      updated <- updateData config typeOutDir ctor fieldVarMap varTypes "Minimal" o
      pure (A.Object updated)
updateSchema _ _ _ _ _ v = pure v

updateVariant :: Config
              -> FilePath
              -> ConstructorName
              -> FieldVarMap
              -> VarTypes
              -> A.Value
              -> IO A.Value
updateVariant config typeOutDir ctor fieldVarMap varTypes (A.Object o) =
  case KM.lookup (K.fromString "detailLevel") o of
    Just (A.String lvl) -> do
      updated <- updateData config typeOutDir ctor fieldVarMap varTypes (T.unpack lvl) o
      pure (A.Object updated)
    _ -> pure (A.Object o)
updateVariant _ _ _ _ _ v = pure v

-- Update or synthesize the "data" schema from forMachine mapping + type info.
updateData :: Config
           -> FilePath
           -> ConstructorName
           -> FieldVarMap
           -> VarTypes
           -> DetailLevel
           -> A.Object
           -> IO A.Object
updateData config typeOutDir ctor fieldVarMap varTypes lvl o =
  case KM.lookup (K.fromString "data") o of
    Just (A.Object d) ->
      case KM.lookup (K.fromString "properties") d of
        Just (A.Object props) -> do
          inferredProps <- inferredProperties typeOutDir ctor fieldVarMap varTypes lvl
          let propsToUpdate =
                if cfgPruneStaleProperties config
                  then KM.filterWithKey (\k _ -> KM.member k inferredProps) props
                  else props
          updatedProps <- KM.traverseWithKey (updateProp typeOutDir ctor fieldVarMap varTypes lvl) propsToUpdate
          let mergedProps =
                if cfgPruneStaleProperties config
                  then KM.union updatedProps inferredProps
                  else KM.union updatedProps inferredProps
          let d' = KM.insert (K.fromString "properties") (A.Object mergedProps) d
          let d'' = setRequiredFields ctor fieldVarMap varTypes lvl d'
          pure (KM.insert (K.fromString "data") (A.Object d'') o)
        _ -> do
          d' <- buildDataFromFieldMap typeOutDir ctor fieldVarMap varTypes lvl d
          let d'' = setRequiredFields ctor fieldVarMap varTypes lvl d'
          pure (KM.insert (K.fromString "data") (A.Object d'') o)
    _ -> do
      d <- buildDataFromFieldMap typeOutDir ctor fieldVarMap varTypes lvl KM.empty
      let d' = setRequiredFields ctor fieldVarMap varTypes lvl d
      pure (KM.insert (K.fromString "data") (A.Object d') o)

-- Build a data.schema from the key->var mapping (if available).
buildDataFromFieldMap :: FilePath
                      -> ConstructorName
                      -> FieldVarMap
                      -> VarTypes
                      -> DetailLevel
                      -> A.Object
                      -> IO A.Object
buildDataFromFieldMap typeOutDir ctor fieldVarMap varTypes lvl base = do
  props <- inferredProperties typeOutDir ctor fieldVarMap varTypes lvl
  if KM.null props
    then pure base
    else do
      let base' =
            KM.insert (K.fromString "type") (A.String "object") $
            KM.insert (K.fromString "additionalProperties") (A.Bool True) base
      pure (KM.insert (K.fromString "properties") (A.Object props) base')

inferredProperties :: FilePath
                   -> ConstructorName
                   -> FieldVarMap
                   -> VarTypes
                   -> DetailLevel
                   -> IO A.Object
inferredProperties typeOutDir ctor fieldVarMap varTypes lvl =
  case Map.lookup ctor fieldVarMap >>= Map.lookup lvl of
    Nothing -> pure KM.empty
    Just m -> KM.fromList <$> mapM (buildProp typeOutDir ctor fieldVarMap varTypes) (Map.toList m)

setRequiredFields :: ConstructorName
                  -> FieldVarMap
                  -> VarTypes
                  -> DetailLevel
                  -> A.Object
                  -> A.Object
setRequiredFields ctor fieldVarMap varTypes lvl o =
  case requiredFieldNames ctor fieldVarMap varTypes lvl of
    [] -> o
    reqs -> KM.insert (K.fromString "required") (A.toJSON reqs) o

requiredFieldNames :: ConstructorName
                   -> FieldVarMap
                   -> VarTypes
                   -> DetailLevel
                   -> [String]
requiredFieldNames ctor fieldVarMap varTypes lvl =
  case Map.lookup ctor fieldVarMap >>= Map.lookup lvl of
    Nothing -> []
    Just m ->
      [ key
      | (key, varName) <- Map.toList m
      , isRequiredField varName
      ]
 where
  isRequiredField v
    | v `elem` [ literalStringVar, renderedStringVar, integerExprVar, numberExprVar
               , booleanExprVar, objectExprVar, arrayExprVar, stringArrayExprVar ] = True
    | otherwise =
        case Map.lookup ctor varTypes >>= Map.lookup v of
          Just ty -> not (isMaybe ty)
          Nothing -> True

buildProp :: FilePath
          -> ConstructorName
          -> FieldVarMap
          -> VarTypes
          -> (String, String)
          -> IO (A.Key, A.Value)
buildProp typeOutDir ctor fieldVarMap varTypes (k, v)
  | v `elem` [literalStringVar, renderedStringVar] =
      pure (K.fromString k, A.object ["type" A..= ("string" :: String)])
  | v == integerExprVar =
      pure (K.fromString k, A.object ["type" A..= ("integer" :: String)])
  | v == numberExprVar =
      pure (K.fromString k, A.object ["type" A..= ("number" :: String)])
  | v == booleanExprVar =
      pure (K.fromString k, A.object ["type" A..= ("boolean" :: String)])
  | v == objectExprVar =
      pure (K.fromString k, A.object ["type" A..= ("object" :: String), "additionalProperties" A..= True])
  | v == arrayExprVar =
      pure (K.fromString k, A.object ["type" A..= ("array" :: String)])
  | v == stringArrayExprVar =
      pure (K.fromString k, A.object ["type" A..= ("array" :: String), "items" A..= A.object ["type" A..= ("string" :: String)]])
  | otherwise =
      case Map.lookup ctor varTypes >>= Map.lookup v of
        Nothing -> pure (K.fromString k, fallbackSchemaForUnknownBindingForCtor ctor k v)
        Just ty -> do
          schema <- typeToSchema typeOutDir fieldVarMap varTypes Set.empty ty
          pure (K.fromString k, schema)

updateProp :: FilePath
           -> ConstructorName
           -> FieldVarMap
           -> VarTypes
           -> DetailLevel
           -> A.Key
           -> A.Value
           -> IO A.Value
updateProp typeOutDir ctor fieldVarMap varTypes lvl key old =
  case Map.lookup ctor fieldVarMap >>= Map.lookup lvl >>= Map.lookup (T.unpack (K.toText key)) of
    Nothing -> pure old
    Just v -> snd <$> buildProp typeOutDir ctor fieldVarMap varTypes (T.unpack (K.toText key), v)

-- Namespace matching uses suffix match, so full path or tail matches work.
splitDot :: String -> [String]
splitDot s = case break (== '.') s of
  (a, []) -> [a]
  (a, _:b) -> a : splitDot b

findCtor :: Map.Map ConstructorName [NamespaceParts] -> [String] -> Maybe ConstructorName
findCtor nsMap parts =
  case rankedMatches of
    ((_, ctor):_) -> Just ctor
    [] -> Nothing
 where
  rankedMatches =
    reverse . sortOn fst $
      [ (scoreMatch ctor nsParts, ctor)
      | (ctor, nss) <- Map.toList nsMap
      , nsParts <- nss
      , nsParts `isListSuffixOf` parts
      ]

  scoreMatch :: ConstructorName -> NamespaceParts -> (Int, Int, Int)
  scoreMatch ctor nsParts =
    ( length nsParts
    , overlapScore ctor parts
    , overlapScore ctor nsParts
    )

isListSuffixOf :: Eq a => [a] -> [a] -> Bool
isListSuffixOf xs ys = xs == drop (length ys - length xs) ys

overlapScore :: ConstructorName -> NamespaceParts -> Int
overlapScore ctor nsParts =
  length $
    filter (`Set.member` ctorWords) nsWords
 where
  ctorWords = Set.fromList (splitCamelWords ctor)
  nsWords = concatMap splitCamelWords nsParts

splitCamelWords :: String -> [String]
splitCamelWords =
  filter (not . null)
    . map (map toLower)
    . concatMap splitWord
    . splitOnNonAlphaNum
 where
  splitOnNonAlphaNum [] = []
  splitOnNonAlphaNum xs =
    let xs' = dropWhile (not . isAlphaNum) xs
    in case span isAlphaNum xs' of
         ("", []) -> []
         ("", rest) -> splitOnNonAlphaNum rest
         (tok, rest) -> tok : splitOnNonAlphaNum rest

  splitWord [] = []
  splitWord (c:cs) = go [c] cs

  go cur [] = [reverse cur]
  go cur (x:xs)
    | isUpper x && any isLower cur = reverse cur : go [x] xs
    | otherwise = go (x:cur) xs

-- Type mapping

typeToSchema :: FilePath -> FieldVarMap -> VarTypes -> Set.Set String -> String -> IO A.Value
typeToSchema typeOutDir fieldVarMap varTypes seen ty
  | isList ty = do
      item <- typeToSchema typeOutDir fieldVarMap varTypes seen (listElem ty)
      pure (A.object ["type" A..= ("array" :: String), "items" A..= item])
  | isMaybe ty = do
      item <- typeToSchema typeOutDir fieldVarMap varTypes seen (maybeElem ty)
      pure (A.object ["anyOf" A..= [A.object ["type" A..= ("null" :: String)], item]])
  | baseTypeName ty `elem` ["Text","String","ByteString","ShortByteString","ShortText"] = pure (A.object ["type" A..= ("string" :: String)])
  | baseTypeName ty == "Bool" = pure (A.object ["type" A..= ("boolean" :: String)])
  | baseTypeName ty `elem` ["Int","Integer","Word","Word8","Word16","Word32","Word64","Natural","SlotNo","EpochNo","BlockNo"] = pure (A.object ["type" A..= ("integer" :: String)])
  | baseTypeName ty `elem` ["Double","Float","NominalDiffTime","DiffTime"] = pure (A.object ["type" A..= ("number" :: String)])
  | otherwise = do
      let name = baseTypeName ty
      case name of
        (c:_) | isLower c -> pure (A.object [])
        _ -> do
          let ref = "types/" ++ name ++ ".schema.json"
          let out = typeOutDir </> name ++ ".schema.json"
          if Set.member name seen
            then pure ()
            else ensureTypeSchema out name fieldVarMap varTypes (Set.insert name seen)
          pure (A.object ["$ref" A..= ref])

isList :: String -> Bool
isList s = headMay s == Just '[' && lastMay s == Just ']'

listElem :: String -> String
listElem s = trim (init (tail s))

isMaybe :: String -> Bool
isMaybe s = "Maybe " `isPrefixOf` trim s

maybeElem :: String -> String
maybeElem s = trim (drop (length ("Maybe " :: String)) (trim s))

baseTypeName :: String -> String
baseTypeName s =
  let s' = trim s
      s'' = case stripPrefix "forall " s' of
              Just rest -> case break (== '.') rest of
                (_vars, '.':r) -> trim r
                _ -> s'
              _ -> s'
      tokens = filter (not . null) (tokenize s'')
  in case filter (\t -> case t of (c:_) -> isUpper c; _ -> False) tokens of
      (x:_) -> x
      [] -> s''

stripPrefix :: String -> String -> Maybe String
stripPrefix pre s = if pre `isPrefixOf` s then Just (drop (length pre) s) else Nothing

ensureTypeSchema :: FilePath -> String -> FieldVarMap -> VarTypes -> Set.Set String -> IO ()
ensureTypeSchema out name fieldVarMap varTypes seen = do
  createDirectoryIfMissing True (takeDirectory out)
  schema <- namedTypeSchema fieldVarMap varTypes seen name
  BL.writeFile out (AP.encodePretty schema)

namedTypeSchema :: FieldVarMap -> VarTypes -> Set.Set String -> String -> IO A.Value
namedTypeSchema fieldVarMap varTypes seen name = do
  structural <- structuralTypeSchema fieldVarMap varTypes seen name
  pure $
    case structural of
      A.Object o ->
        A.Object $
          KM.insert (K.fromString "$schema") (A.String "https://json-schema.org/draft/2020-12/schema") $
          KM.insert (K.fromString "title") (A.String (T.pack name)) o
      v -> v

structuralTypeSchema :: FieldVarMap -> VarTypes -> Set.Set String -> String -> IO A.Value
structuralTypeSchema fieldVarMap varTypes seen name
  | isNamedStringType name = pure (A.object ["type" A..= ("string" :: String)])
  | isNamedIntegerType name = pure (A.object ["type" A..= ("integer" :: String)])
  | isNamedNumberType name = pure (A.object ["type" A..= ("number" :: String)])
  | isNamedBooleanType name = pure (A.object ["type" A..= ("boolean" :: String)])
  | isNamedNonEmptyType name =
      pure (A.object ["type" A..= ("array" :: String), "minItems" A..= (1 :: Int)])
  | isNamedWithOriginType name =
      pure (A.object
        [ "oneOf" A..=
            [ A.object
                [ "type" A..= ("object" :: String)
                , "properties" A..= A.object
                    [ "tag" A..= A.object ["type" A..= ("string" :: String)] ]
                , "required" A..= ["tag" :: String]
                , "additionalProperties" A..= True
                ]
            , A.object ["type" A..= ("string" :: String)]
            ]
        ])
  | otherwise =
      case Map.lookup name fieldVarMap of
        Just byLevel | not (Map.null byLevel) -> do
          props <- mergedPropsForType fieldVarMap varTypes seen name byLevel
          pure (A.object
            [ "type" A..= ("object" :: String)
            , "additionalProperties" A..= True
            , "properties" A..= A.Object props
            ])
        _ ->
          pure (A.object
            [ "type" A..= ("object" :: String)
            , "additionalProperties" A..= True
            ])

mergedPropsForType
  :: FieldVarMap
  -> VarTypes
  -> Set.Set String
  -> String
  -> Map.Map DetailLevel (Map.Map String String)
  -> IO A.Object
mergedPropsForType fieldVarMap varTypes seen ctor byLevel = do
  let fieldVars =
        foldl' (Map.unionWith (++)) Map.empty
          [ Map.map (:[]) lvlMap
          | lvlMap <- Map.elems byLevel
          ]
  props <- mapM buildMergedProp (Map.toList fieldVars)
  pure (KM.fromList props)
 where
  buildMergedProp (fieldName, vars) = do
    schemas <- mapM (schemaForVar fieldName) vars
    pure (K.fromString fieldName, mergeSchemas schemas)

  schemaForVar fieldName var
    | var `elem` [literalStringVar, renderedStringVar] =
        pure (A.object ["type" A..= ("string" :: String)])
    | var == integerExprVar =
        pure (A.object ["type" A..= ("integer" :: String)])
    | var == numberExprVar =
        pure (A.object ["type" A..= ("number" :: String)])
    | var == booleanExprVar =
        pure (A.object ["type" A..= ("boolean" :: String)])
    | var == objectExprVar =
        pure (A.object ["type" A..= ("object" :: String), "additionalProperties" A..= True])
    | var == arrayExprVar =
        pure (A.object ["type" A..= ("array" :: String)])
    | var == stringArrayExprVar =
        pure (A.object ["type" A..= ("array" :: String), "items" A..= A.object ["type" A..= ("string" :: String)]])
    | otherwise =
        case Map.lookup ctor varTypes >>= Map.lookup var of
          Nothing -> pure (fallbackSchemaForUnknownBindingForCtor ctor fieldName var)
          Just ty -> typeToSchema "bench/trace-schemas/types" fieldVarMap varTypes seen ty

fallbackSchemaForUnknownBindingForCtor :: ConstructorName -> String -> String -> A.Value
fallbackSchemaForUnknownBindingForCtor ctor key var
  | ctor == "TracePickInboundPeers"
  , lowerKey `elem` ["selected", "available"] =
      A.object ["type" A..= ("array" :: String)]
  | ctor == "FetchingNewLedgerState"
  , lowerKey `elem` ["numberofledgerpeers", "numberofbigledgerpeers"] =
      A.object ["type" A..= ("integer" :: String)]
  | ctor `elem` ["TracePublicRootsResults", "TracePublicRootsFailure", "TraceBigLedgerPeersResults", "TraceBigLedgerPeersFailure"]
  , lowerKey == "difftime" =
      A.object ["type" A..= ("number" :: String)]
  | otherwise = fallbackSchemaForUnknownBinding key var
 where
  lowerKey = map toLower key

fallbackSchemaForUnknownBinding :: String -> String -> A.Value
fallbackSchemaForUnknownBinding key var
  | lowerKey == "port" || lowerVar == "port" || lowerVar == "portno" =
      A.object ["type" A..= ("integer" :: String)]
  | lowerKey `elem` peerSelectionCounterKeys =
      A.object ["type" A..= ("integer" :: String)]
  | otherwise =
      A.object ["type" A..= ("string" :: String)]
 where
  lowerKey = map toLower key
  lowerVar = map toLower var
  peerSelectionCounterKeys =
    [ "targetknown"
    , "actualknown"
    , "targetestablished"
    , "actualestablished"
    , "targetactive"
    , "actualactive"
    ]

mergeSchemas :: [A.Value] -> A.Value
mergeSchemas [] = A.object []
mergeSchemas [x] = x
mergeSchemas xs =
  let unique = dedupeValues xs
  in if length unique == 1 then head unique else A.object ["anyOf" A..= unique]

dedupeValues :: [A.Value] -> [A.Value]
dedupeValues = reverse . foldl' step []
 where
  step acc v =
    let key = BL8.unpack (A.encode v)
    in if any (\existing -> BL8.unpack (A.encode existing) == key) acc
         then acc
         else v : acc

isNamedStringType, isNamedIntegerType, isNamedNumberType, isNamedBooleanType, isNamedNonEmptyType, isNamedWithOriginType :: String -> Bool
isNamedStringType name = name `elem` ["Text", "String", "ByteString", "ShortByteString", "ShortText", "HeaderHash"]
isNamedIntegerType name = name `elem` ["Int", "Integer", "Word", "Word8", "Word16", "Word32", "Word64", "Natural", "SlotNo", "EpochNo", "BlockNo"]
isNamedNumberType name = name `elem` ["Double", "Float", "NominalDiffTime", "DiffTime"]
isNamedBooleanType name = name == "Bool"
isNamedNonEmptyType name = name == "NonEmpty" || ".NonEmpty" `isSuffixOf` name
isNamedWithOriginType name = name == "WithOrigin" || ".WithOrigin" `isSuffixOf` name
