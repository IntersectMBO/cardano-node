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
import Data.Char (isAlphaNum, isLower, isUpper)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes, mapMaybe)
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO
import Control.Exception (catch, IOException)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL

rootDirs :: [FilePath]
rootDirs =
  [ "cardano-node/src"
  , "trace-dispatcher/src"
  , "trace-forward/src"
  , "trace-resources/src"
  ]

type ConstructorName = String
type NamespaceParts = [String]
type DetailLevel = String

detailLevels :: [DetailLevel]
detailLevels = ["Minimal", "Normal", "Detailed", "Maximum"]

main :: IO ()
main = do
  let nsFile = "bench/trace-schemas/newNamespaces.txt"
  namespaces <- filter (not . null) . map T.unpack . T.lines <$> T.readFile nsFile
  hsFiles <- collectTargets rootDirs

  let nsMap = foldl' mergeNs Map.empty (map parseNamespaceMap (map readFileSafe hsFiles))
  let clausesByFile = map (\fp -> (fp, parseForMachineClauses (readFileSafe fp))) hsFiles

  let fieldVarMap = foldl' (Map.unionWith (Map.unionWith Map.union)) Map.empty (map (parseFieldVarMap . snd) clausesByFile)

  varTypesMaps <- mapM (\(fp, clauses) -> ghciTypesForFile fp clauses) clausesByFile
  let varTypes = foldl' (Map.unionWith Map.union) Map.empty varTypesMaps

  let msgOutDir = "bench/trace-schemas/messages"
  let typeOutDir = "bench/trace-schemas/types"
  createDirectoryIfMissing True msgOutDir
  createDirectoryIfMissing True typeOutDir

  mapM_ (updateSchemaForNamespace msgOutDir typeOutDir nsMap fieldVarMap varTypes) namespaces

-- Utilities

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z xs = go z xs where go acc [] = acc; go acc (y:ys) = let acc' = f acc y in acc' `seq` go acc' ys

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

parseNamespaceMap :: String -> Map.Map ConstructorName [NamespaceParts]
parseNamespaceMap src = snd (foldl' step (Nothing, Map.empty) (lines src))
  where
    step (cur, acc) line =
      let cur' = case extractCtor line of
            Just c -> Just c
            Nothing -> cur
      in case (cur', extractNamespaceParts line) of
          (Just c, Just parts) -> (cur', Map.insertWith (++) c [parts] acc)
          _ -> (cur', acc)

    extractCtor line =
      if "namespaceFor" `isInfix` line
        then firstCtorAfter "namespaceFor" line
        else Nothing

    extractNamespaceParts line =
      if "Namespace" `isInfix` line && "[" `isInfix` line && "]" `isInfix` line
        then let parts = quotedStrings line in if null parts then Nothing else Just parts
        else Nothing


firstCtorAfter :: String -> String -> Maybe String
firstCtorAfter marker line =
  case T.splitOn (T.pack marker) (T.pack line) of
    (_:rest:_) -> firstCtorToken (T.unpack rest)
    _ -> firstCtorToken line

firstCtorToken :: String -> Maybe String
firstCtorToken s =
  case filter isCtorToken (tokenize s) of
    (x:_) -> Just x
    _ -> Nothing
  where
    isCtorToken (c:_) = isUpper c
    isCtorToken _ = False

quotedStrings :: String -> [String]
quotedStrings [] = []
quotedStrings ('"':xs) =
  let (a, b) = break (== '"') xs
  in a : quotedStrings (drop 1 b)
quotedStrings (_:xs) = quotedStrings xs

-- forMachine parsing

data Clause = Clause
  { clauseLevel :: String
  , clausePattern :: String
  , clauseBody :: [String]
  }

parseForMachineClauses :: String -> [Clause]
parseForMachineClauses src = go (lines src)
  where
    isHeader line = "forMachine" `isInfix` line && "(" `isInfix` line && ")" `isInfix` line && "=" `isInfix` line
    go [] = []
    go (l:ls)
      | isHeader l =
          let (lvlTok, pat) = parseHeader l
              (body, rest) = span (not . isHeader) ls
          in Clause lvlTok pat body : go rest
      | otherwise = go ls

parseHeader :: String -> (String, String)
parseHeader line =
  let ws = words line
      lvl = case dropWhile (/= "forMachine") ws of
        (_:l:_) -> l
        _ -> "_"
      pat = extractBetween '(' ')' line
  in (lvl, pat)

extractBetween :: Char -> Char -> String -> String
extractBetween a b s =
  case dropWhile (/= a) s of
    [] -> ""
    (_:rest) -> takeWhile (/= b) rest

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
ctorFromPattern pat = case filter isCtorToken (tokenize pat) of
  (x:_) -> Just x
  _ -> Nothing
  where
    isCtorToken (c:_) = isUpper c
    isCtorToken _ = False

parseFieldVarMap :: [Clause] -> Map.Map ConstructorName (Map.Map DetailLevel (Map.Map String String))
parseFieldVarMap clauses = foldl' step Map.empty clauses
  where
    step acc cl =
      case ctorFromPattern (clausePattern cl) of
        Nothing -> acc
        Just ctor ->
          let vars = Set.fromList (extractVars (clausePattern cl))
              lvls = case clauseLevel cl of
                "DMinimal" -> ["Minimal"]
                "DNormal" -> ["Normal"]
                "DDetailed" -> ["Detailed"]
                "DMaximum" -> ["Maximum"]
                _ -> detailLevels
              pairs = mapMaybe (parseFieldLine vars) (clauseBody cl)
              addOne m (k,v) = foldl' (\mm lvl -> Map.insertWith (Map.union) lvl (Map.singleton k v) mm) m lvls
          in Map.insertWith (Map.unionWith Map.union) ctor (foldl' addOne Map.empty pairs) acc

parseFieldLine :: Set.Set String -> String -> Maybe (String, String)
parseFieldLine vars line = do
  key <- parseQuotedKey line
  rhs <- parseDotEqRhs line
  let rhsTokens = Set.fromList (tokenize rhs)
  let hits = filter (`Set.member` rhsTokens) (Set.toList vars)
  case hits of
    [v] -> Just (key, v)
    _ -> Nothing

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
  ctor <- ctorFromPattern (clausePattern cl)
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
  sig <- findSigLine out
  let parts = parseSigTypes sig
  let pairs = zip vars parts
  pure $ Map.fromList pairs

findSigLine :: String -> Maybe String
findSigLine out =
  let ls = lines out
  in case filter (isInfix "::") ls of
      (x:_) -> Just x
      _ -> Nothing

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

updateSchemaForNamespace :: FilePath
                         -> FilePath
                         -> Map.Map ConstructorName [NamespaceParts]
                         -> FieldVarMap
                         -> VarTypes
                         -> String
                         -> IO ()
updateSchemaForNamespace msgOutDir typeOutDir nsMap fieldVarMap varTypes ns = do
  let parts = splitDot ns
  let ctor = findCtor nsMap parts
  let out = msgOutDir </> foldr (</>) (last parts ++ ".schema.json") (init parts)
  exists <- doesFileExist out
  schema <- if exists
    then do
      bs <- BL.readFile out
      case A.decode bs of
        Just v -> pure v
        Nothing -> pure (baseSchema ns)
    else pure (baseSchema ns)
  schema' <- updateSchema typeOutDir ctor fieldVarMap varTypes schema
  createDirectoryIfMissing True (takeDirectory out)
  BL.writeFile out (A.encode schema')

baseSchema :: String -> A.Value
baseSchema ns = A.object ["ns" A..= ns, "data" A..= A.object []]

updateSchema :: FilePath
             -> Maybe ConstructorName
             -> FieldVarMap
             -> VarTypes
             -> A.Value
             -> IO A.Value
updateSchema _ Nothing _ _ v = pure v
updateSchema typeOutDir (Just ctor) fieldVarMap varTypes (A.Object o) = do
  case KM.lookup (K.fromString "variants") o of
    Just (A.Array arr) -> do
      updated <- V.mapM (updateVariant typeOutDir ctor fieldVarMap varTypes) arr
      pure (A.Object (KM.insert (K.fromString "variants") (A.Array updated) o))
    _ -> do
      updated <- updateData typeOutDir ctor fieldVarMap varTypes "Minimal" o
      pure (A.Object updated)
updateSchema _ _ _ _ v = pure v

updateVariant :: FilePath
              -> ConstructorName
              -> FieldVarMap
              -> VarTypes
              -> A.Value
              -> IO A.Value
updateVariant typeOutDir ctor fieldVarMap varTypes (A.Object o) =
  case KM.lookup (K.fromString "detailLevel") o of
    Just (A.String lvl) -> do
      updated <- updateData typeOutDir ctor fieldVarMap varTypes (T.unpack lvl) o
      pure (A.Object updated)
    _ -> pure (A.Object o)
updateVariant _ _ _ _ v = pure v

updateData :: FilePath
           -> ConstructorName
           -> FieldVarMap
           -> VarTypes
           -> DetailLevel
           -> A.Object
           -> IO A.Object
updateData typeOutDir ctor fieldVarMap varTypes lvl o =
  case KM.lookup (K.fromString "data") o of
    Just (A.Object d) ->
      case KM.lookup (K.fromString "properties") d of
        Just (A.Object props) -> do
          updatedProps <- KM.traverseWithKey (updateProp typeOutDir ctor fieldVarMap varTypes lvl) props
          let d' = KM.insert (K.fromString "properties") (A.Object updatedProps) d
          pure (KM.insert (K.fromString "data") (A.Object d') o)
        _ -> pure o
    _ -> pure o

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
    Just v ->
      case Map.lookup ctor varTypes >>= Map.lookup v of
        Nothing -> pure old
        Just ty -> typeToSchema typeOutDir ty

splitDot :: String -> [String]
splitDot s = case break (== '.') s of
  (a, []) -> [a]
  (a, _:b) -> a : splitDot b

findCtor :: Map.Map ConstructorName [NamespaceParts] -> [String] -> Maybe ConstructorName
findCtor nsMap parts =
  let matches =
        [ ctor
        | (ctor, nss) <- Map.toList nsMap
        , any (\nsParts -> nsParts `isListSuffixOf` parts) nss
        ]
  in case matches of
      (x:_) -> Just x
      [] -> Nothing

isListSuffixOf :: Eq a => [a] -> [a] -> Bool
isListSuffixOf xs ys = xs == drop (length ys - length xs) ys

-- Type mapping

typeToSchema :: FilePath -> String -> IO A.Value
typeToSchema typeOutDir ty
  | isList ty = do
      item <- typeToSchema typeOutDir (listElem ty)
      pure (A.object ["type" A..= ("array" :: String), "items" A..= item])
  | isMaybe ty = do
      item <- typeToSchema typeOutDir (maybeElem ty)
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
          ensureTypeStub out name
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

ensureTypeStub :: FilePath -> String -> IO ()
ensureTypeStub out name = do
  exists <- doesFileExist out
  if exists
    then pure ()
    else do
      createDirectoryIfMissing True (takeDirectory out)
      let v = A.object
            [ "$schema" A..= ("https://json-schema.org/draft/2020-12/schema" :: String)
            , "title" A..= name
            , "type" A..= ("object" :: String)
            , "additionalProperties" A..= True
            ]
      BL.writeFile out (A.encode v)
