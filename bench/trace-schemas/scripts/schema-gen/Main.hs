{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import GHC
import GHC.Driver.Session
import GHC.Driver.Flags (GeneralFlag (Opt_HideAllPackages))
import GHC.LanguageExtensions.Type (Extension(..))
import GHC.Driver.Session (PackageFlag(..), PackageArg(..), ModRenaming(..))
import GHC.Unit.Types (Definite (..), GenUnit (..), UnitId (..))
-- import GHC.Utils.Outputable
import GHC.Utils.Misc (filterOut)
import GHC.Data.FastString (unpackFS, fsLit)
import GHC.Types.Name.Reader (rdrNameOcc, mkVarUnqual)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name (getOccName)
import GHC.Types.SrcLoc
import GHC.Core.Type
import GHC.Core.TyCon (tyConName)
import GHC.Core.TyCo.Rep (Scaled(..))
import GHC.Builtin.Types (unitTy)
import GHC.Types.TyThing
import GHC.Types.Var
import GHC.Types.Id
import GHC.Core.DataCon
import GHC.Hs
import GHC.Data.Bag
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes, mapMaybe, listToMaybe)
import Data.List (isPrefixOf, isSuffixOf, stripPrefix)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import System.Directory
import System.FilePath
import System.Environment (lookupEnv)
import System.Process (readProcess)
import System.IO (hSetBuffering, BufferMode (LineBuffering), stdout, stderr, hPutStrLn)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import GHC.Types.FieldLabel (FieldLabel, flLabel)
import Language.Haskell.Syntax.Basic (field_label)
import Data.Maybe (maybeToList)

mergeTypes :: Type -> Type -> Type
mergeTypes a _ = a

readPackageEnvIds :: FilePath -> IO [String]
readPackageEnvIds path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      contents <- readFile path
      pure
        [ drop (length ("package-id " :: String)) line
        | line <- lines contents
        , "package-id " `isPrefixOf` line
        ]

getUnitIdFromGhcPkg :: String -> IO (Maybe String)
getUnitIdFromGhcPkg pkgName = do
  res <- readProcess "ghc-pkg" ["field", pkgName, "id"] ""
  let ls = lines res
  pure $ case ls of
    (line:_) | "id: " `isPrefixOf` line -> Just (drop (length ("id: " :: String)) line)
    _ -> Nothing


showPkgFlag :: PackageFlag -> String
showPkgFlag (ExposePackage name arg _) = "ExposePackage " ++ name ++ " " ++ show arg
showPkgFlag (HidePackage name) = "HidePackage " ++ name

basePkgName :: String -> String
basePkgName s =
  let s' = maybe s id (stripPrefix "z-" s)
  in case T.splitOn "-z-" (T.pack s') of
       (x:_) -> T.unpack x
       [] -> s'

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  mbLibdir <- lookupEnv "GHC_LIBDIR"
  case mbLibdir of
    Nothing -> putStrLn "GHC_LIBDIR is not set; run with `GHC_LIBDIR=$(ghc --print-libdir)`."
    Just libdir -> runGhc (Just libdir) $ do
      dflags0 <- getSessionDynFlags
      let roots =
            [ "cardano-node/src"
            , "trace-dispatcher/src"
            , "trace-forward/src"
            , "trace-resources/src"
            ]
      pathsDir <- liftIO $ findPathsModule "Paths_cardano_node.hs"
      envFile <- liftIO (lookupEnv "GHC_ENVIRONMENT")
      let envFile' = case envFile of
            Just "-" -> Nothing
            other -> other
      let basePkgs =
            [ "ouroboros-network"
            , "typed-protocols"
            , "io-classes"
            ]
      baseUnitIds <- liftIO $ mapM getUnitIdFromGhcPkg basePkgs
      let sublibPkgs =
            [ "z-ouroboros-network-z-cardano-diffusion"
            , "z-ouroboros-network-z-orphan-instances"
            , "z-typed-protocols-z-stateful"
            , "z-io-classes-z-si-timers"
            ]
      let sublibExposeNames = map basePkgName sublibPkgs
      sublibUnitIds <- liftIO $ mapM getUnitIdFromGhcPkg sublibPkgs
      let pkgFlags =
            [ ExposePackage pkgName (UnitIdArg (RealUnit (Definite (UnitId (fsLit unitId))))) (ModRenaming True [])
            | (pkgName, Just unitId) <- zip basePkgs baseUnitIds
            ]
              ++ [ ExposePackage exposeName (UnitIdArg (RealUnit (Definite (UnitId (fsLit unitId))))) (ModRenaming True [])
                 | (exposeName, Just unitId) <- zip sublibExposeNames sublibUnitIds
                 ]
              ++ [HidePackage "optparse-applicative-fork"]
      let dflags1 =
            (gopt_unset dflags0 Opt_HideAllPackages)
              { importPaths = roots ++ maybeToList pathsDir ++ importPaths dflags0
              , packageFlags = pkgFlags ++ packageFlags dflags0
              , packageEnv = envFile'
              }
      let dflags = foldl xopt_set dflags1 [OverloadedStrings, LambdaCase]
      _ <- setSessionDynFlags dflags
      dflagsApplied <- getSessionDynFlags
      liftIO $ hPutStrLn stderr $ "SchemaGen: packageEnv=" ++ show (packageEnv dflagsApplied)
      liftIO $ hPutStrLn stderr $ "SchemaGen: hideAllPackages=" ++ show (gopt Opt_HideAllPackages dflagsApplied)
      let flagsRendered = map showPkgFlag (packageFlags dflagsApplied)
      liftIO $ hPutStrLn stderr $ "SchemaGen: packageFlags count=" ++ show (length flagsRendered)
      liftIO $ mapM_ (hPutStrLn stderr) flagsRendered
      liftIO $ putStrLn "SchemaGen: GHC session initialized."
      files <- liftIO $ collectTargets roots
      targets <- mapM (\fp -> guessTarget fp (Just (homeUnitId_ dflags)) Nothing) files
      setTargets targets
      _ <- load LoadAllTargets
      liftIO $ putStrLn $ "SchemaGen: loaded targets: " ++ show (length files)

      nsMap <- buildNamespaceMap files
      fmMap <- buildForMachineFieldMap files

      nsList <- liftIO $ fmap T.lines (T.readFile "bench/trace-schemas/newNamespaces.txt")
      let namespaces = map T.unpack $ filter (not . T.null) nsList

      let typeOutDir = "bench/trace-schemas/types"
      let msgOutDir = "bench/trace-schemas/messages"
      liftIO $ createDirectoryIfMissing True typeOutDir
      liftIO $ createDirectoryIfMissing True msgOutDir
      liftIO $ generateSchemas msgOutDir typeOutDir nsMap fmMap namespaces
      liftIO $ putStrLn "SchemaGen: done."

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

collectTargets :: [FilePath] -> IO [FilePath]
collectTargets roots = do
  files <- fmap concat $ mapM listHsFiles roots
  fmap catMaybes $ forM files $ \fp -> do
    content <- T.readFile fp
    if "forMachine" `T.isInfixOf` content || "namespaceFor" `T.isInfixOf` content
      then pure (Just fp)
      else pure Nothing

listHsFiles :: FilePath -> IO [FilePath]
listHsFiles root = do
  exists <- doesDirectoryExist root
  if not exists then pure [] else go root
  where
    go dir = do
      entries <- listDirectory dir
      fmap concat $ forM entries $ \e -> do
        let p = dir </> e
        isDir <- doesDirectoryExist p
        if isDir
          then go p
          else pure [p | ".hs" `isSuffixOf` p]

findPathsModule :: FilePath -> IO (Maybe FilePath)
findPathsModule fname = do
  let root = "dist-newstyle"
  exists <- doesDirectoryExist root
  if not exists
    then pure Nothing
    else do
      matches <- findByName root
      pure $ fmap takeDirectory (listToMaybe matches)
  where
    findByName dir = do
      entries <- listDirectory dir
      fmap concat $ forM entries $ \e -> do
        let p = dir </> e
        isDir <- doesDirectoryExist p
        if isDir
          then findByName p
          else pure [p | takeFileName p == fname]

--------------------------------------------------------------------------------
-- Namespace mapping
--------------------------------------------------------------------------------

type ConstructorName = String
type NamespaceParts = [String]

buildNamespaceMap :: [FilePath] -> Ghc (Map.Map ConstructorName [NamespaceParts])
buildNamespaceMap files = do
  pairs <- fmap concat $ forM files $ \fp -> do
    ms <- getModSummaryFromFile fp
    case ms of
      Nothing -> pure []
      Just modSum -> do
        p <- parseModule modSum
        let L _ modl = pm_parsed_source p
        pure (extractNamespaceFor modl)
  pure $ Map.fromListWith (++) pairs

getModSummaryFromFile :: FilePath -> Ghc (Maybe ModSummary)
getModSummaryFromFile fp = do
  graph <- getModuleGraph
  let matches = filter (\ms -> ms_hspp_file ms == fp) (mgModSummaries graph)
  pure $ case matches of
    (x:_) -> Just x
    []    -> Nothing

extractNamespaceFor :: HsModule GhcPs -> [(ConstructorName, [NamespaceParts])]
extractNamespaceFor modl =
  concatMap fromBind (hsmodDecls modl)
  where
    fromBind :: LHsDecl GhcPs -> [(ConstructorName, [NamespaceParts])]
    fromBind (L _ (ValD _ (FunBind _ (L _ name) mg))) =
      if occNameString (rdrNameOcc name) == "namespaceFor"
        then concatMap fromMatch (unLoc (mg_alts mg))
        else []
    fromBind _ = []

    fromMatch :: LMatch GhcPs (LHsExpr GhcPs) -> [(ConstructorName, [NamespaceParts])]
    fromMatch (L _ (Match _ _ pats grhss)) =
      case pats of
        (_:p:_) -> case constructorName p of
          Nothing -> []
          Just ctor ->
            let ns = extractNamespaceParts grhss
            in map (\nsp -> (ctor, [nsp])) ns
        _ -> []

    constructorName :: LPat GhcPs -> Maybe ConstructorName
    constructorName (L _ ConPat{pat_con = L _ con}) = Just (occNameString (rdrNameOcc con))
    constructorName (L _ (AsPat _ _ _ p)) = constructorName p
    constructorName _ = Nothing

    extractNamespaceParts :: GRHSs GhcPs (LHsExpr GhcPs) -> [NamespaceParts]
    extractNamespaceParts (GRHSs _ grhss _) =
      concatMap fromGRHS grhss

    fromGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> [NamespaceParts]
    fromGRHS (L _ (GRHS _ _ expr)) = extractNamespaceFromExpr expr

    extractNamespaceFromExpr :: LHsExpr GhcPs -> [NamespaceParts]
    extractNamespaceFromExpr (L _ (HsApp _ (L _ (HsApp _ (L _ (HsVar _ (L _ fn))) (L _ (ExplicitList _ parts)))) _))
      | occNameString (rdrNameOcc fn) == "Namespace" =
          [mapMaybe litStr parts]
    extractNamespaceFromExpr (L _ (HsApp _ (L _ (HsVar _ (L _ fn))) (L _ (ExplicitList _ parts))))
      | occNameString (rdrNameOcc fn) == "Namespace" =
          [mapMaybe litStr parts]
    extractNamespaceFromExpr _ = []

    litStr :: LHsExpr GhcPs -> Maybe String
    litStr (L _ (HsLit _ (HsString _ fs))) = Just (T.unpack (T.pack (unpackFS fs)))
    litStr _ = Nothing

--------------------------------------------------------------------------------
-- forMachine field mapping
--------------------------------------------------------------------------------

data DetailLevel = Minimal | Normal | Detailed | Maximum deriving (Eq, Ord, Show)

allLevels :: Set.Set DetailLevel
allLevels = Set.fromList [Minimal, Normal, Detailed, Maximum]

levelFromCon :: String -> Maybe DetailLevel
levelFromCon = \case
  "DMinimal"  -> Just Minimal
  "DNormal"   -> Just Normal
  "DDetailed" -> Just Detailed
  "DMaximum"  -> Just Maximum
  _           -> Nothing

detailName :: DetailLevel -> String
detailName = show

type FieldMap = Map.Map ConstructorName (Map.Map DetailLevel (Map.Map String Type))

buildForMachineFieldMap :: [FilePath] -> Ghc FieldMap
buildForMachineFieldMap files = do
  maps <- fmap catMaybes $ forM files $ \fp -> do
    ms <- getModSummaryFromFile fp
    case ms of
      Nothing -> pure Nothing
      Just modSum -> do
        p <- parseModule modSum
        t <- typecheckModule p
        let tcs = tm_typechecked_source t
        pure (Just (extractForMachineFields tcs))
  pure $ Map.unionsWith (Map.unionWith (Map.unionWith mergeTypes)) maps
  where
    mergeTypes a _ = a

extractForMachineFields :: TypecheckedSource -> FieldMap
extractForMachineFields binds = Map.fromListWith (Map.unionWith (Map.unionWith mergeTypes)) (concatMap fromBind (bagToList binds))
  where
    mergeTypes a _ = a

    fromBind :: LHsBind GhcTc -> [(ConstructorName, Map.Map DetailLevel (Map.Map String Type))]
    fromBind (L _ FunBind{fun_id = L _ fid, fun_matches = mg}) =
      if occNameString (getOccName (idName fid)) == "forMachine"
        then concatMap fromMatch (unLoc (mg_alts mg))
        else []
    fromBind _ = []

    fromMatch :: LMatch GhcTc (LHsExpr GhcTc) -> [(ConstructorName, Map.Map DetailLevel (Map.Map String Type))]
    fromMatch (L _ (Match _ _ pats grhss)) =
      case pats of
        (p1:p2:_) ->
          let lvlFromPat = patLevel p1
              ctor = patConstructor p2
          in case ctor of
              Nothing -> []
              Just c  ->
                let fieldMap = extractFields grhss lvlFromPat
                in [(c, fieldMap)]
        _ -> []

    patLevel :: LPat GhcTc -> Maybe DetailLevel
    patLevel (L _ ConPat{pat_con = L _ con}) =
      levelFromCon (occNameString (getOccName con))
    patLevel _ = Nothing

    patConstructor :: LPat GhcTc -> Maybe ConstructorName
    patConstructor (L _ ConPat{pat_con = L _ con}) =
      Just (occNameString (getOccName con))
    patConstructor (L _ (AsPat _ _ _ p)) = patConstructor p
    patConstructor _ = Nothing

    extractFields :: GRHSs GhcTc (LHsExpr GhcTc) -> Maybe DetailLevel -> Map.Map DetailLevel (Map.Map String Type)
    extractFields (GRHSs _ grhss _) lvlFromPat =
      let base = Map.fromList [(lvl, Map.empty) | lvl <- [Minimal, Normal, Detailed, Maximum]]
          add = foldr mergeLevels base (map (fromGRHS lvlFromPat) grhss)
      in add

    fromGRHS :: Maybe DetailLevel -> LGRHS GhcTc (LHsExpr GhcTc) -> Map.Map DetailLevel (Map.Map String Type)
    fromGRHS lvlFromPat (L _ (GRHS _ _ expr)) =
      collectExprFields lvlFromPat allLevels expr

    mergeLevels :: Map.Map DetailLevel (Map.Map String Type)
                -> Map.Map DetailLevel (Map.Map String Type)
                -> Map.Map DetailLevel (Map.Map String Type)
    mergeLevels = Map.unionWith (Map.unionWith mergeTypes)

collectExprFields :: Maybe DetailLevel -> Set.Set DetailLevel -> LHsExpr GhcTc -> Map.Map DetailLevel (Map.Map String Type)
collectExprFields lvlFromPat active expr =
  case unLoc expr of
    HsIf _ cond thenE elseE ->
      case splitCondLevels cond of
        Nothing ->
          merge (collectExprFields lvlFromPat active thenE) (collectExprFields lvlFromPat active elseE)
        Just (thenLvls, elseLvls) ->
          merge (collectExprFields lvlFromPat thenLvls thenE) (collectExprFields lvlFromPat elseLvls elseE)
    HsCase _ scrut mg ->
      case scrutLevel scrut of
        Just lvls ->
          foldr merge emptyMap
            [ collectExprFields (Just lvl) (Set.singleton lvl) e
            | (lvl, m) <- zip lvls (unLoc (mg_alts mg))
            , e <- matchBodies m
            ]
        Nothing ->
          foldr merge emptyMap
            [ collectExprFields lvlFromPat active e
            | m <- unLoc (mg_alts mg)
            , e <- matchBodies m
            ]
    OpApp _ lhs op rhs ->
      case isDotEq op of
        True -> case (extractKey lhs, inferType rhs) of
          (Just k, Just t) -> assign k t
          (Just k, Nothing) -> assign k unitTy
          _ -> emptyMap
        False -> merge (collectExprFields lvlFromPat active lhs) (collectExprFields lvlFromPat active rhs)
    HsApp _ a b ->
      merge (collectExprFields lvlFromPat active a) (collectExprFields lvlFromPat active b)
    HsLam _ mg ->
      foldr merge emptyMap
        [ collectExprFields lvlFromPat active e
        | m <- unLoc (mg_alts mg)
        , e <- matchBodies m
        ]
    HsDo _ _ (L _ stmts) ->
      foldr merge emptyMap (mapMaybe (stmtExpr lvlFromPat active) stmts)
    ExplicitList _ es ->
      foldr merge emptyMap (map (collectExprFields lvlFromPat active) es)
    _ -> emptyMap
  where
    emptyMap = Map.fromList [(lvl, Map.empty) | lvl <- [Minimal, Normal, Detailed, Maximum]]
    assign k t =
      let levels = maybe active (Set.singleton) lvlFromPat
          addOne lvl = Map.singleton lvl (Map.singleton k t)
      in Map.unionsWith (Map.unionWith mergeTypes) (map addOne (Set.toList levels))
    merge = Map.unionWith (Map.unionWith mergeTypes)

collectBinds :: Maybe DetailLevel -> Set.Set DetailLevel -> HsLocalBinds GhcTc -> Map.Map DetailLevel (Map.Map String Type)
collectBinds lvlFromPat active = \case
  HsValBinds _ (ValBinds _ binds _) ->
    foldr (Map.unionWith (Map.unionWith mergeTypes)) emptyMap (map (collectExprFields lvlFromPat active . grhsBody) (bindGRHS binds))
  _ -> emptyMap
  where
    emptyMap = Map.fromList [(lvl, Map.empty) | lvl <- [Minimal, Normal, Detailed, Maximum]]

bindGRHS :: LHsBinds GhcTc -> [LGRHS GhcTc (LHsExpr GhcTc)]
bindGRHS binds =
  concatMap fromBind (bagToList binds)
  where
    fromBind (L _ FunBind{fun_matches = mg}) =
      concatMap (\(L _ (Match _ _ _ (GRHSs _ grhss _))) -> grhss) (unLoc (mg_alts mg))
    fromBind _ = []

stmtExpr :: Maybe DetailLevel -> Set.Set DetailLevel -> ExprLStmt GhcTc -> Maybe (Map.Map DetailLevel (Map.Map String Type))
stmtExpr lvlFromPat active (L _ stmt) = case stmt of
  BodyStmt _ e _ _ -> Just (collectExprFields lvlFromPat active e)
  LetStmt _ binds -> Just (collectBinds lvlFromPat active binds)
  _ -> Nothing

grhsBody :: LGRHS GhcTc (LHsExpr GhcTc) -> LHsExpr GhcTc
grhsBody (L _ (GRHS _ _ e)) = e

matchBodies :: LMatch GhcTc (LHsExpr GhcTc) -> [LHsExpr GhcTc]
matchBodies (L _ (Match _ _ _ (GRHSs _ grhss _))) = map grhsBody grhss

inferType :: LHsExpr GhcTc -> Maybe Type
inferType (L _ (HsVar _ (L _ v))) = Just (idType v)
inferType (L _ (HsApp _ f x)) =
  case f of
    L _ (HsVar _ (L _ v))
      | occNameString (getOccName v) `elem` ["toJSON", "forMachine", "toObject", "toJSONList"] ->
          inferType x
    _ -> inferType x
inferType (L _ (HsPar _ _ e _)) = inferType e
inferType _ = Nothing

isDotEq :: LHsExpr GhcTc -> Bool
isDotEq (L _ (HsVar _ (L _ n))) = occNameString (getOccName n) == ".="
isDotEq (L _ (HsRecSel _ (FieldOcc _ (L _ n)))) = occNameString (rdrNameOcc n) == ".="
isDotEq _ = False

extractKey :: LHsExpr GhcTc -> Maybe String
extractKey (L _ (HsLit _ (HsString _ fs))) = Just (unpackFS fs)
extractKey _ = Nothing

splitCondLevels :: LHsExpr GhcTc -> Maybe (Set.Set DetailLevel, Set.Set DetailLevel)
splitCondLevels (L _ (OpApp _ lhs op rhs))
  | isCmp op, Just lvl <- conLevel rhs, Just var <- exprVarName lhs, var == "dtal" =
      let cmp = cmpOp op in
      case cmp of
        Just "<"  -> Just (levelsLt lvl, levelsGe lvl)
        Just "<=" -> Just (levelsLe lvl, levelsGt lvl)
        Just ">"  -> Just (levelsGt lvl, levelsLe lvl)
        Just ">=" -> Just (levelsGe lvl, levelsLt lvl)
        Just "==" -> Just (Set.singleton lvl, Set.delete lvl allLevels)
        _         -> Nothing
  | otherwise = Nothing
  where
    isCmp (L _ (HsVar _ (L _ n))) = occNameString (getOccName n) `elem` ["<", "<=", ">", ">=", "=="]
    isCmp _ = False
    cmpOp (L _ (HsVar _ (L _ n))) = Just (occNameString (getOccName n))
    cmpOp _ = Nothing
splitCondLevels _ = Nothing

exprVarName :: LHsExpr GhcTc -> Maybe String
exprVarName (L _ (HsVar _ (L _ n))) = Just (occNameString (getOccName n))
exprVarName _ = Nothing

conLevel :: LHsExpr GhcTc -> Maybe DetailLevel
conLevel (L _ (HsVar _ (L _ n))) = levelFromCon (occNameString (getOccName n))
conLevel _ = Nothing

levelsGe, levelsGt, levelsLe, levelsLt :: DetailLevel -> Set.Set DetailLevel
levelsGe lvl = Set.fromList [l | l <- [Minimal, Normal, Detailed, Maximum], l >= lvl]
levelsGt lvl = Set.fromList [l | l <- [Minimal, Normal, Detailed, Maximum], l > lvl]
levelsLe lvl = Set.fromList [l | l <- [Minimal, Normal, Detailed, Maximum], l <= lvl]
levelsLt lvl = Set.fromList [l | l <- [Minimal, Normal, Detailed, Maximum], l < lvl]

scrutLevel :: LHsExpr GhcTc -> Maybe [DetailLevel]
scrutLevel (L _ (HsVar _ (L _ n)))
  | occNameString (getOccName n) == "dtal" = Just [Minimal, Normal, Detailed, Maximum]
scrutLevel _ = Nothing

--------------------------------------------------------------------------------
-- Schema generation
--------------------------------------------------------------------------------

generateSchemas :: FilePath
                -> FilePath
                -> Map.Map ConstructorName [NamespaceParts]
                -> FieldMap
                -> [String]
                -> IO ()
generateSchemas msgOutDir typeOutDir nsMap fmMap namespaces = do
  cache <- newIORef Set.empty
  forM_ namespaces $ \ns -> do
    let parts = splitOnDot ns
    let ctor = findCtor nsMap parts
    let fieldsByLevel = maybe emptyLevels (\c -> Map.findWithDefault emptyLevels c fmMap) ctor
    let hasVariants = any (not . Map.null) (Map.elems fieldsByLevel)
    schema <- if hasVariants
      then do
        variants <- mapM (variantEntry typeOutDir cache fieldsByLevel) [Minimal, Normal, Detailed, Maximum]
        pure $ A.object [ "ns" A..= ns, "variants" A..= variants ]
      else pure $ A.object [ "ns" A..= ns, "data" A..= (A.object []) ]
    let out = msgOutDir </> foldr (</>) (last parts ++ ".schema.json") (init parts)
    createDirectoryIfMissing True (takeDirectory out)
    BL.writeFile out (A.encode schema)

variantEntry :: FilePath
             -> IORef (Set.Set String)
             -> Map.Map DetailLevel (Map.Map String Type)
             -> DetailLevel
             -> IO A.Value
variantEntry typeOutDir cache m lvl = do
  let fields = Map.findWithDefault Map.empty lvl m
  props <- Map.traverseWithKey (\_ t -> typeToSchemaRef typeOutDir cache t) fields
  let dataSchema = A.object
        [ "type" A..= ("object" :: String)
        , "properties" A..= props
        , "additionalProperties" A..= True
        ]
  pure $ A.object [ "detailLevel" A..= detailName lvl, "data" A..= dataSchema ]

findCtor :: Map.Map ConstructorName [NamespaceParts] -> [String] -> Maybe ConstructorName
findCtor nsMap parts =
  let matches = [ ctor
                | (ctor, nss) <- Map.toList nsMap
                , any (\nsParts -> nsParts `isSuffixOf` parts) nss
                ]
  in case matches of
      (x:_) -> Just x
      []    -> Nothing

splitOnDot :: String -> [String]
splitOnDot s = case break (=='.') s of
  (a, []) -> [a]
  (a, _:b) -> a : splitOnDot b

emptyLevels :: Map.Map DetailLevel (Map.Map String Type)
emptyLevels = Map.fromList [(lvl, Map.empty) | lvl <- [Minimal, Normal, Detailed, Maximum]]

--------------------------------------------------------------------------------
-- Type to schema (best-effort structural)
--------------------------------------------------------------------------------

typeToSchemaRef :: FilePath -> IORef (Set.Set String) -> Type -> IO A.Value
typeToSchemaRef _ _ ty | isStringTy ty = pure (A.object ["type" A..= ("string" :: String)])
typeToSchemaRef _ _ ty | isBoolTy ty   = pure (A.object ["type" A..= ("boolean" :: String)])
typeToSchemaRef _ _ ty | isIntTy ty    = pure (A.object ["type" A..= ("integer" :: String)])
typeToSchemaRef _ _ ty | isFloatTy ty  = pure (A.object ["type" A..= ("number" :: String)])
typeToSchemaRef outDir cache ty
  | isListTy ty, Just elemTy <- listElemType ty = do
      itemSchema <- typeToSchemaRef outDir cache elemTy
      pure $ A.object ["type" A..= ("array" :: String), "items" A..= itemSchema]
  | isMaybeTy ty, Just elemTy <- maybeElemType ty = do
      itemSchema <- typeToSchemaRef outDir cache elemTy
      pure $ A.object ["anyOf" A..= [A.object ["type" A..= ("null" :: String)], itemSchema]]
  | otherwise =
      case splitTyConApp_maybe ty of
        Just (tc, args) -> do
          let name = occNameString (getOccName (tyConName tc))
          ensureTypeSchema outDir cache tc args
          pure $ A.object ["$ref" A..= ("types/" ++ name ++ ".schema.json")]
        Nothing -> pure (A.object [])

ensureTypeSchema :: FilePath -> IORef (Set.Set String) -> TyCon -> [Type] -> IO ()
ensureTypeSchema outDir cache tc args = do
  let name = occNameString (getOccName (tyConName tc))
  seen <- readIORef cache
  when (Set.notMember name seen) $ do
    modifyIORef' cache (Set.insert name)
    schema <- schemaForTyCon outDir cache tc args
    let out = outDir </> (name ++ ".schema.json")
    BL.writeFile out (A.encode schema)

schemaForTyCon :: FilePath -> IORef (Set.Set String) -> TyCon -> [Type] -> IO A.Value
schemaForTyCon outDir cache tc _args = do
  let dcs = tyConDataCons tc
  case dcs of
    [] -> pure (A.object [])
    [dc] -> schemaForDataCon outDir cache dc
    _ -> do
      alts <- mapM (schemaForDataCon outDir cache) dcs
      pure $ A.object ["oneOf" A..= alts]

schemaForDataCon :: FilePath -> IORef (Set.Set String) -> DataCon -> IO A.Value
schemaForDataCon outDir cache dc = do
  let ctorName = occNameString (getOccName (dataConName dc))
  let labels = dataConFieldLabels dc
  let argTys = map (\(Scaled _ t) -> t) (dataConOrigArgTys dc)
  if not (null labels)
    then do
      props <- Map.fromList <$> forM (zip labels argTys) (\(lbl, ty) -> do
        v <- typeToSchemaRef outDir cache ty
        pure (fieldLabelName lbl, v))
      pure $ A.object
        [ "type" A..= ("object" :: String)
        , "properties" A..= props
        , "required" A..= Map.keys props
        ]
    else if null argTys
      then pure $ A.object
        [ "type" A..= ("object" :: String)
        , "properties" A..= A.object [ "tag" A..= A.object ["const" A..= ctorName] ]
        , "required" A..= ["tag" :: String]
        ]
      else do
        content <- case argTys of
          [ty] -> typeToSchemaRef outDir cache ty
          tys -> do
            items <- mapM (typeToSchemaRef outDir cache) tys
            pure $ A.object ["type" A..= ("array" :: String), "items" A..= items]
        pure $ A.object
          [ "type" A..= ("object" :: String)
          , "properties" A..= A.object
              [ "tag" A..= A.object ["const" A..= ctorName]
              , "contents" A..= content
              ]
          , "required" A..= ["tag" :: String, "contents"]
          ]

fieldLabelName :: FieldLabel -> String
fieldLabelName = unpackFS . field_label . flLabel

isStringTy :: Type -> Bool
isStringTy ty = any (tyConNameEq ty) ["Text","String","ByteString","ShortByteString","ShortText"]

isBoolTy :: Type -> Bool
isBoolTy ty = tyConNameEq ty "Bool"

isIntTy :: Type -> Bool
isIntTy ty = any (tyConNameEq ty) ["Int","Integer","Word","Word8","Word16","Word32","Word64","Natural","SlotNo","EpochNo","BlockNo"]

isFloatTy :: Type -> Bool
isFloatTy ty = any (tyConNameEq ty) ["Double","Float","NominalDiffTime","DiffTime"]

isListTy :: Type -> Bool
isListTy ty = case splitTyConApp_maybe ty of
  Just (tc, _) -> occNameString (getOccName (tyConName tc)) == "[]"
  _ -> False

listElemType :: Type -> Maybe Type
listElemType ty = case splitTyConApp_maybe ty of
  Just (tc, [t]) | occNameString (getOccName (tyConName tc)) == "[]" -> Just t
  _ -> Nothing

isMaybeTy :: Type -> Bool
isMaybeTy ty = case splitTyConApp_maybe ty of
  Just (tc, _) -> occNameString (getOccName (tyConName tc)) == "Maybe"
  _ -> False

maybeElemType :: Type -> Maybe Type
maybeElemType ty = case splitTyConApp_maybe ty of
  Just (tc, [t]) | occNameString (getOccName (tyConName tc)) == "Maybe" -> Just t
  _ -> Nothing

tyConNameEq :: Type -> String -> Bool
tyConNameEq ty name = case splitTyConApp_maybe ty of
  Just (tc, _) -> occNameString (getOccName (tyConName tc)) == name
  _ -> False
