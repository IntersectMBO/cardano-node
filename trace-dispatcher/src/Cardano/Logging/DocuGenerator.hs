{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use map" -}

module Cardano.Logging.DocuGenerator (
  -- First call documentTracer for every tracer and then
  -- docuResultToText on all results
    documentTracer
  , documentTracer'
  , docuResultsToText
  -- Callbacks
  , docTracer
  , docTracerDatapoint
  , docIt
  , addFiltered
  , addLimiter
  , addSilent
  , addDocumentedNamespace

  , DocuResult
  , DocTracer(..)
) where

import           Cardano.Logging.ConfigurationParser ()
import           Cardano.Logging.Types

import           Prelude hiding (lines, unlines)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as TR
import qualified Data.Aeson.Encode.Pretty as AE
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.List (groupBy, intersperse, nub, sortBy)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Text (Text, lines, pack, split, toLower, unlines)
import           Data.Text.Internal.Builder (toLazyText)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromString, fromText, singleton)
import           Data.Time (getZonedTime)

import           Trace.Forward.Utils.DataPoint (DataPoint (..))

type InconsistencyWarning = Text

utf16CircledT :: Text
utf16CircledT = "\x24E3"

utf16CircledS :: Text
utf16CircledS = "\x24E2"

utf16CircledM :: Text
utf16CircledM = "\x24DC"

-- | Convenience function for adding a namespace prefix to a documented
addDocumentedNamespace  :: [Text] -> Documented a -> Documented a
addDocumentedNamespace  out (Documented list) =
  Documented $ map
    (\ dm@DocMsg {} -> dm {dmNamespace = nsReplacePrefix out (dmNamespace dm)})
    list

data DocuResult =
  DocuTracer Builder
  | DocuMetric Builder
  | DocuDatapoint Builder
  deriving (Show)

data DocTracer = DocTracer {
      dtTracerNames :: [[Text]]
    , dtSilent      :: [[Text]]
    , dtNoMetrics   :: [[Text]]
    , dtBuilderList :: [([Text], DocuResult)]
    , dtWarnings    :: [InconsistencyWarning]
} deriving (Show)

instance Semigroup DocTracer where
  dtl <> dtr = DocTracer
                 (dtTracerNames dtl <> dtTracerNames dtr)
                 (dtSilent dtl <> dtSilent dtr)
                 (dtNoMetrics dtl <> dtNoMetrics dtr)
                 (dtBuilderList dtl <> dtBuilderList dtr)
                 (dtWarnings dtl <> dtWarnings dtr)

isTracer :: DocuResult -> Bool
isTracer DocuTracer {} = True
isTracer _             = False

isMetric :: DocuResult -> Bool
isMetric DocuMetric {} = True
isMetric _             = False

isDatapoint :: DocuResult -> Bool
isDatapoint DocuDatapoint {} = True
isDatapoint _                = False

unpackDocu :: DocuResult -> Builder
unpackDocu (DocuTracer b)    = b
unpackDocu (DocuMetric b)    = b
unpackDocu (DocuDatapoint b) = b

documentTracer' :: forall a a1.
     MetaTrace a
  => (Trace IO a1 -> IO (Trace IO a))
  -> Trace IO a1
  -> IO DocTracer
documentTracer' hook tracer = do
    tr' <- hook tracer
    documentTracer tr'

-- This fuction calls document tracers and returns a DocTracer result
documentTracer :: forall a.
     MetaTrace a
  => Trace IO a
  -> IO DocTracer
documentTracer tracer = do
    DocCollector docRef <- documentTracersRun [tracer]
    items <- fmap Map.toList (liftIO (readIORef docRef))
    let sortedItems = sortBy
                        (\ (_,l) (_,r) -> compare (ldNamespace l) (ldNamespace r))
                        items
    let messageDocs = map (\(i, ld) -> case ldNamespace ld of
                                        (prn,pon) : _  -> (prn ++ pon, documentItem (i, ld))
                                        []             -> (["No ns"], documentItem (i, ld))) sortedItems
        metricsItems = map snd $ filter (not . Map.null . ldMetricsDoc . snd) sortedItems
        metricsDocs = documentMetrics metricsItems
        tracerName = case sortedItems of
                      ((_i, ld) : _) -> case ldNamespace ld of
                                          (prn, _pon) : _  -> prn
                                          []               -> []
                      []             -> []
        silent = case sortedItems of
                      ((_i, ld) : _) -> ldSilent ld
                      [] -> False
        hasNoMetrics = null metricsItems
        warnings = concatMap (\(i, ld) -> case ldNamespace ld of
                                            (_,_): _       -> warningItem (i, ld)
                                            []             -> (pack "No ns for " <> ldDoc ld) :
                                              warningItem (i, ld)) sortedItems
    pure $ DocTracer
            [tracerName]
            [tracerName | silent]
            [tracerName | hasNoMetrics]
            (messageDocs ++ metricsDocs)
            warnings

  where
    documentItem :: (Int, LogDoc) -> DocuResult
    documentItem (_idx, ld@LogDoc {..}) =
      case ldBackends of
        [DatapointBackend] -> DocuDatapoint $
                    mconcat $ intersperse (fromText "\n\n")
                      [ namespacesBuilder (nub ldNamespace)
                      , accentuated ldDoc
                      ]
        _ -> DocuTracer $
                    mconcat $ intersperse (fromText "\n\n")
                      [ namespacesBuilder (nub ldNamespace)
                      , accentuated ldDoc
                      , propertiesBuilder ld
                      , configBuilder ld
                      ]

    warningItem :: (Int, LogDoc) -> [InconsistencyWarning]
    warningItem (_idx, ld@LogDoc {..}) =
      case ldBackends of
        [DatapointBackend] -> namespacesWarning (nub ldNamespace) ld
        _ -> namespacesWarning (nub ldNamespace) ld
                ++ propertiesWarning ld

    documentMetrics :: [LogDoc] -> [([Text],DocuResult)]
    documentMetrics logDocs =
      let nameCommentNamespaceList =
            concatMap (\ld -> zip (Map.toList (ldMetricsDoc ld)) (repeat (ldNamespace ld))) logDocs
          sortedNameCommentNamespaceList =
            sortBy (\a b -> compare ((fst . fst) a) ((fst . fst) b)) nameCommentNamespaceList
          groupedNameCommentNamespaceList =
            groupBy (\a b -> (fst . fst) a == (fst . fst) b) sortedNameCommentNamespaceList
      in mapMaybe documentMetrics' groupedNameCommentNamespaceList

    documentMetrics' :: [( (Text, Text) , [([Text],[Text])] )] -> Maybe ([Text], DocuResult)
    documentMetrics' ncns@(((name, comment), _) : _tail) =
      Just ([name], DocuMetric
              $ mconcat $ intersperse(fromText "\n\n")
                    [ metricToBuilder (name,comment)
                    , namespacesMetricsBuilder (nub (concatMap snd ncns))
                    ])
    documentMetrics' [] = Nothing

    namespacesBuilder :: [([Text], [Text])] -> Builder
    namespacesBuilder [ns] = namespaceBuilder ns
    namespacesBuilder []   = fromText "__Warning__: namespace missing"
    namespacesBuilder nsl  =
      mconcat (intersperse (singleton '\n')(map namespaceBuilder nsl))

    namespaceBuilder :: ([Text], [Text]) -> Builder
    namespaceBuilder (nsPr, nsPo) = fromText "### " <>
      mconcat (intersperse (singleton '.') (map fromText (nsPr ++ nsPo)))

    namespacesMetricsBuilder :: [ ([Text], [Text])] -> Builder
    namespacesMetricsBuilder [ns] = fromText "Dispatched by: \n" <> namespaceMetricsBuilder ns
    namespacesMetricsBuilder []   = mempty
    namespacesMetricsBuilder nsl  = fromText "Dispatched by: \n" <>
      mconcat (intersperse (singleton '\n')(map namespaceMetricsBuilder nsl))

    namespaceMetricsBuilder :: ([Text], [Text]) -> Builder
    namespaceMetricsBuilder (nsPr, nsPo) = mconcat (intersperse (singleton '.')
                                                      (map fromText (nsPr ++ nsPo)))

    namespacesWarning :: [([Text], [Text])] -> LogDoc -> [InconsistencyWarning]
    namespacesWarning [] ld  = ["Namespace missing " <> ldDoc ld]
    namespacesWarning _ _  = []

    propertiesBuilder :: LogDoc -> Builder
    propertiesBuilder LogDoc {..} =
        case ldSeverityCoded of
          Just s  -> fromText "Severity:  " <> asCode (fromString (show s)) <> "\n"
          Nothing -> fromText "Severity missing" <> "\n"
      <>
        case ldPrivacyCoded of
          Just p  -> fromText "Privacy:   " <> asCode (fromString (show p)) <> "\n"
          Nothing -> fromText "Privacy missing" <> "\n"
      <>
        case ldDetailsCoded of
          Just d  -> fromText "Details:   " <> asCode (fromString (show d)) <> "\n"
          Nothing -> fromText "Details missing" <> "\n"

    propertiesWarning :: LogDoc ->[InconsistencyWarning]
    propertiesWarning LogDoc {..} =
        case ldSeverityCoded of
          Just _s -> []
          Nothing -> map (\ns -> pack "Severity missing" <> nsRawToText ns) ldNamespace
      <>
        case ldPrivacyCoded of
          Just _p -> []
          Nothing -> map (\ns -> pack "Privacy missing" <> nsRawToText ns) ldNamespace
      <>
        case ldDetailsCoded of
          Just _d -> []
          Nothing -> map (\ns -> pack "Details missing" <> nsRawToText ns) ldNamespace

    configBuilder :: LogDoc -> Builder
    configBuilder LogDoc {..} =
      fromText "From current configuration:\n"
      <> case nub ldDetails of
          []  -> mempty
          [d] -> if Just d /= ldDetailsCoded
                    then fromText "Details:   "
                            <> asCode (fromString (show d))
                    else mempty
          l   -> fromText "Details:   "
                  <> mconcat (intersperse (fromText ",\n      ")
                               (map (asCode . fromString . show) l))
      <> fromText "\n"
      <> backendsBuilder (nub ldBackends)
      <> fromText "\n"
      <> filteredBuilder (nub ldFiltered) ldSeverityCoded
      <> limiterBuilder (nub ldLimiter)

    backendsBuilder :: [BackendConfig] -> Builder
    backendsBuilder [] = fromText "No backends found"
    backendsBuilder l  = fromText "Backends:\n      "
                          <> mconcat (intersperse (fromText ",\n      ")
                                (map backendFormatToText l))

    backendFormatToText :: BackendConfig -> Builder
    backendFormatToText be = asCode (fromString (show be))

    filteredBuilder :: [SeverityF] -> Maybe SeverityS -> Builder
    filteredBuilder [] _ = mempty
    filteredBuilder _ Nothing = mempty
    filteredBuilder l (Just r) =
      fromText "Filtered "
      <> case l of
            [SeverityF (Just lh)] ->
              if fromEnum r >= fromEnum lh
                then (asCode . fromString) "Visible"
                else (asCode . fromString) "Invisible"
            [SeverityF Nothing] -> "Invisible"
            _ -> mempty
      <> fromText " by config value: "
      <> mconcat (intersperse (fromText ", ")
          (map (asCode . fromString . show) l))

    limiterBuilder ::
         [(Text, Double)]
      -> Builder
    limiterBuilder [] = mempty
    limiterBuilder l  =
      mconcat (intersperse (fromText ", ")
        (map (\ (n, d) ->  fromText "\nLimiter "
                        <> (asCode . fromText) n
                        <> fromText " with frequency "
                        <> (asCode . fromString. show) d)
              l))

    metricToBuilder :: (Text, Text) -> Builder
    metricToBuilder (name, text) =
        fromText "### "
          <> fromText name
            <> fromText "\n"
              <> accentuated text



-- | Calls the tracers in a documetation control mode,
-- and returns a DocCollector, from which the documentation gets generated
documentTracersRun :: forall a. MetaTrace a => [Trace IO a] -> IO DocCollector
documentTracersRun tracers = do
    let nss = allNamespaces :: [Namespace a]
        nsIdx = zip nss [0..]
    coll <- fmap DocCollector (liftIO $ newIORef (Map.empty :: Map.Map Int LogDoc))
    mapM_ (docTrace nsIdx coll) tracers
    pure coll
  where
    docTrace nsIdx dc@(DocCollector docRef) (Trace tr) =
      mapM_
        (\ (ns, idx) -> do
            let condDoc = documentFor ns
                doc = fromMaybe mempty condDoc

            modifyIORef docRef
                        (Map.insert
                          idx
                          ((emptyLogDoc
                              doc
                              (metricsDocFor ns))
                            { ldSeverityCoded = severityFor ns Nothing
                            , ldPrivacyCoded  = privacyFor ns Nothing
                            , ldDetailsCoded  = detailsFor ns Nothing
                          }))
            TR.traceWith tr (emptyLoggingContext {lcNSInner = nsInner ns},
                            Left (TCDocument idx dc)))
        nsIdx

-------------------- Callbacks ---------------------------

docTracer :: MonadIO m =>
     BackendConfig
  -> Trace m FormattedMessage
docTracer backendConfig = Trace $ TR.arrow $ TR.emit output
  where
    output p@(_, Left TCDocument {}) =
      docIt backendConfig p
    output (_, _) = pure ()

docTracerDatapoint :: MonadIO m =>
     BackendConfig
  -> Trace m DataPoint
docTracerDatapoint backendConfig = Trace $ TR.arrow $ TR.emit output
  where
    output p@(_, Left TCDocument {}) =
      docItDatapoint backendConfig p
    output (_, _) = pure ()


-- | Callback for doc collection
addFiltered :: MonadIO m => TraceControl -> Maybe SeverityF -> m ()
addFiltered (TCDocument idx (DocCollector docRef)) (Just sev) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldFiltered = seq sev (sev : ldFiltered e)})
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> error "DocuGenerator>>missing log doc"))
        docMap)
addFiltered _ _ = pure ()

-- | Callback for doc collection
addLimiter :: MonadIO m => TraceControl -> (Text, Double) -> m ()
addLimiter (TCDocument idx (DocCollector docRef)) (ln, lf) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldLimiter = seq ln (seq lf ((ln, lf) : ldLimiter e))})
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> error "DocuGenerator>>missing log doc"))
        docMap)
addLimiter _ _ = pure ()

addSilent :: MonadIO m => TraceControl -> Maybe Bool -> m ()
addSilent (TCDocument idx (DocCollector docRef)) (Just silent) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldSilent = silent})
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> error "DocuGenerator>>missing log doc"))
        docMap)
addSilent _ _ = pure ()

-- | Callback for doc collection
docIt :: MonadIO m
  => BackendConfig
  -> (LoggingContext, Either TraceControl a)
  -> m ()
docIt EKGBackend (LoggingContext{},
  Left (TCDocument idx (DocCollector docRef))) = do
    liftIO $ modifyIORef docRef (\ docMap ->
        Map.insert
          idx
          ((\e -> e { ldBackends  = EKGBackend : ldBackends e
                    })
            (case Map.lookup idx docMap of
                          Just e  -> e
                          Nothing -> error "DocuGenerator>>missing log doc"))
          docMap)
docIt backend (LoggingContext {..},
  Left (TCDocument idx (DocCollector docRef))) = do
    liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldBackends  = backend : ldBackends e
                  , ldNamespace = nub ((lcNSPrefix,lcNSInner) : ldNamespace e)
                  , ldDetails   = case lcDetails of
                                    Nothing -> ldDetails e
                                    Just d  -> d : ldDetails e
                  })
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> error "DocuGenerator>>missing log doc"))
        docMap)
docIt _ (_, _) = pure ()

-- | Callback for doc collection
docItDatapoint :: MonadIO m =>
     BackendConfig
  -> (LoggingContext, Either TraceControl a)
  -> m ()
docItDatapoint _backend (LoggingContext {..},
  Left (TCDocument idx (DocCollector docRef))) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldNamespace = nub ((lcNSPrefix, lcNSInner) : ldNamespace e)
                  , ldBackends  = [DatapointBackend]
                  })
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> error "DocuGenerator>>missing log doc"))
        docMap)
docItDatapoint _backend (LoggingContext {}, _) = pure ()


-- Finally generate a text from all the builders
docuResultsToText :: DocTracer -> TraceConfig -> IO Text
docuResultsToText dt@DocTracer {..} configuration = do
  time <- getZonedTime
  let traceBuilders = sortBy (\ (l,_) (r,_) -> compare l r)
                          (filter (isTracer . snd) dtBuilderList)
      metricsBuilders = sortBy (\ (l,_) (r,_) -> compare l r)
                          (filter (isMetric .snd) dtBuilderList)
      datapointBuilders = sortBy (\ (l,_) (r,_) -> compare l r)
                          (filter (isDatapoint . snd) dtBuilderList)
      header  = fromText "# Cardano Trace Documentation\n\n"
      header1  = fromText "## Table Of Contents\n\n"
      toc      = generateTOC dt
                    (map fst traceBuilders)
                    (map fst metricsBuilders)
                    (map fst datapointBuilders)

      header2  = fromText "\n## Trace Messages\n\n"
      contentT = mconcat $ intersperse (fromText "\n\n")
                              (map (unpackDocu . snd) traceBuilders)
      header3  = fromText "\n## Metrics\n\n"
      contentM = mconcat $ intersperse (fromText "\n\n")
                              (map (unpackDocu . snd) metricsBuilders)
      header4  = fromText "\n## Datapoints\n\n"
      contentD = mconcat $ intersperse (fromText "\n\n")
                              (map (unpackDocu . snd) datapointBuilders)
      config  = fromText "\n## Configuration: \n```\n"
                        <> AE.encodePrettyToTextBuilder configuration
                        <> fromText "\n```\n"
      numbers = fromString $  show (length traceBuilders) <> " log messages, " <> "\n" <>
                              show (length metricsBuilders) <> " metrics," <> "\n" <>
                              show (length datapointBuilders) <> " datapoints." <> "\n\n"

      legend  = fromText $ utf16CircledT <> "- This is the root of a tracer\n\n" <>
                           utf16CircledS <> "- This is the root of a tracer that is silent because of the current configuration\n\n" <>
                           utf16CircledM <> "- This is the root of a tracer, that provides metrics\n\n"
      ts      = fromString $ "Generated at " <> show time <> ".\n"
  pure $ toStrict $ toLazyText (
         header
      <> header1
      <> toc
      <> header2
      <> contentT
      <> header3
      <> contentM
      <> header4
      <> contentD
      <> config
      <> numbers
      <> legend
      <> ts)


generateTOC :: DocTracer -> [[Text]] -> [[Text]] -> [[Text]] -> Builder
generateTOC dt traces metrics datapoints =
       generateTOCTraces
    <> generateTOCMetrics
    <> generateTOCDatapoints
    <> generateTOCRest
  where
    generateTOCTraces =
      fromText "### [Trace Messages](#trace-messages)\n\n"
      <> mconcat (reverse (fst (foldl (namespaceToToc (Just dt)) ([], []) traces)))
      <> fromText "\n"
    generateTOCMetrics =
      fromText "### [Metrics](#metrics)\n\n"
      <> mconcat (reverse (fst (foldl (namespaceToToc Nothing) ([], []) (map splitToNS metrics))))
      <> fromText "\n"
    generateTOCDatapoints =
      fromText "### [Datapoints](#datapoints)\n\n"
      <> mconcat (reverse (fst (foldl (namespaceToToc Nothing) ([], []) datapoints)))
      <> fromText "\n"
    generateTOCRest =
         fromText "### [Configuration](#configuration)\n\n"
      <> fromText "\n"


    namespaceToToc :: Maybe DocTracer -> ([Builder], [Text]) -> [Text]-> ([Builder], [Text])
    namespaceToToc condDocTracer (builders, context) ns =
      let ref = namespaceRefBuilder ns
      in case ns of
        (hd:tl) -> if init (hd:tl) == context
                    then
                      let symbolsText = case condDocTracer of
                                          Nothing -> ""
                                          Just docTracers -> getSymbolsOf ns docTracers
                      in ( fromString (concat (replicate (length context) "    "))
                          <> fromText "1. "
                          <> fromText "["
                          <> fromText (last ns)
                          <> fromText symbolsText
                          <> fromText "](#"
                          <> ref
                          <> fromText ")\n" : builders, context)
                    else
                      let cpl  = commonPrefixLength context ns
                          ns' = drop cpl ns
                          context' = take cpl context
                      in namespaceToTocWithContext condDocTracer (builders, context') ns' ns ref
        [] -> ([],[])
    namespaceToTocWithContext ::
         Maybe DocTracer
      -> ([Builder], [Text])
      -> [Text]
      -> [Text]
      -> Builder
      -> ([Builder], [Text])
    namespaceToTocWithContext condDocTracer (builders, context) ns nsFull ref =
      case ns of
        [single] -> let symbolsText = case condDocTracer of
                              Nothing -> ""
                              Just docTracers -> getSymbolsOf (context ++ [single]) docTracers
                    in ((fromString (concat (replicate (length context) "    "))
                                <> fromText "1. "
                                <> fromText "["
                                <> fromText single
                                <> fromText symbolsText
                                <> fromText "](#"
                                <> ref
                                <> fromText ")\n") : builders, context)
        (hdn : tln) ->
          let symbolsText = case condDocTracer of
                              Nothing -> ""
                              Just docTracers -> getSymbolsOf (context ++ [hdn]) docTracers
              builder = fromString (concat (replicate (length context) "    "))
                        <> fromText "1. __"
                        <> fromText hdn
                        <> fromText symbolsText
                        <> fromText "__\n"
          in  namespaceToTocWithContext condDocTracer
                (builder : builders, context ++ [hdn]) tln nsFull ref
        [] -> error "inpossible"

    splitToNS :: [Text] -> [Text]
    splitToNS [sym] = split (== '.') sym
    splitToNS other = other

    getSymbolsOf :: [Text] -> DocTracer -> Text
    getSymbolsOf ns DocTracer {..} =
      let isTracer' = elem ns dtTracerNames
      in  if isTracer'
            then
              let isSilent  = elem ns dtSilent
                  noMetrics = elem ns dtNoMetrics
              in utf16CircledT <> if isSilent then utf16CircledS else ""
                      <> if noMetrics then "" else utf16CircledM
            else ""

    commonPrefixLength :: Eq a => [a] -> [a] -> Int
    commonPrefixLength [] _ = 0
    commonPrefixLength _ [] = 0
    commonPrefixLength (a : ta) (b : tb) =
      if a == b
          then 1 + commonPrefixLength ta tb
          else 0

    namespaceRefBuilder ns = mconcat (map (fromText . toLower ) ns)

asCode :: Builder -> Builder
asCode b = singleton '`' <> b <> singleton '`'

accentuated :: Text -> Builder
accentuated t = if t == ""
                  then fromText "\n"
                  else fromText "\n"
                        <> fromText (unlines $ map addAccent (lines t))
  where
    addAccent :: Text -> Text
    addAccent t' = if t' == ""
                    then ">"
                    else "> " <> t'


