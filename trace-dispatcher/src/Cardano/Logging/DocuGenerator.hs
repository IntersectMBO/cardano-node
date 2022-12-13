{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.DocuGenerator (
  -- First call documentTracer for every tracer and then
  -- docuResultToText on all results
    documentTracer
  , docuResultsToText
  -- Callbacks
  , docTracer
  , docTracerDatapoint
  , docIt
  , addFiltered
  , addLimiter
  -- Convenience functions
  , showT
  , addDocumentedNamespace

  , DocuResult
) where

import           Cardano.Logging.Types
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.List (groupBy, intersperse, nub, sortBy)
import qualified Data.Map as Map
import           Data.Text (Text, pack, toLower)
import qualified Data.Text as T
import           Data.Text.Internal.Builder (toLazyText)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromString, fromText, singleton)
import           Data.Time (getZonedTime)
import           Trace.Forward.Utils.DataPoint (DataPoint (..))

import Debug.Trace

-- | Convenience function for adding a namespace prefix to a documented
addDocumentedNamespace  :: [Text] -> Documented a -> Documented a
addDocumentedNamespace  tl (Documented list) =
  Documented $ map
    (\ dm@DocMsg {} -> dm {dmNamespace = nsReplaceOuter (dmNamespace dm) tl})
    list

-- | Convenience function
showT :: Show a => a -> Text
showT = pack . show

-- use <> instead, as it is a semigroup

-- | Convenience function for concatenating to the list in documented
-- addDocs :: Documented a -> Documented a -> Documented a
-- addDocs (Documented l) (Documented r) = Documented (l ++ r)

data DocuResult =
  DocuTracer Builder
  | DocuMetric Builder
  | DocuDatapoint Builder

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

-- This fuction calls document tracers and returns a list with namespaces
-- and an associated DocuResult
documentTracer :: forall a.
     MetaTrace a
  => [Trace IO a]
  -> IO [([Text], DocuResult)]
documentTracer tracers = do
    DocCollector docRef <- documentTracersRun tracers
    items <- fmap Map.toList (liftIO (readIORef docRef))
    let sortedItems = trace (show items) $ sortBy
                        (\ (_,l) (_,r) -> compare (ldNamespace l) (ldNamespace r))
                        items
    let messageDocs = map (\(i, ld) -> case ldNamespace ld of
                                []     -> (["No NamespaceOuter"], documentItem (i, ld))
                                (hn:_) -> (hn, documentItem (i, ld))) sortedItems
        metricsItems = map snd $ filter (not . Map.null . ldMetricsDoc . snd) sortedItems
        metricsDocs = documentMetrics metricsItems
    pure $ messageDocs ++ metricsDocs
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
                      , configBuilder ld
                      ]

    documentMetrics :: [LogDoc] -> [([Text], DocuResult)]
    documentMetrics logDocs =
      let nameCommentNamespaceList =
            concatMap (\ld -> zip (Map.toList (ldMetricsDoc ld)) (repeat (ldNamespace ld))) logDocs
          sortedNameCommentNamespaceList =
            sortBy (\a b -> compare ((fst . fst) a) ((fst . fst) b)) nameCommentNamespaceList
          groupedNameCommentNamespaceList =
            groupBy (\a b -> (fst . fst) a == (fst . fst) b) sortedNameCommentNamespaceList
      in map documentMetrics' groupedNameCommentNamespaceList

    documentMetrics' :: [((Text, Text), [[Text]])] -> ([Text], DocuResult)
    documentMetrics' ncns@(((name, comment), _) : _tail) =
      ([name], DocuMetric
              $ mconcat $ intersperse(fromText "\n\n")
                    [ metricToBuilder (name,comment)
                    , namespacesMetricsBuilder (nub (concatMap snd ncns))
                    ])

    namespacesBuilder :: [[Text]] -> Builder
    namespacesBuilder [ns] = namespaceBuilder ns
    namespacesBuilder []   = fromText "__Warning__: NamespaceOuter missing"
    namespacesBuilder nsl  =
      mconcat (intersperse (singleton '\n')(map namespaceBuilder nsl))

    namespaceBuilder :: [Text] -> Builder
    namespaceBuilder ns = fromText "### " <>
      mconcat (intersperse (singleton '.') (map fromText ns))

    namespacesMetricsBuilder :: [[Text]] -> Builder
    namespacesMetricsBuilder [ns] = fromText "Dispatched by: \n" <> namespaceMetricsBuilder ns
    namespacesMetricsBuilder []   = mempty
    namespacesMetricsBuilder nsl  = fromText "Dispatched by: \n" <>
      mconcat (intersperse (singleton '\n')(map namespaceMetricsBuilder nsl))

    namespaceMetricsBuilder :: [Text] -> Builder
    namespaceMetricsBuilder ns = mconcat (intersperse (singleton '.') (map fromText ns))


    configBuilder :: LogDoc -> Builder
    configBuilder LogDoc {..} =
      fromText "From current configuration:\n"
      <> case nub ldDetails of
          []  -> fromText "Details:   " <> asCode (fromString (show DNormal))
          [d] -> fromText "Details:   " <> asCode (fromString (show d))
          l   -> fromText "Details:   "
                  <> mconcat (intersperse (singleton ',')
                        (map (asCode . fromString . show) l))
      <> fromText "\n"
      <> backendsBuilder (nub ldBackends)
      <> fromText "\n"
      <> filteredBuilder (nub ldFiltered) (nub ldSeverity)
      <> limiterBuilder (nub ldLimiter)

    backendsBuilder :: [BackendConfig] -> Builder
    backendsBuilder [] = fromText "No backends found"
    backendsBuilder l  = fromText "Backends:\n      "
                          <> mconcat (intersperse (fromText ",\n      ")
                                (map backendFormatToText l))

    backendFormatToText :: BackendConfig -> Builder
    backendFormatToText be = asCode (fromString (show be))

    filteredBuilder :: [SeverityF] -> [SeverityS] -> Builder
    filteredBuilder [] _ = mempty
    filteredBuilder l r =
      fromText "Filtered "
      <> case (l, r) of
            ([SeverityF (Just lh)], [rh]) ->
              if fromEnum rh >= fromEnum lh
                then (asCode . fromString) "Visible"
                else (asCode . fromString) "Invisible"
            ([SeverityF Nothing], [_rh]) -> "Invisible"
            _ -> mempty
      <> fromText " by config value: "
      <> mconcat (intersperse (fromText ", ")
          (map (asCode . fromString . show) l))

    limiterBuilder ::
         [(Text, Double)]
      -> Builder
    limiterBuilder [] = mempty
    limiterBuilder l  =
      fromText "\nLimiters: "
        <> mconcat (intersperse (fromText ", ")
            (map (\ (n, d) ->  fromText "Limiter "
                            <> (asCode . fromText) n
                            <> fromText " with frequency "
                            <> (asCode . fromString. show) d)
                 l))

    -- metricsBuilder :: (Text, Text) -> [(Text, Builder)]
    -- metricsBuilder (name, t) = (name, metricFormatToText t)

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
    coll <- fmap DocCollector (liftIO $ newIORef Map.empty)
    mapM_ (docTrace nsIdx coll) tracers
    pure coll
  where
    docTrace nsIdx dc (Trace tr) =
      mapM_
        (\ (ns, idx) ->
            T.traceWith tr (emptyLoggingContext {lcNSInner = nsGetInner ns},
                            Left (TCDocument idx (documentFor ns) (metricsDocFor ns) dc)))
        nsIdx

-------------------- Callbacks ---------------------------

docTracer :: MonadIO m =>
     BackendConfig
  -> Trace m FormattedMessage
docTracer backendConfig = Trace $ T.arrow $ T.emit output
  where
    output p@(_, Left TCDocument {}) =
      docIt backendConfig p
    output (_, _) = pure ()

docTracerDatapoint :: MonadIO m =>
     BackendConfig
  -> Trace m DataPoint
docTracerDatapoint backendConfig = Trace $ T.arrow $ T.emit output
  where
    output p@(_, Left TCDocument {}) =
      docItDatapoint backendConfig p
    output (_, _) = pure ()


-- | Callback for doc collection
addFiltered :: MonadIO m => TraceControl -> Maybe SeverityF -> m ()
addFiltered (TCDocument idx mdText mdMetrics (DocCollector docRef)) (Just sev) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldFiltered = seq sev (sev : ldFiltered e)})
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> seq mdText (seq mdMetrics (emptyLogDoc mdText mdMetrics))))
        docMap)
addFiltered _ _ = pure ()

-- | Callback for doc collection
addLimiter :: MonadIO m => TraceControl -> (Text, Double) -> m ()
addLimiter (TCDocument idx mdText mdMetrics (DocCollector docRef)) (ln, lf) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldLimiter = seq ln (seq lf ((ln, lf) : ldLimiter e))})
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> seq mdText (seq mdMetrics (emptyLogDoc mdText mdMetrics))))
        docMap)
addLimiter _ _ = pure ()

-- | Callback for doc collection
docIt :: MonadIO m
  => BackendConfig
  -> (LoggingContext, Either TraceControl a)
  -> m ()
docIt EKGBackend (LoggingContext{},
  Left (TCDocument idx mdText mdMetrics (DocCollector docRef))) = do
    liftIO $ modifyIORef docRef (\ docMap ->
        Map.insert
          idx
          ((\e -> e { ldBackends  = EKGBackend : ldBackends e
                    })
            (case Map.lookup idx docMap of
                          Just e  -> e
                          Nothing -> seq mdText (seq mdMetrics (emptyLogDoc mdText mdMetrics))))
          docMap)
docIt backend (LoggingContext {..},
  Left (TCDocument idx mdText mdMetrics (DocCollector docRef))) = do
    liftIO $ modifyIORef docRef (\ docMap ->
        Map.insert
          idx
          ((\e -> e { ldBackends  = backend : ldBackends e
                    , ldNamespace = nub ((lcNSOuter ++ lcNSInner) : ldNamespace e)
                    , ldSeverity  = case lcSeverity of
                                      Nothing -> ldSeverity e
                                      Just s  -> s : ldSeverity e
                    , ldPrivacy   = case lcPrivacy of
                                      Nothing -> ldPrivacy e
                                      Just p  -> p : ldPrivacy e
                    , ldDetails   = case lcDetails of
                                      Nothing -> ldDetails e
                                      Just d  -> d : ldDetails e
                    })
            (case Map.lookup idx docMap of
                          Just e  -> e
                          Nothing -> seq mdText (seq mdMetrics (emptyLogDoc mdText mdMetrics))))
          docMap)

-- | Callback for doc collection
docItDatapoint :: MonadIO m =>
     BackendConfig
  -> (LoggingContext, Either TraceControl a)
  -> m ()
docItDatapoint _backend (LoggingContext {..},
  Left (TCDocument idx mdText _mdMetrics (DocCollector docRef))) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldNamespace = nub ((lcNSOuter ++ lcNSInner) : ldNamespace e)
                  , ldBackends  = [DatapointBackend]
                  })
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> emptyLogDoc mdText []))
        docMap)


-- Finally generate a text from all the builders
docuResultsToText :: [([Text], DocuResult)] -> TraceConfig -> IO Text
docuResultsToText builderList configuration = do
  time <- getZonedTime
  let traceBuilders = sortBy (\ (l,_) (r,_) -> compare l r)
                          (filter (isTracer . snd) builderList)
      metricsBuilders = sortBy (\ (l,_) (r,_) -> compare l r)
                          (filter (isMetric .snd) builderList)
      datapointBuilders = sortBy (\ (l,_) (r,_) -> compare l r)
                          (filter (isDatapoint . snd) builderList)
      header  = fromText "# Cardano Trace Documentation\n\n"
      header1  = fromText "## Table Of Contents\n\n"
      toc      = generateTOC (map fst traceBuilders) (map fst metricsBuilders) (map fst datapointBuilders)
      header2  = fromText "\n## Trace Messages\n\n"
      contentT = mconcat $ intersperse (fromText "\n\n")
                              (map (unpackDocu . snd) traceBuilders)
      header3  = fromText "\n## Metrics\n\n"
      contentM = mconcat $ intersperse (fromText "\n\n")
                              (map (unpackDocu . snd) metricsBuilders)
      header4  = fromText "\n## Datapoints\n\n"
      contentD = mconcat $ intersperse (fromText "\n\n")
                              (map (unpackDocu . snd) datapointBuilders)
      config  = fromString $ "Configuration: " <> show configuration <> "\n\n"
      numbers = fromString $  show (length builderList) <> " log messages." <> "\n\n"
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
      <> ts)


generateTOC :: [[Text]] -> [[Text]] -> [[Text]] -> Builder
generateTOC traces metrics datapoints =
       generateTOCTraces
    <> generateTOCMetrics
    <> generateTOCDatapoints
  where
    generateTOCTraces =
      fromText "### [Trace Messages](#trace-messages)\n\n"
      <> mconcat (reverse (fst (foldl namespaceToToc ([], []) traces)))
      <> fromText "\n"
    generateTOCMetrics =
      fromText "### [Metrics](#metrics)\n\n"
      <> mconcat (reverse (fst (foldl namespaceToToc ([], []) (map splitToNS metrics))))
      <> fromText "\n"
    generateTOCDatapoints =
      fromText "### [Datapoints](#datapoints)\n\n"
      <> mconcat (reverse (fst (foldl namespaceToToc ([], []) datapoints)))
      <> fromText "\n"

    namespaceToToc :: ([Builder], [Text]) -> [Text]-> ([Builder], [Text])
    namespaceToToc (builders, context) ns =
      let ref = namespaceRefBuilder ns
          ns' = if take 2 ns == ["Cardano", "Node"]
                  then drop 2 ns
                  else ns
      in if init ns' == context
        then
          (( fromString (concat (replicate (length context) "\t"))
          <> fromText "1. "
          <> fromText "["
          <> fromText (last ns')
          <> fromText "](#"
          <> ref
          <> fromText ")\n") : builders, context)
        else
          let cpl  = commonPrefixLength context ns'
              ns'' = drop cpl ns'
              context' = take cpl context
          in namespaceToTocWithContext (builders, context') ns'' ref

    namespaceToTocWithContext ::
         ([Builder], [Text])
      -> [Text]
      -> Builder
      -> ([Builder], [Text])
    namespaceToTocWithContext (builders, context) ns ref =
      case ns of
        [single] ->   ((fromString (concat (replicate (length context) "\t"))
                      <> fromText "1. "
                      <> fromText "["
                      <> fromText single
                      <> fromText "](#"
                      <> ref
                      <> fromText ")\n") : builders, context)
        (hdn : tln) ->
          let builder = fromString (concat (replicate (length context) "\t"))
                        <> fromText "1. __"
                        <> fromText hdn
                        <> fromText "__\n"
          in  namespaceToTocWithContext
              (builder : builders, context ++ [hdn]) tln ref
        [] -> error "inpossible"

    splitToNS :: [Text] -> [Text]
    splitToNS [sym] = T.split (== '.') sym


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
                        <> fromText (T.unlines $ map ("> " <>) (T.lines t))
