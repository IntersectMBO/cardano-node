{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.DocuGenerator (
    documentMarkdown
  , buildersToText
  , docIt
  , addFiltered
  , addLimiter
  , docTracer
  , docTracerDatapoint
  , anyProto
  , showT
  , DocuResult
  , appDoc
  , mapDoc
) where

import           Cardano.Logging.Types
import           Control.Exception (SomeException, catch)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.List (intersperse, nub, sortBy)
import qualified Data.Map as Map
import           Data.Text (Text, pack, toLower)
import qualified Data.Text as T
import           Data.Text.Internal.Builder (toLazyText)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromString, fromText,
                     singleton)
import           Data.Time (getZonedTime)
import           Trace.Forward.Utils.DataPoint (DataPoint (..))

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

anyProto :: a
anyProto = undefined

showT :: Show a => a -> Text
showT = pack . show

appDoc :: (a -> b) -> DocMsg a -> DocMsg b
appDoc f DocMsg {..} = DocMsg  (f dmPrototype) dmMetricsMD dmMarkdown

mapDoc :: (a -> b) -> [DocMsg a] -> [DocMsg b]
mapDoc f = map (appDoc f)

docTracer :: MonadIO m
  => BackendConfig
  -> Trace m FormattedMessage
docTracer backendConfig = Trace $ T.arrow $ T.emit output
  where
    output p@(_, Just Document {}, FormattedMetrics m) =
      docIt backendConfig (FormattedMetrics m) p
    output (lk, Just c@Document {}, FormattedForwarder lo) =
      docIt backendConfig (FormattedForwarder lo) (lk, Just c, lo)
    output (lk, Just c@Document {}, FormattedHuman co msg) =
      docIt backendConfig (FormattedHuman co "") (lk, Just c, msg)
    output (lk, Just c@Document {}, FormattedMachine msg) =
      docIt backendConfig (FormattedMachine "") (lk, Just c, msg)
    output (_, _, _) = pure ()

docTracerDatapoint :: MonadIO m
  => BackendConfig
  -> Trace m DataPoint
docTracerDatapoint backendConfig = Trace $ T.arrow $ T.emit output
  where
    output p@(_, Just Document {}, DataPoint m) =
      docItDatapoint backendConfig (DataPoint m) p
    output (_, _, _) = pure ()

documentTracers :: Documented a -> [Trace IO a] -> IO DocCollector
documentTracers (Documented documented) tracers = do
    let docIdx = zip documented [0..]
    coll <- fmap DocCollector (liftIO $ newIORef Map.empty)
    mapM_ (docTrace docIdx coll) tracers
    pure coll
  where
    docTrace docIdx dc@(DocCollector docColl) (Trace tr) =
      mapM_
        (\ (DocMsg {..}, idx) -> catch (
            T.traceWith tr (emptyLoggingContext,
                            Just (Document idx dmMarkdown dmMetricsMD dc), dmPrototype))
            (\ (ex :: SomeException) ->
                modifyIORef docColl
                  (\ docMap ->
                    Map.insert
                      idx
                      (case Map.lookup idx docMap of
                        Just e  -> e { ldDoc = ldDoc e <> "Error generating doc" <> (pack . show) ex}
                        Nothing -> emptyLogDoc ("Error generating doc" <> (pack . show) ex) [])
                      docMap)))
        docIdx


addFiltered :: MonadIO m => TraceControl -> Maybe SeverityF -> m ()
addFiltered (Document idx mdText mdMetrics (DocCollector docRef)) (Just sev) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldFiltered = seq sev (sev : ldFiltered e)})
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> seq mdText (seq mdMetrics (emptyLogDoc mdText mdMetrics))))
        docMap)
addFiltered _ _ = pure ()

addLimiter :: MonadIO m => TraceControl -> (Text, Double) -> m ()
addLimiter (Document idx mdText mdMetrics (DocCollector docRef)) (ln, lf) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldLimiter = seq ln (seq lf ((ln, lf) : ldLimiter e))})
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> seq mdText (seq mdMetrics (emptyLogDoc mdText mdMetrics))))
        docMap)
addLimiter _ _ = pure ()

docIt :: MonadIO m =>
     BackendConfig
  -> FormattedMessage
  -> (LoggingContext, Maybe TraceControl, a)
  -> m ()
docIt backend formattedMessage (LoggingContext {..},
  Just (Document idx mdText mdMetrics (DocCollector docRef)), _msg) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldBackends  = seq backend (seq formattedMessage
                                    ((backend, formattedMessage) : ldBackends e))
                  , ldNamespace = seq lcNamespace
                                    (lcNamespace : ldNamespace e)
                  , ldSeverity  = case lcSeverity of
                                    Nothing -> ldSeverity e
                                    Just s  -> seq s (s : ldSeverity e)
                  , ldPrivacy   = case lcPrivacy of
                                    Nothing -> ldPrivacy e
                                    Just p  -> seq p (p : ldPrivacy e)
                  , ldDetails   = case lcDetails of
                                    Nothing -> ldDetails e
                                    Just d  -> seq d (d : ldDetails e)
                  })
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> seq mdText (seq mdMetrics (emptyLogDoc mdText mdMetrics))))
        docMap)

docItDatapoint :: MonadIO m =>
     BackendConfig
  -> DataPoint
  -> (LoggingContext, Maybe TraceControl, a)
  -> m ()
docItDatapoint _backend _formattedMessage (LoggingContext {..},
  Just (Document idx mdText _mdMetrics (DocCollector docRef)), _msg) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldNamespace = seq lcNamespace
                                    (lcNamespace : ldNamespace e)
                  , ldBackends  = [(DatapointBackend, FormattedMachine "")]
                  })
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> seq mdText (emptyLogDoc mdText [])))
        docMap)

generateTOC :: [Namespace] -> [Namespace] -> [Namespace] -> Builder
generateTOC traces metrics datapoints =
       generateTOCTraces
    <> generateTOCMetrics
    <> generateTOCDatapoints
  where
    generateTOCTraces =
      fromText "\n1.[Trace Messages] (#trace-messages)"
      <> mconcat (zipWith (curry namespaceToToc) traces [(1 :: Int) .. ])
    generateTOCDatapoints =
      fromText "\n3.[Datapoints] (#datapoints)"
      <> mconcat (zipWith (curry namespaceToToc) datapoints [(1 :: Int) .. ])
    namespaceToToc (ns, i) =
      fromText "\n1."
      <> fromText ((pack.show) i)
      <> fromText " ["
      <> namespaceBuilder ns
      <> fromText "] (#"
      <> namespaceRefBuilder ns
      <> fromText ")"
    namespaceBuilder ns = mconcat (intersperse (singleton '.') (map fromText ns))
    namespaceRefBuilder ns = mconcat (map (fromText . toLower) ns)
    generateTOCMetrics =
      fromText "\n2.[Metrics] (#metrics)"
      <> mconcat (zipWith (curry nameToToc) metrics [(1 :: Int) .. ])
    nameToToc ([name], i) =
      fromText "\n2."
      <> fromText ((pack.show) i)
      <> fromText " ["
      <> fromText name
      <> fromText "] (#"
      <> fromText (nameRefBuilder name)
      <> fromText ")"
    nameToToc (list, _i) =
      fromText "unexpected" <> fromText ((pack . show) list)
    nameRefBuilder name = toLower $ T.filter (/= '.') name

buildersToText :: [(Namespace, DocuResult)] -> TraceConfig -> IO Text
buildersToText builderList configuration = do
  time <- getZonedTime
  let traceBuilders = sortBy (\ (l,_) (r,_) -> compare l r)
                          (filter (isTracer . snd) builderList)
      metricsBuilders = sortBy (\ (l,_) (r,_) -> compare l r)
                          (filter (isMetric .snd) builderList)
      datapointBuilders = sortBy (\ (l,_) (r,_) -> compare l r)
                          (filter (isDatapoint . snd) builderList)
      header  = fromText "# Cardano Trace Documentation"
      header1  = fromText "# Table Of Contents"
      toc      = generateTOC (map fst traceBuilders) (map fst metricsBuilders) (map fst datapointBuilders)
      header2  = fromText "\n\n## Trace Messages\n"
      contentT = mconcat $ intersperse (fromText "\n\n")
                              (map (unpackDocu . snd) traceBuilders)
      header3  = fromText "\n\n## Metrics\n"
      contentM = mconcat $ intersperse (fromText "\n\n")
                              (map (unpackDocu . snd) metricsBuilders)
      header4  = fromText "\n\n## Datapoints\n"
      contentD = mconcat $ intersperse (fromText "\n\n")
                              (map (unpackDocu . snd) datapointBuilders)
      config  = fromString $ "\n\nConfiguration: " <> show configuration
      numbers = fromString $ "\n\n" <> show (length builderList) <> " log messages."
      ts      = fromString $ "\nGenerated at " <> show time <> "."
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

documentMarkdown ::
     Documented a
  -> [Trace IO a]
  -> IO [(Namespace, DocuResult)]
documentMarkdown (Documented documented) tracers = do
    DocCollector docRef <- documentTracers (Documented documented) tracers
    items <- fmap Map.toList (liftIO (readIORef docRef))
    let sortedItems = sortBy
                        (\ (_,l) (_,r) -> compare (ldNamespace l) (ldNamespace r))
                        items
    let messageDocs = map (\(i, ld) -> case ldNamespace ld of
                                []     -> (["No Namespace"], documentItem (i, ld))
                                (hn:_) -> (hn, documentItem (i, ld))) sortedItems
        metricsItems = filter (not . Map.null . ldMetricsDoc . snd) sortedItems
        metricsDocs = map documentMetrics metricsItems
    pure $ messageDocs ++ concat metricsDocs
  where
    documentItem :: (Int, LogDoc) -> DocuResult
    documentItem (_idx, ld@LogDoc {..}) =
      case ldBackends of
        [(DatapointBackend, _)] -> DocuDatapoint $
                    mconcat $ intersperse (fromText "\n\n")
                      [ namespacesBuilder (nub ldNamespace)
                      , betweenLines (fromText ldDoc)
                      ]
        _                     -> DocuTracer $
                    mconcat $ intersperse (fromText "\n\n")
                      [ namespacesBuilder (nub ldNamespace)
                      , betweenLines (fromText ldDoc)
                      , propertiesBuilder ld
                      , configBuilder ld
                      ]

    documentMetrics :: (Int, LogDoc) -> [([Text], DocuResult)]
    documentMetrics (_idx, ld@LogDoc {..}) =
      map (\(name, builder) -> ([name] , DocuMetric $
          mconcat $ intersperse (fromText "\n\n")
            [ builder
            , namespacesMetricsBuilder (nub ldNamespace)
            , configMetricsBuilder ld
            ]))
          $ metricsBuilder ldMetricsDoc

    namespacesBuilder :: [Namespace] -> Builder
    namespacesBuilder [ns] = namespaceBuilder ns
    namespacesBuilder []   = fromText "__Warning__: Namespace missing"
    namespacesBuilder nsl  =
      mconcat (intersperse (singleton '\n')(map namespaceBuilder nsl))

    namespaceBuilder :: Namespace -> Builder
    namespaceBuilder ns = fromText "### " <>
      mconcat (intersperse (singleton '.') (map fromText ns))

    namespacesMetricsBuilder :: [Namespace] -> Builder
    namespacesMetricsBuilder [ns] = fromText "Dispatched by: \n" <> namespaceMetricsBuilder ns
    namespacesMetricsBuilder []   = mempty
    namespacesMetricsBuilder nsl  = fromText "Dispatched by: \n" <>
      mconcat (intersperse (singleton '\n')(map namespaceMetricsBuilder nsl))

    namespaceMetricsBuilder :: Namespace -> Builder
    namespaceMetricsBuilder ns = mconcat (intersperse (singleton '.') (map fromText ns))

    propertiesBuilder :: LogDoc -> Builder
    propertiesBuilder LogDoc {..} =
        case nub ldSeverity of
          []  -> fromText "> Severity:   " <> asCode (fromString (show Info))
          [s] -> fromText "> Severity:   " <> asCode (fromString (show s))
          l   -> fromText "> Severities: "
                  <> mconcat (intersperse (singleton ',')
                        (map (asCode . fromString . show) l))
      <>
        case nub ldPrivacy of
          []  -> fromText "\nPrivacy:   " <> asCode (fromString (show Public))
          [p] -> fromText "\nPrivacy:   " <> asCode (fromString (show p))
          l   -> fromText "\nPrivacies: "
                  <> mconcat (intersperse (singleton ',')
                        (map (asCode . fromString . show) l))

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

    configMetricsBuilder :: LogDoc -> Builder
    configMetricsBuilder LogDoc {..} =
      fromText "From current configuration:\n"
      <> filteredBuilder (nub ldFiltered) (nub ldSeverity)
      <> limiterBuilder (nub ldLimiter)

    backendsBuilder :: [(BackendConfig, FormattedMessage)] -> Builder
    backendsBuilder [] = fromText "No backends found"
    backendsBuilder l  = fromText "Backends:\n\t\t\t"
                          <> mconcat (intersperse (fromText ",\n\t\t\t")
                                (map backendFormatToText l))

    backendFormatToText :: (BackendConfig, FormattedMessage) -> Builder
    backendFormatToText (be, FormattedMetrics _) = asCode (fromString   (show be))

    backendFormatToText (be, FormattedHuman _c _) = asCode (fromString (show be))
    backendFormatToText (be, FormattedMachine _) = asCode (fromString (show be))
    backendFormatToText (be, FormattedForwarder _) = asCode (fromString (show be))


    filteredBuilder :: [SeverityF] -> [SeverityS] -> Builder
    filteredBuilder [] _ = mempty
    filteredBuilder l r =
      fromText "Filtered: "
      <> case (l, r) of
            ([SeverityF (Just lh)], [rh]) ->
              if fromEnum rh >= fromEnum lh
                then (asCode . fromString) "Visible"
                else (asCode . fromString) "Invisible"
            ([SeverityF Nothing], [_rh]) -> "Invisible"
            _ -> mempty
      <> fromText " because the filter level is "
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

    metricsBuilder :: Map.Map Text Text -> [(Text, Builder)]
    metricsBuilder metricsDoc =
      let metricsList = Map.toList metricsDoc
      in map (\t@(name, _) -> (name, metricFormatToText t)) metricsList

    metricFormatToText :: (Text, Text) -> Builder
    metricFormatToText (name, text) =
      fromText "### "
        <> fromText name
          <> fromText "\n"
            <> betweenLines (fromText text)

asCode :: Builder -> Builder
asCode b = singleton '`' <> b <> singleton '`'

betweenLines :: Builder -> Builder
betweenLines b = fromText "\n***\n" <> b <> fromText "\n***\n"
