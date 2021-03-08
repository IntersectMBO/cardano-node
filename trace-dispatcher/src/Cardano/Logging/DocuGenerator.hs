{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.DocuGenerator where

import           Cardano.Logging.Trace
import           Cardano.Logging.Types
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.Aeson.Text (encodeToLazyText)
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef,
                     writeIORef)
import           Data.List (intersperse, nub, sortBy)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Text.Internal.Builder (toLazyText)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromString, fromText,
                     singleton)

documentTracers :: MonadIO m => Documented a -> [Trace m a] -> m DocCollector
documentTracers (Documented documented) tracers = do
    let docIdx = zip documented [0..]
    coll <- fmap DocCollector (liftIO $ newIORef Map.empty)
    mapM_ (docTrace docIdx coll) tracers
    pure coll
  where
    docTrace docIdx docColl (Trace tr) =
      mapM_
        (\ (DocMsg {..}, idx) -> do
          T.traceWith tr (emptyLoggingContext {lcNamespace = [dmName]},
                          Just (Document idx dmMarkdown docColl), dmPrototype))
        docIdx

docIt :: MonadIO m =>
     Backend
  -> LogFormat
  -> (LoggingContext, Maybe TraceControl, a)
  -> m ()
docIt backend logFormat (LoggingContext {..},
  Just (Document idx mdText (DocCollector docRef)), msg) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldBackends  = (backend, logFormat) : ldBackends e
                  , ldNamespace = lcNamespace : ldNamespace e
                  , ldSeverity  = case lcSeverity of
                                    Nothing -> ldSeverity e
                                    Just s  -> s : ldSeverity e
                  , ldPrivacy   = case lcPrivacy of
                                    Nothing -> ldPrivacy e
                                    Just s  -> s : ldPrivacy e
                  , ldDetails   = case lcDetails of
                                    Nothing -> ldDetails e
                                    Just s  -> s : ldDetails e
                  })
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> emptyLogDoc mdText))
        docMap)

buildersToText :: [(Namespace, Builder)] -> Text
buildersToText builderList =
  let sortedBuilders = sortBy (\ (l,_) (r,_) -> compare l r) builderList
  in toStrict $ toLazyText
      $ mconcat
        $ intersperse (fromText "\n\n") (map snd sortedBuilders)

documentMarkdown :: (LogFormatting a, MonadIO m) =>
     Documented a
  -> [Trace m a]
  -> m [(Namespace, Builder)]
documentMarkdown (Documented documented) tracers = do
    DocCollector docRef <- documentTracers (Documented documented) tracers
    items <- fmap Map.toList (liftIO (readIORef docRef))
    let sortedItems = sortBy
                        (\ (_,l) (_,r) -> compare (ldNamespace l) (ldNamespace r))
                        items
    pure $ map (\(i, ld) -> (head (ldNamespace ld), documentItem (i, ld))) sortedItems
  where
    documentItem :: (Int, LogDoc) -> Builder
    documentItem (idx, ld@LogDoc {..}) = mconcat $ intersperse (fromText "\n\n")
      [ namespacesBuilder (nub ldNamespace)
      , representationBuilder (documented `listIndex` idx)
      , propertiesBuilder ld
      , backendsBuilder (nub ldBackends)
      , betweenLines (fromText ldDoc)
      ]

    namespacesBuilder :: [Namespace] -> Builder
    namespacesBuilder [ns] = namespaceBuilder ns
    namespacesBuilder []   = fromText "__Warning__: Namespace missing"
    namespacesBuilder nsl  =
      mconcat (intersperse (singleton '\n')(map namespaceBuilder nsl))
        <> fromText "Warning: Mutiple Namespaces"

    namespaceBuilder :: Namespace -> Builder
    namespaceBuilder ns = fromText "### " <>
      mconcat (intersperse (singleton '.') (map fromText ns))

    representationBuilder :: LogFormatting a => Maybe (DocMsg a) -> Builder
    representationBuilder Nothing = mempty
    representationBuilder (Just DocMsg {..}) = mconcat
      $ intersperse (singleton '\n')
        [case forHuman dmPrototype of
          "" -> mempty
          t  -> fromText "For human:\n" <> asCode (fromText t)
        , let r1 = forMachine DBrief dmPrototype
              r2 = forMachine DRegular dmPrototype
              r3 = forMachine DDetailed dmPrototype
          in if r1 == mempty && r2 == mempty && r3 == mempty
            then mempty
              else if r1 == r2 && r2 == r3
                then fromText "For machine:\n"
                      <> asCode (fromText (toStrict (encodeToLazyText r1)))
                else if r1 == r2
                  then fromText "For machine regular:\n"
                        <> asCode (fromText (toStrict (encodeToLazyText r2)))
                        <> fromText "\nFor machine detailed: "
                        <> asCode (fromText (toStrict (encodeToLazyText r3)))
                  else if r2 == r3
                    then fromText "For machine brief:\n"
                          <> asCode (fromText (toStrict (encodeToLazyText r1)))
                          <> fromText "\nFor machine regular:\n"
                          <> asCode (fromText (toStrict (encodeToLazyText r2)))
                    else fromText "For machine brief:\n"
                          <> asCode (fromText (toStrict (encodeToLazyText r1)))
                          <> fromText "\nFor machine regular:\n"
                          <> asCode (fromText (toStrict (encodeToLazyText r2)))
                          <> fromText "\nFor machine detailed:\n"
                          <> asCode (fromText (toStrict (encodeToLazyText r3)))
        , case asMetrics dmPrototype of
            [] -> mempty
            l -> mconcat
                  (intersperse (singleton '\n')
                    (map
                      (\case
                        (IntM mbT i) -> fromText "Integer metrics:\n"
                          <> case mbT of
                              Nothing -> mempty
                              Just n -> asCode (fromText n <> singleton ' '
                                        <> fromString (show i))
                        (DoubleM mbT i) -> fromText "Double metrics:\n"
                          <> case mbT of
                              Nothing -> mempty
                              Just n -> asCode (fromText n <> singleton ' '
                                        <> fromString (show i)))
                      l))
        ]

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
      <>
        case nub ldDetails of
          []  -> fromText "\nDetails:   " <> asCode (fromString (show DRegular))
          [d] -> fromText "\nDetails:   " <> asCode (fromString (show d))
          l   -> fromText "\nDetails:   "
                  <> mconcat (intersperse (singleton ',')
                        (map (asCode . fromString . show) l))

    backendsBuilder :: [(Backend, LogFormat)] -> Builder
    backendsBuilder [] = fromText "No backends found"
    backendsBuilder l  = fromText "Backends: "
                          <> mconcat (intersperse (fromText ", ")
                                (map backendFormatToText l))

    backendFormatToText :: (Backend, LogFormat) -> Builder
    backendFormatToText (be,lf) = asCode (fromString (show be))
                            <> fromText " / "
                            <> asCode (fromString (show lf))


asCode :: Builder -> Builder
asCode b = singleton '`' <> b <> singleton '`'

betweenLines :: Builder -> Builder
betweenLines b = fromText "\n***\n" <> b <> fromText "\n***\n"


listIndex :: [a] -> Int -> Maybe a
listIndex l i = if i >= length l
                  then Nothing
                  else Just (l !! i)
