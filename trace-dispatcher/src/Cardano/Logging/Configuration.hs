{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Cardano.Logging.Configuration
  ( configureTracers
  , withNamespaceConfig
  , filterSeverityFromConfig
  , readConfiguration
  ) where

import           Control.Exception (throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import qualified Data.Aeson as AE
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.List (foldl', maximumBy, nub)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Text (Text, split)
import           Data.Yaml
import           GHC.Generics

import           Cardano.Logging.Trace (filterTraceBySeverity)
import           Cardano.Logging.Types

data TraceOptionSeverity = TraceOptionSeverity {
      nsS      :: Text
    , severity :: SeverityF
    } deriving (Eq, Ord, Show)

instance AE.ToJSON TraceOptionSeverity where
    toJSON tos = object [ "ns" .= nsS tos
                        , "severity" .= AE.toJSON (severity tos)
                        ]

instance AE.FromJSON TraceOptionSeverity where
    parseJSON (Object obj) = TraceOptionSeverity
                           <$> obj .: "ns"
                           <*> obj .: "severity"

data TraceOptionDetail = TraceOptionDetail {
      nsD     :: Text
    , detail :: DetailLevel
    } deriving (Eq, Ord, Show, Generic)

instance AE.ToJSON TraceOptionDetail where
    toJSON tos = object [ "ns" .= nsD tos
                        , "detail" .= AE.toJSON (detail tos)
                        ]

instance AE.FromJSON TraceOptionDetail where
    parseJSON (Object obj) = TraceOptionDetail
                           <$> obj .: "ns"
                           <*> obj .: "detail"

data TraceOptionBackend = TraceOptionBackend {
      nsB     :: Text
    , backends :: [Backend]
    } deriving (Eq, Ord, Show, Generic)

instance AE.ToJSON TraceOptionBackend where
    toJSON tos = object [ "ns" .= nsB tos
                        , "backends" .= AE.toJSON (backends tos)
                        ]

instance AE.FromJSON TraceOptionBackend where
    parseJSON (Object obj) = TraceOptionBackend
                           <$> obj .: "ns"
                           <*> obj .: "backends"

data ConfigRepresentation = ConfigRepresentation {
    traceOptionSeverity :: [TraceOptionSeverity]
  , traceOptionDetail   :: [TraceOptionDetail]
  , traceOptionBackend  :: [TraceOptionBackend]
  }
  deriving (Eq, Ord, Show)

--instance AE.ToJSON ConfigRepresentation where
    -- toJSON tos = object [ "traceOptionSeverity" .= AE.toJSON $ traceOptionSeverity tos
    --                     , "traceOptionDetail"   .= AE.toJSON $ traceOptionDetail tos
    --                     , "traceOptionBackend"  .= AE.toJSON $ traceOptionBackend tos
    --                     ]

instance AE.FromJSON ConfigRepresentation where
    parseJSON (Object obj) = ConfigRepresentation
                           <$> obj .: "TraceOptionSeverity"
                           <*> obj .: "TraceOptionDetail"
                           <*> obj .: "TraceOptionBackend"

readConfiguration :: FilePath -> IO TraceConfig
readConfiguration fp =
    either throwIO pure =<< parseRepresentation <$> BS.readFile fp
--
-- -- | Reads the tracer's configuration file (path is passed via '--config' CLI option).
-- readTracerConfig :: FilePath -> IO TraceConfig
-- readTracerConfig pathToConfig =
--   eitherDecodeFileStrict' pathToConfig >>= \case
--     Left e -> Ex.die $ "Invalid tracer's configuration: " <> show e
--     Right (config :: TracerConfig) -> return config

parseRepresentation :: ByteString -> Either ParseException TraceConfig
parseRepresentation bs = fill (decodeEither' bs)
  where
    fill ::
         Either ParseException ConfigRepresentation
      -> Either ParseException TraceConfig
    fill (Left e)   = Left e
    fill (Right rl) = Right $ fill' emptyTraceConfig rl
    fill' :: TraceConfig -> ConfigRepresentation -> TraceConfig
    fill' (TraceConfig tc) cr =
      let tc'  = foldl' (\ tci (TraceOptionSeverity ns severity') ->
                          let ns' = split (=='.') ns
                          in Map.insertWith (++) ns' [CoSeverity severity'] tci)
                        tc
                        (traceOptionSeverity cr)
          tc'' = foldl' (\ tci (TraceOptionDetail ns detail') ->
                          let ns' = split (=='.') ns
                          in Map.insertWith (++) ns' [CoDetail detail'] tci)
                        tc'
                        (traceOptionDetail cr)
          tc''' = foldl' (\ tci (TraceOptionBackend ns backend') ->
                          let ns' = split (=='.') ns
                          in Map.insertWith (++) ns' [CoBackend backend'] tci)
                        tc''
                        (traceOptionBackend cr)
      in TraceConfig tc'''

-- | Call this function at initialisation, and later for reconfiguration
configureTracers :: Monad m => TraceConfig -> Documented a -> [Trace m a]-> m ()
configureTracers config (Documented documented) tracers = do
    mapM_ (configureTrace Reset) tracers
    mapM_ (configureAllTrace (Config config)) tracers
    mapM_ (configureTrace Optimize) tracers
  where
    configureTrace c (Trace tr) =
      T.traceWith tr (emptyLoggingContext, Just c, dmPrototype (head documented))
    configureAllTrace c (Trace tr) =
      mapM
        ((\ m -> T.traceWith tr (emptyLoggingContext, Just c, m)) . dmPrototype)
        documented

-- | Take a selector function, and a function from trace to trace with
--   this selector to make a trace transformer with a config value
withNamespaceConfig :: forall m a b. (MonadIO m, Ord b) =>
     (TraceConfig -> Namespace -> b)
  -> (Maybe b -> Trace m a -> Trace m a)
  -> Trace m a
  -> m (Trace m a)
withNamespaceConfig extract needsConfigFunc tr = do
    ref  <- liftIO (newIORef (Left (Map.empty, Nothing)))
    pure $ Trace $ T.arrow $ T.emit $ mkTrace ref
  where
    mkTrace ::
         IORef (Either (Map.Map Namespace b, Maybe b) b)
      -> (LoggingContext, Maybe TraceControl, a)
      -> m ()
    mkTrace ref (lc, Nothing, a) = do
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Right val ->
          T.traceWith
            (unpackTrace $ needsConfigFunc (Just val) tr) (lc, Nothing, a)
        Left (cmap, Just v) ->
          case Map.lookup (lcNamespace lc) cmap of
                Just val -> T.traceWith
                              (unpackTrace $ needsConfigFunc (Just val) tr)
                              (lc, Nothing, a)
                Nothing  -> T.traceWith
                              (unpackTrace $ needsConfigFunc (Just v) tr)
                              (lc, Nothing, a)
        Left (_cmap, Nothing) -> error "Missing configuration"
    mkTrace ref (lc, Just Reset, a) = do
      liftIO $ writeIORef ref (Left (Map.empty, Nothing))
      T.traceWith (unpackTrace $ needsConfigFunc Nothing tr) (lc, Just Reset, a)

    mkTrace ref (lc, Just (Config c), m) = do
      let ! val = extract c (lcNamespace lc)
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Left (cmap, Nothing) ->
          case Map.lookup (lcNamespace lc) cmap of
            Nothing -> do
              liftIO
                  $ writeIORef ref
                  $ Left (Map.insert (lcNamespace lc) val cmap, Nothing)
              T.traceWith
                (unpackTrace $ needsConfigFunc (Just val) tr)
                (lc, Just (Config c), m)
            Just v  -> do
              if v == val
                then T.traceWith
                      (unpackTrace $ needsConfigFunc (Just val) tr)
                      (lc, Just (Config c), m)
                else error $ "Inconsistent trace configuration with context "
                                  ++ show (lcNamespace lc)
        Right _val -> error $ "Trace not reset before reconfiguration (1)"
                            ++ show (lcNamespace lc)
        Left (_cmap, Just _v) -> error $ "Trace not reset before reconfiguration (2)"
                            ++ show (lcNamespace lc)

    mkTrace ref (lc, Just Optimize, m) = do
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Left (cmap, Nothing) ->
          case Map.size cmap of
            0 ->  -- This will never be called!?
                  pure ()
            1 -> do
                  case Map.elems cmap of
                    [val] -> do
                      liftIO $ writeIORef ref $ Right val
                      T.traceWith
                        (unpackTrace $ needsConfigFunc (Just val) tr)
                        (lc, Just Optimize, m)
                    _   -> error "Cardano.Logging.Configuration>>withConfig: Impossible"
            _n -> let eles = nub $ Map.elems cmap
                      decidingDict =
                        foldl
                            (\acc e -> Map.insertWith (\o _ -> o + 1 :: Integer) e 1 acc)
                            Map.empty eles
                      (mostCommon,_) = maximumBy (\(_, n') (_, m') -> compare n' m')
                                                 (Map.assocs decidingDict)
                      newmap = Map.filter (/= mostCommon) cmap
                  in do
                    liftIO $ writeIORef ref (Left (newmap, Just mostCommon))
                    T.traceWith
                      (unpackTrace $ needsConfigFunc Nothing tr)
                      (lc, Just Optimize, m)
        Right _val -> T.traceWith
                        (unpackTrace $ needsConfigFunc Nothing tr)
                        (lc, Just Optimize, m)
        Left (_cmap, Just _v) ->
                    T.traceWith
                        (unpackTrace $ needsConfigFunc Nothing tr)
                        (lc, Just Optimize, m)

-- | Filter a trace by severity and take the filter value from the config
filterSeverityFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
filterSeverityFromConfig = withNamespaceConfig getSeverity filterTraceBySeverity

--------------------------------------------------------
-- Internal

-- | If no severity can be found in the config, it is set to Warning
getSeverity :: TraceConfig -> Namespace -> SeverityF
getSeverity config context =
    fromMaybe WarningF (getOption severitySelector config context)
  where
    severitySelector :: ConfigOption -> Maybe SeverityF
    severitySelector (CoSeverity s) = Just s
    severitySelector _              = Nothing

-- | Searches in the config to find an option
getOption :: (ConfigOption -> Maybe a) -> TraceConfig -> Namespace -> Maybe a
getOption sel config [] =
  case Map.lookup [] (tcOptions config) of
    Nothing -> Nothing
    Just options -> case mapMaybe sel options of
                      []        -> Nothing
                      (opt : _) -> Just opt
getOption sel config context =
  case Map.lookup context (tcOptions config) of
    Nothing -> getOption sel config (tail context)
    Just options -> case mapMaybe sel options of
                      []        -> getOption sel config (init context)
                      (opt : _) -> Just opt
