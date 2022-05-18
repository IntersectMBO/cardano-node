{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.Configuration
  ( configureTracers
  , withNamespaceConfig
  , filterSeverityFromConfig
  , withDetailsFromConfig
  , withBackendsFromConfig
  , withLimitersFromConfig

  , getSeverity
  , getDetails
  , getBackends
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Control.Tracer as T
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.List (maximumBy, nub)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Text (Text, intercalate, unpack)

import           Cardano.Logging.DocuGenerator (addFiltered, addLimiter)
import           Cardano.Logging.FrequencyLimiter (LimitingMessage (..), limitFrequency)
import           Cardano.Logging.Trace (filterTraceBySeverity, setDetails)
import           Cardano.Logging.Types



-- | Call this function at initialisation, and later for reconfiguration
configureTracers :: forall a.
     TraceConfig
  -> Documented a
  -> [Trace IO a]
  -> IO ()
configureTracers config (Documented documented) tracers = do
    mapM_ (configureTrace Reset) tracers
    mapM_ (configureAllTrace (Config config)) tracers
    mapM_ (configureTrace Optimize) tracers
  where
    configureTrace control (Trace tr) =
      T.traceWith tr (emptyLoggingContext, Left control)
    configureAllTrace control (Trace tr) =
      mapM
        (\ DocMsg {..} -> T.traceWith tr (
                              emptyLoggingContext {lcNamespace = dmNamespace}
                            , Left control))
        documented



-- | Take a selector function called 'extract'.
-- Take a function from trace to trace with this config dependent value.
-- In this way construct a trace transformer with a config value
withNamespaceConfig :: forall m a b c. (MonadIO m, Ord b) =>
     String
  -> (TraceConfig -> Namespace -> m b)
  -> (Maybe b -> Trace m c -> m (Trace m a))
  -> Trace m c
  -> m (Trace m a)
withNamespaceConfig name extract withConfig tr = do
    ref  <- liftIO (newIORef (Left (Map.empty, Nothing)))
    pure $ Trace $ T.arrow $ T.emit $ mkTrace ref
  where
    mkTrace ::
         IORef (Either (Map.Map Namespace b, Maybe b) b)
      -> (LoggingContext, Either TraceControl a)
      -> m ()
    mkTrace ref (lc, Right a) = do
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Right val -> do
          tt <- withConfig (Just val) tr
          T.traceWith
            (unpackTrace tt) (lc, Right a)
        Left (cmap, Just v) ->
          case Map.lookup (lcNamespace lc) cmap of
                Just val -> do
                  tt <- withConfig (Just val) tr
                  T.traceWith (unpackTrace tt) (lc, Right a)
                Nothing  -> do
                  tt <- withConfig (Just v) tr
                  T.traceWith (unpackTrace tt) (lc, Right a)
        Left (_cmap, Nothing) -> pure ()
        -- This can happen during reconfiguration, so we don't throw an error any more
    mkTrace ref (lc, Left Reset) = do
      liftIO $ writeIORef ref (Left (Map.empty, Nothing))
      tt <- withConfig Nothing tr
      T.traceWith (unpackTrace tt) (lc, Left Reset)

    mkTrace ref (lc, Left (Config c)) = do
      ! val <- extract c (lcNamespace lc)
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Left (cmap, Nothing) ->
          case Map.lookup (lcNamespace lc) cmap of
            Nothing -> do
              liftIO
                  $ writeIORef ref
                  $ Left (Map.insert (lcNamespace lc) val cmap, Nothing)
              Trace tt <- withConfig (Just val) tr
              T.traceWith tt (lc, Left (Config c))
            Just v  -> do
              if v == val
                then do
                  Trace tt <- withConfig (Just val) tr
                  T.traceWith tt (lc, Left (Config c))
                else error $ "Inconsistent trace configuration with context "
                                  ++ show (lcNamespace lc)
        Right _val -> error $ "Trace not reset before reconfiguration (1)"
                            ++ show (lcNamespace lc)
        Left (_cmap, Just _v) -> error $ "Trace not reset before reconfiguration (2)"
                            ++ show (lcNamespace lc)

    mkTrace ref (lc, Left Optimize) = do
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Left (cmap, Nothing) ->
          case nub (Map.elems cmap) of
            []     -> pure ()
            [val]  -> do
                        liftIO $ writeIORef ref $ Right val
                        Trace tt <- withConfig (Just val) tr
                        T.traceWith tt (lc, Left Optimize)
            _      -> let decidingDict =
                            foldl
                              (\acc e -> Map.insertWith (+) e (1 :: Int) acc)
                              Map.empty
                              (Map.elems cmap)
                          (mostCommon, _) = maximumBy
                                              (\(_, n') (_, m') -> compare n' m')
                                              (Map.assocs decidingDict)
                          newmap = Map.filter (/= mostCommon) cmap
                      in do
                        liftIO $ writeIORef ref (Left (newmap, Just mostCommon))
                        Trace tt <- withConfig Nothing tr
                        T.traceWith tt (lc, Left Optimize)
        Right _val -> error $ "Trace not reset before reconfiguration (3)"
                            ++ show (lcNamespace lc)
        Left (_cmap, Just _v) ->
                      error $ "Trace not reset before reconfiguration (4)"
                                  ++ show (lcNamespace lc)
    mkTrace ref (lc, Left dc@Document {}) = do
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Right val -> do
          tt <- withConfig (Just val) tr
          T.traceWith
            (unpackTrace tt) (lc, Left dc)
        Left (cmap, Just v) ->
          case Map.lookup (lcNamespace lc) cmap of
                Just val -> do
                  tt <- withConfig (Just val) tr
                  T.traceWith (unpackTrace tt) (lc, Left dc)
                Nothing  -> do
                  tt <- withConfig (Just v) tr
                  T.traceWith (unpackTrace tt) (lc, Left dc)
        Left (_cmap, Nothing) -> error ("Missing configuration(2) " <> name <> " ns " <> show (lcNamespace lc))


-- | Filter a trace by severity and take the filter value from the config
filterSeverityFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
filterSeverityFromConfig =
    withNamespaceConfig
      "severity"
      getSeverity'
      (\ mbSev (Trace tr) ->
      pure $ Trace $ T.arrow $ T.emit $
        \case
          (lc, Left c@Document {}) -> do
            addFiltered c mbSev
            T.traceWith
              (unpackTrace (filterTraceBySeverity mbSev (Trace tr)))
              (lc, Left c)
          (lc, cont) -> do
            T.traceWith
              (unpackTrace (filterTraceBySeverity mbSev (Trace tr)))
              (lc, cont))

-- | Set detail level of a trace from the config
withDetailsFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
withDetailsFromConfig =
  withNamespaceConfig
    "details"
    getDetails'
    (\mbDtl b -> case mbDtl of
              Just dtl -> pure $ setDetails dtl b
              Nothing  -> pure $ setDetails DNormal b)

-- | Routing and formatting of a trace from the config
withBackendsFromConfig :: (MonadIO m) =>
  (Maybe [BackendConfig] -> Trace m FormattedMessage -> m (Trace m a))
  -> m (Trace m a)
withBackendsFromConfig routerAndFormatter =
  withNamespaceConfig
    "backends"
    getBackends'
    routerAndFormatter
    (Trace T.nullTracer)

data Limiter m a = Limiter Text Double (Trace m a)

instance Eq (Limiter m a) where
  Limiter t1 _ _ == Limiter t2 _ _ = t1 == t2

instance Ord (Limiter m a) where
  Limiter t1 _ _ <= Limiter t2 _ _ = t1 <= t2

instance Show (Limiter m a) where
  show (Limiter name _ _) = "Limiter " <> unpack name


-- | Routing and formatting of a trace from the config
withLimitersFromConfig :: forall a m .(MonadUnliftIO m) =>
     Trace m a
  -> Trace m LimitingMessage
  -> m (Trace m a)
withLimitersFromConfig tr trl = do
    ref <- liftIO $ newIORef Map.empty
    withNamespaceConfig
      "limiters"
      (getLimiter ref)
      withLimiter
      tr
  where
    -- | May return a limiter, which is a stateful transformation from trace to trace
    getLimiter ::
         IORef (Map.Map Text (Limiter m a))
      -> TraceConfig
      -> Namespace
      -> m (Maybe (Limiter m a))
    getLimiter stateRef config ns =
      case getLimiterSpec config ns of
        Nothing -> pure Nothing
        Just (name, frequency) -> do
          state <- liftIO $ readIORef stateRef
          case Map.lookup name state of
            Just limiter -> pure $ Just limiter
            Nothing -> do
              limiterTrace <- limitFrequency frequency name tr trl
              let limiter = Limiter name frequency limiterTrace
              liftIO $ writeIORef stateRef (Map.insert name limiter state)
              pure $ Just limiter

    withLimiter ::
         Maybe (Maybe (Limiter m a))
      -> Trace m a
      -> m (Trace m a)
    withLimiter Nothing tr' = pure tr'
    withLimiter (Just Nothing) tr' = pure tr'


    withLimiter (Just (Just (Limiter n d (Trace trli)))) (Trace tr') =
      pure $ Trace $ T.arrow $ T.emit $
        \ case
          (lc, Right v) ->
            T.traceWith trli (lc, Right v)
          (lc, Left c@Document {}) -> do
            addLimiter c (n, d)
            T.traceWith tr' (lc, Left c)
          (lc, Left c) ->
            T.traceWith tr' (lc, Left c)

--------------------------------------------------------

-- | If no severity can be found in the config, it is set to Warning
getSeverity :: TraceConfig -> Namespace -> SeverityF
getSeverity config ns =
    fromMaybe (SeverityF (Just Warning)) (getOption severitySelector config ns)
  where
    severitySelector :: ConfigOption -> Maybe SeverityF
    severitySelector (ConfSeverity s) = Just s
    severitySelector _              = Nothing

getSeverity' :: Applicative m => TraceConfig -> Namespace -> m SeverityF
getSeverity' config ns = pure $ getSeverity config ns

-- | If no details can be found in the config, it is set to DNormal
getDetails :: TraceConfig -> Namespace -> DetailLevel
getDetails config ns =
    fromMaybe DNormal (getOption detailSelector config ns)
  where
    detailSelector :: ConfigOption -> Maybe DetailLevel
    detailSelector (ConfDetail d) = Just d
    detailSelector _            = Nothing

getDetails' :: Applicative m => TraceConfig -> Namespace -> m DetailLevel
getDetails' config ns = pure $ getDetails config ns

-- | If no backends can be found in the config, it is set to
-- [EKGBackend, Forwarder, Stdout HumanFormatColoured]
getBackends :: TraceConfig -> Namespace -> [BackendConfig]
getBackends config ns =
    fromMaybe [EKGBackend, Forwarder, Stdout HumanFormatColoured]
      (getOption backendSelector config ns)
  where
    backendSelector :: ConfigOption -> Maybe [BackendConfig]
    backendSelector (ConfBackend s) = Just s
    backendSelector _             = Nothing

getBackends' :: Applicative m => TraceConfig -> Namespace -> m [BackendConfig]
getBackends' config ns = pure $ getBackends config ns

-- | May return a limiter specification
getLimiterSpec :: TraceConfig -> Namespace -> Maybe (Text, Double)
getLimiterSpec config ns = getOption limiterSelector config ns
  where
    limiterSelector :: ConfigOption -> Maybe (Text, Double)
    limiterSelector (ConfLimiter f) = Just (intercalate "." ns, f)
    limiterSelector _               = Nothing

-- | Searches in the config to find an option
getOption :: (ConfigOption -> Maybe a) -> TraceConfig -> Namespace -> Maybe a
getOption sel config [] =
  case Map.lookup [] (tcOptions config) of
    Nothing -> Nothing
    Just options -> case mapMaybe sel options of
                      []        -> Nothing
                      (opt : _) -> Just opt
getOption sel config ns =
  case Map.lookup ns (tcOptions config) of
    Nothing -> getOption sel config (init ns)
    Just options -> case mapMaybe sel options of
                      []        -> getOption sel config (init ns)
                      (opt : _) -> Just opt
