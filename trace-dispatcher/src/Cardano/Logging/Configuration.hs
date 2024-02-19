{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.Configuration
  ( ConfigReflection (..)
  , emptyConfigReflection
  , configureTracers
  , withNamespaceConfig
  , filterSeverityFromConfig
  , withDetailsFromConfig
  , withBackendsFromConfig
  , withLimitersFromConfig

  , maybeSilent
  , isSilentTracer
  , hasNoMetrics

  , getSeverity
  , getDetails
  , getBackends
  ) where

import           Cardano.Logging.DocuGenerator (addFiltered, addLimiter, addSilent)
import           Cardano.Logging.FrequencyLimiter (limitFrequency)
import           Cardano.Logging.Trace
import           Cardano.Logging.TraceDispatcherMessage
import           Cardano.Logging.Types

import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Control.Tracer as T
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import           Data.List (maximumBy, nub)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import           Data.Text (Text, intercalate, unpack)


-- | Call this function at initialisation, and later for reconfiguration.
-- Config reflection is used to optimise the tracers and has to collect
-- information about the tracers. Although it is possible to give more then
-- one tracer of the same time, it is not a common case to do this.
configureTracers :: forall a m.
     (MetaTrace a
  ,  MonadIO m)
  => ConfigReflection
  -> TraceConfig
  -> [Trace m a]
  -> m ()
configureTracers cr config tracers = do
    mapM_ (\t -> do
            configureTrace TCReset t
            configureAllTrace (TCConfig config) t
            configureTrace (TCOptimize cr) t)
          tracers
  where
    configureTrace control (Trace tr) =
      T.traceWith tr (emptyLoggingContext, Left control)
    configureAllTrace control (Trace tr) =
      mapM_  (\ ns ->
              T.traceWith
                tr
                (emptyLoggingContext
                  { lcNSInner = nsInner ns}
                  , Left control))
            (allNamespaces :: [Namespace a])

-- | Switch off any message of a particular tracer based on the configuration.
-- If the top tracer is silent and no subtracer is not silent, then switch it off
maybeSilent :: forall m a. (MonadIO m) =>
   ( TraceConfig -> Namespace a -> Bool)
  -> [Text]
  -> Bool
  -> Trace m a
  -> m (Trace m a)
maybeSilent selectorFunc prefixNames isMetrics (Trace tr) = do
    ref  <- liftIO (newIORef Nothing)
    contramapMCond (Trace tr) (mapFunc ref)
  where
    mapFunc ref =
      \case
        (lc, Right a) -> do
          silence <- liftIO $ readIORef ref
          if silence == Just True
            then pure Nothing
            else pure $ Just (lc, Right a)
        (lc, Left (TCConfig c)) -> do
          silence <- liftIO $ readIORef ref
          case silence of
            Nothing -> do
              let val = selectorFunc c (Namespace prefixNames [] :: Namespace a)
              liftIO $ writeIORef ref (Just val)
            Just _ -> pure ()
          pure $ Just (lc, Left (TCConfig c))
        (lc, Left TCReset) -> do
          liftIO $ writeIORef ref Nothing
          pure $ Just (lc, Left TCReset)
        (lc, Left (TCOptimize cr)) -> do
          silence <- liftIO $ readIORef ref
          case silence of
            Just True -> liftIO $ if isMetrics
                                    then modifyIORef (crNoMetrics cr) (Set.insert prefixNames)
                                    else modifyIORef (crSilent cr) (Set.insert prefixNames)
            _         -> pure ()
          liftIO $ modifyIORef (crAllTracers cr) (Set.insert prefixNames)
          pure $ Just (lc,  Left (TCOptimize cr))
        (lc, Left c@TCDocument {}) -> do
          silence <- liftIO $ readIORef ref
          unless isMetrics
            (addSilent c silence)
          pure $ Just (lc,  Left c)


-- When all messages are filtered out, it is silent
isSilentTracer :: forall a. MetaTrace a => TraceConfig -> Namespace a -> Bool
isSilentTracer tc (Namespace prefixNS _) =
    let allNS = allNamespaces :: [Namespace a]
    in all (\ (Namespace _ innerNS) ->
              isFiltered (Namespace prefixNS innerNS :: Namespace a))
           allNS
  where
    isFiltered :: Namespace a -> Bool
    isFiltered ns =
      let msgSeverity    = severityFor ns Nothing
          severityFilter = getSeverity tc ns
      in case severityFilter of
            SeverityF Nothing -> True -- silent config
            SeverityF (Just sevF) ->
              case msgSeverity of
                Just msev -> sevF > msev
                Nothing   -> False -- Impossible case

-- When all messages are filtered out, it is silent
hasNoMetrics :: forall a. MetaTrace a => TraceConfig -> Namespace a -> Bool
hasNoMetrics _tc _ns =
    let allNS = allNamespaces :: [Namespace a]
    in all (null . metricsDocFor) allNS

-- | Take a selector function called 'extract'.
-- Take a function from trace to trace with this config dependent value.
-- In this way construct a trace transformer with a config value
withNamespaceConfig :: forall m a b c. (MonadIO m, Ord b) =>
     String
  -> (TraceConfig -> Namespace a -> m b)
  -> (Maybe b -> Trace m c -> m (Trace m a))
  -> Trace m c
  -> m (Trace m a)
withNamespaceConfig name extract withConfig tr = do
    ref  <- liftIO (newIORef (Left (Map.empty, Nothing)))
    pure $ contramapM' (mapFunc ref)
  where
    mapFunc ref =
      \case
        (lc, Right a) -> do
          eitherConf <- liftIO $ readIORef ref
          case eitherConf of
            Right val -> do
              tt <- withConfig (Just val) tr
              T.traceWith (unpackTrace tt) (lc, Right a)
            Left (cmap, Just v) ->
              case Map.lookup (lcNSPrefix lc ++ lcNSInner lc) cmap of
                    Just val -> do
                      tt <- withConfig (Just val) tr
                      T.traceWith (unpackTrace tt) (lc, Right a)
                    Nothing  -> do
                      tt <- withConfig (Just v) tr
                      T.traceWith (unpackTrace tt) (lc, Right a)
            -- This can happen during reconfiguration, so we don't throw an error any more
            Left (_cmap, Nothing) -> pure ()
        (lc, Left TCReset) -> do
          liftIO $ writeIORef ref (Left (Map.empty, Nothing))
          tt <- withConfig Nothing tr
          T.traceWith (unpackTrace tt) (lc, Left TCReset)
        (lc, Left (TCConfig c)) -> do
          let nst = lcNSPrefix lc ++ lcNSInner lc
          !val <- extract c (Namespace (lcNSPrefix lc) (lcNSInner lc))
          eitherConf <- liftIO $ readIORef ref
          case eitherConf of
            Left (cmap, Nothing) ->
              case Map.lookup nst cmap of
                Nothing -> do
                  liftIO
                      $ writeIORef ref (Left (Map.insert nst val cmap, Nothing))
                  tt <- withConfig (Just val) tr
                  T.traceWith (unpackTrace tt) (lc, Left (TCConfig c))
                Just v  -> do
                  if v == val
                    then do
                      Trace tt <- withConfig (Just val) tr
                      T.traceWith tt (lc, Left (TCConfig c))
                    else error $ "Inconsistent trace configuration with context "
                                      ++ show nst
            Right _val -> error $ "Trace not reset before reconfiguration (1)"
                                ++ show nst
            Left (_cmap, Just _v) -> error $ "Trace not reset before reconfiguration (2)"
                                ++ show nst
        (lc, Left (TCOptimize cr)) -> do
          eitherConf <- liftIO $ readIORef ref
          let nst = lcNSPrefix lc ++ lcNSInner lc
          case eitherConf of
            Left (cmap, Nothing) ->
              case nub (Map.elems cmap) of
                []     -> pure ()
                [val]  -> do
                            liftIO $ writeIORef ref $ Right val
                            Trace tt <- withConfig (Just val) tr
                            T.traceWith tt (lc, Left (TCOptimize cr))
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
                            T.traceWith tt (lc, Left (TCOptimize cr))
            Right _val -> error $ "Trace not reset before reconfiguration (3)"
                                ++ show nst
            Left (_cmap, Just _v) ->
                          error $ "Trace not reset before reconfiguration (4)"
                                      ++ show nst
        (lc, Left dc@TCDocument {}) -> do
          eitherConf <- liftIO $ readIORef ref
          let nst = lcNSPrefix lc ++ lcNSInner lc
          case eitherConf of
            Right val -> do
              tt <- withConfig (Just val) tr
              T.traceWith
                (unpackTrace tt) (lc, Left dc)
            Left (cmap, Just v) ->
              case Map.lookup nst cmap of
                    Just val -> do
                      tt <- withConfig (Just val) tr
                      T.traceWith (unpackTrace tt) (lc, Left dc)
                    Nothing  -> do
                      tt <- withConfig (Just v) tr
                      T.traceWith (unpackTrace tt) (lc, Left dc)
            Left (_cmap, Nothing) -> error ("Missing configuration(2) " <> name <> " ns " <> show nst)


-- | Filter a trace by severity and take the filter value from the config
filterSeverityFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
filterSeverityFromConfig =
    withNamespaceConfig
      "severity"
      getSeverity'
      (\sev tr -> contramapMCond tr (mapF sev))
  where
    mapF confSev =
      \case
        (lc, Right cont) -> do
          let visible = case lcSeverity lc of
                            (Just s)  -> case confSev of
                                          Just (SeverityF (Just fs)) -> s >= fs
                                          Just (SeverityF Nothing)   -> False
                                          Nothing -> True
                            Nothing -> True
          if visible
            then pure $ Just (lc, Right cont)
            else pure Nothing
        (lc, Left c@TCDocument {}) -> do
          addFiltered c confSev
          pure (Just (lc, Left c))
        (lc, anx) ->  do
          pure (Just (lc, anx))


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
withBackendsFromConfig rappendPrefixNameAndFormatter =
  withNamespaceConfig
    "backends"
    getBackends'
    rappendPrefixNameAndFormatter
    (Trace T.nullTracer)

data Limiter m a = Limiter Text Double (Trace m a)

instance Eq (Limiter m a) where
  Limiter t1 _ _ == Limiter t2 _ _ = t1 == t2

instance Ord (Limiter m a) where
  Limiter t1 _ _ <= Limiter t2 _ _ = t1 <= t2

instance Show (Limiter m a) where
  show (Limiter name _ _) = "Limiter " <> unpack name


-- | Routing and formatting of a trace from the config
withLimitersFromConfig :: forall a m . (MonadUnliftIO m)
  => Trace m TraceDispatcherMessage
  -> Trace m a
  -> m (Trace m a)
withLimitersFromConfig tri tr = do
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
      -> Namespace a
      -> m (Maybe (Limiter m a))
    getLimiter stateRef config ns =
      case getLimiterSpec config ns of
        Nothing -> pure Nothing
        Just (name, frequency) ->
          if frequency == 0
            then pure Nothing
            else do
              state <- liftIO $ readIORef stateRef
              case Map.lookup name state of
                Just limiter -> pure $ Just limiter
                Nothing -> do
                  limiterTrace <- limitFrequency frequency name tri tr
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
      pure $ contramapM' (mapFunc (Limiter n d (Trace trli)) (Trace tr'))
    mapFunc (Limiter n d (Trace trli)) (Trace tr') =
        \case
          (lc, Right v) ->
            T.traceWith trli (lc, Right v)
          (lc, Left c@TCDocument {}) -> do
            addLimiter c (n, d)
            T.traceWith tr' (lc, Left c)
          (lc, Left c) ->
            T.traceWith tr' (lc, Left c)

--------------------------------------------------------

-- | If no severity can be found in the config, it is set to Warning
getSeverity :: TraceConfig -> Namespace a -> SeverityF
getSeverity config ns =
    fromMaybe (SeverityF (Just Warning))
              (getOption severitySelector config (nsGetComplete ns))
  where
    severitySelector :: ConfigOption -> Maybe SeverityF
    severitySelector (ConfSeverity s) = Just s
    severitySelector _              = Nothing

getSeverity' :: Applicative m => TraceConfig -> Namespace a -> m SeverityF
getSeverity' config ns = pure $ getSeverity config ns

-- | If no details can be found in the config, it is set to DNormal
getDetails :: TraceConfig -> Namespace a -> DetailLevel
getDetails config ns =
    fromMaybe DNormal (getOption detailSelector config (nsGetComplete ns))
  where
    detailSelector :: ConfigOption -> Maybe DetailLevel
    detailSelector (ConfDetail d) = Just d
    detailSelector _            = Nothing

getDetails' :: Applicative m => TraceConfig -> Namespace a -> m DetailLevel
getDetails' config n = pure $ getDetails config n

-- | If no backends can be found in the config, it is set to
-- [EKGBackend, Forwarder, Stdout HumanFormatColoured]
getBackends :: TraceConfig -> Namespace a -> [BackendConfig]
getBackends config ns =
    fromMaybe [EKGBackend, Forwarder, Stdout HumanFormatColoured]
      (getOption backendSelector config (nsGetComplete ns))
  where
    backendSelector :: ConfigOption -> Maybe [BackendConfig]
    backendSelector (ConfBackend s) = Just s
    backendSelector _             = Nothing

getBackends' :: Applicative m => TraceConfig -> Namespace a -> m [BackendConfig]
getBackends' config ns = pure $ getBackends config ns

-- | May return a limiter specification
getLimiterSpec :: TraceConfig -> Namespace a -> Maybe (Text, Double)
getLimiterSpec config ns = getOption limiterSelector config (nsGetComplete ns)
  where
    limiterSelector :: ConfigOption -> Maybe (Text, Double)
    limiterSelector (ConfLimiter f) = Just (intercalate "." (nsPrefix ns ++ nsInner ns), f)
    limiterSelector _               = Nothing

-- | Searches in the config to find an option
getOption :: (ConfigOption -> Maybe a) -> TraceConfig -> [Text] -> Maybe a
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
