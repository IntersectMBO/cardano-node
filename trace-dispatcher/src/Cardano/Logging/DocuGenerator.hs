{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.DocuGenerator where


import           Data.Text (Text)

import           Cardano.Logging.Trace
import           Cardano.Logging.Types
import qualified Control.Tracer as T


-- documentTracers :: Monad m => Trace m a -> Documented a -> m ()
-- documentTracers tracers documented = do
--     coll <- emptyCollector
--     mapM_ (docTrace (emptyCollector)) tracers
--   where
--     docTrace :: Monad m => DocCollector -> Trace m a -> m ()
--     docTrace docColl tr =
--       mapM
--         ((\ m -> T.traceWith tr (emptyLoggingContext, Just (Document docColl), m)) . fst)
--        documented

-- -- | Call this function at initialisation, and later for reconfiguration
-- configureTracers :: Monad m => TraceConfig -> Documented a -> [Trace m a]-> m ()
-- configureTracers config (Documented documented) tracers = do
--     mapM_ (configureTrace Reset) tracers
--     mapM_ (configureAllTrace (Config config)) tracers
--     mapM_ (configureTrace Optimize) tracers
--   where
--     configureTrace c (Trace tr) =
--       T.traceWith tr (emptyLoggingContext, Just c, fst (head documented))
--     configureAllTrace c (Trace tr) =
--       mapM
--         ((\ m -> T.traceWith tr (emptyLoggingContext, Just c, m)) . fst)
--         documented
