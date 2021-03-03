{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.DocuGenerator where


import           Data.Text (Text)

import           Cardano.Logging.Trace
import           Cardano.Logging.Types
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.IORef (IORef, modifyIORef, newIORef, writeIORef)
import qualified Data.Map as Map

documentTracers :: MonadIO m => Documented a -> [Trace m a] -> m DocCollector
documentTracers (Documented documented) tracers = do
    let docIdx = zip documented [1..]
    coll <- fmap DocCollector (liftIO $ newIORef Map.empty)
    mapM_ (docTrace docIdx coll) tracers
    pure coll
  where
    docTrace docIdx docColl (Trace tr) =
      mapM_
        (\ ((m,mdText), idx) -> do
          T.traceWith tr (emptyLoggingContext, Just (Document idx mdText docColl), m))
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
