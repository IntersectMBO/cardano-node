{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.DocuGenerator where


import           Data.Text (Text)

import           Cardano.Logging.Trace
import           Cardano.Logging.Types


documentTracers :: Monad m => Trace m a -> Documented a -> m ()
documentTracers tracers documented =
  mapM_ (docTrace (Document (emptyCollector documented))) tracers
  where
    docTrace :: Monad m => DocCollector a -> Trace m a -> m ()
    docTrace c tr = T.traceWith tr (emptyLoggingContext, Left c)
