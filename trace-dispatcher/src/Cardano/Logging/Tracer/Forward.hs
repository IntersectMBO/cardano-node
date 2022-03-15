{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Forward
  (
    forwardTracer
  ) where

import           Control.Monad.IO.Class

import qualified Control.Tracer as T
import           Trace.Forward.Utils.TraceObject (ForwardSink, writeToSink)

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import           Cardano.Logging.Utils (uncurry3)

---------------------------------------------------------------------------

forwardTracer :: forall m. (MonadIO m)
  => ForwardSink TraceObject
  -> Trace m FormattedMessage
forwardTracer forwardSink =
  Trace $ T.arrow $ T.emit $ uncurry3 (output forwardSink)
 where
  output ::
       ForwardSink TraceObject
    -> LoggingContext
    -> Maybe TraceControl
    -> FormattedMessage
    -> m ()
  output sink LoggingContext {} Nothing (FormattedForwarder lo) = liftIO $
    writeToSink sink lo
  output _sink LoggingContext {} (Just Reset) _msg = liftIO $ do
    pure ()
  output _sink lk (Just c@Document {}) (FormattedForwarder lo) = do
    docIt Forwarder (FormattedForwarder lo) (lk, Just c, lo)
  output _sink LoggingContext {} _ _a = pure ()
