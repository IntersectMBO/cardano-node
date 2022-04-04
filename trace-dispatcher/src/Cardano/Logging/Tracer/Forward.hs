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

---------------------------------------------------------------------------

forwardTracer :: forall m. (MonadIO m)
  => ForwardSink TraceObject
  -> Trace m FormattedMessage
forwardTracer forwardSink =
  Trace $ T.arrow $ T.emit $ uncurry (output forwardSink)
 where
  output ::
       ForwardSink TraceObject
    -> LoggingContext
    -> Either TraceControl FormattedMessage
    -> m ()
  output sink LoggingContext {} (Right (FormattedForwarder lo)) = liftIO $
    writeToSink sink lo
  output _sink LoggingContext {} (Left Reset) = liftIO $ do
    pure ()
  output _sink lk (Left c@Document {}) =
    docIt Forwarder (lk, Left c)
  output _sink LoggingContext {} _  = pure ()
