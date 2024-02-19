{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Forward
  (
    forwardTracer
  ) where

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types

import           Control.Monad.IO.Class
import qualified Control.Tracer as T

import           Trace.Forward.Utils.TraceObject (ForwardSink, writeToSink)


---------------------------------------------------------------------------

-- | It is mandatory to construct only one forwardTracer tracer in any application!
-- Throwing away a forwardTracer tracer and using a new one will result in an exception
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
  output _sink LoggingContext {} (Left TCReset) = liftIO $ do
    pure ()
  output _sink lk (Left c@TCDocument {}) =
    docIt Forwarder (lk, Left c)
  output _sink LoggingContext {} (Right _)  = pure ()
  output _sink LoggingContext {} _  = pure ()
