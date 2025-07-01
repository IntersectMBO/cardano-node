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


---------------------------------------------------------------------------

-- | It is mandatory to construct only one forwardTracer tracer in any application!
-- Throwing away a forwardTracer tracer and using a new one will result in an exception
forwardTracer :: forall m. (MonadIO m)
  => (TraceObject -> IO ())
  -> Trace m FormattedMessage
forwardTracer write =
  Trace $ T.arrow $ T.emit $ uncurry output
 where
  output ::
       LoggingContext
    -> Either TraceControl FormattedMessage
    -> m ()
  output LoggingContext {} (Right (FormattedForwarder lo)) = liftIO $
    write lo
  output LoggingContext {} (Left TCReset) = liftIO $ do
    pure ()
  output lk (Left c@TCDocument {}) =
    docIt Forwarder (lk, Left c)
  output LoggingContext {} (Right _)  = pure ()
  output LoggingContext {} _  = pure ()
