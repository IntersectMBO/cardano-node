module Cardano.TxSubmit.Util
  ( logException
  ) where

import           Cardano.Logging (Trace, traceWith)
import           Cardano.TxSubmit.Tracing.TraceSubmitApi (TraceSubmitApi (..))

import           Prelude

import           Control.Exception.Safe (SomeException, catch, throwIO)
import           Data.Text (Text)

-- | ouroboros-network catches 'SomeException' and if a 'nullTracer' is passed into that
-- code, the caught exception will not be logged. Therefore wrap all tx submission code that
-- is called from network with an exception logger so at least the exception will be
-- logged (instead of silently swallowed) and then rethrown.
logException :: Trace IO TraceSubmitApi -> Text -> IO a -> IO a
logException tracer txt action = action `catch` logger
  where
    logger :: SomeException -> IO a
    logger e = do
      traceWith tracer (EndpointException txt e)
      throwIO e


