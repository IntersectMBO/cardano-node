module Cardano.TxSubmit.Util
  ( logException
  ) where

import           Cardano.Api (textShow)
import           Prelude

import           Cardano.BM.Trace (Trace, logError)
import           Control.Exception (SomeException, catch, throwIO)
import           Data.Text (Text)

-- | ouroboros-network catches 'SomeException' and if a 'nullTracer' is passed into that
-- code, the caught exception will not be logged. Therefore wrap all tx submission code that
-- is called from network with an exception logger so at least the exception will be
-- logged (instead of silently swallowed) and then rethrown.
logException :: Trace IO Text -> Text -> IO a -> IO a
logException tracer txt action = action `catch` logger
  where
    logger :: SomeException -> IO a
    logger e = do
      logError tracer $ txt <> textShow e
      throwIO e


