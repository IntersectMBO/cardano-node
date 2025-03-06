{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: pull in negotiated selection
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

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
  Trace $ T.arrow $ T.emit $ uncurry output
 where

  output ::
       LoggingContext
    -> Either TraceControl FormattedMessage
    -> m ()
  output loggingContext = \case
    Right (FormattedForwarder lo) -> liftIO $ writeToSink forwardSink lo
    Left c@(TCDocument{})         -> docIt Forwarder (loggingContext, Left c)

    -- ignored:
    -- * any other formatted message than the one formatted for forwarding
    -- * any other in-band control message except TCDocument
    _                             -> pure ()

{-
  writeSelection lo = case TraceSelectAll of
    TraceSelectMachine -> writeToSink forwardSink lo{toHuman = Nothing}
    TraceSelectAll     -> writeToSink forwardSink lo
    TraceSelectHuman   -> writeToSink forwardSink lo{toMachine = ""}
    TraceSelectNone    -> pure ()
-}