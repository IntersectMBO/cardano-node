{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This  module initializes a reforwarding service for use by
--   cardano-tracer.  It could [re-] serve the three miniprotocols on
--   a new local socket.  Currently,
--     - it reforwards trace messages to the new socket, optionally
--       filtering trace messages.
--     - it does not (currently) reforward EKG to the new socket.
--     - it creates a new Datapoint store, but the datapoint store is empty.

module Cardano.Tracer.Handlers.ReForwarder
  ( initReForwarder
  ) where

import           Cardano.Logging.Forwarding
import           Cardano.Logging.Trace
import           Cardano.Logging.Tracer.DataPoint
import qualified Cardano.Logging.Types as Log
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.MetaTrace
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (withIOManager)

import           Control.Monad (when)
import           Data.List (isPrefixOf)
import qualified Data.Text as Text

import           Trace.Forward.Utils.DataPoint
import           Trace.Forward.Utils.TraceObject (ForwardSink, writeToSink)

-- | Initialize the reforwarding service if configured to be active.
--   Returns
--    - the function by which logging sources report their log messages
--    - the DataPoint tracer (for the data point store associated with
--      the forwarding trace server).
initReForwarder :: TracerConfig
                -> Log.Trace IO TracerTrace
                -> IO ( [Log.TraceObject] -> IO ()
                      , Trace IO DataPoint
                      )
initReForwarder TracerConfig{networkMagic, hasForwarding}
                teTracer = do
  mForwarding <- case hasForwarding of
      Nothing -> pure Nothing
      Just x  -> case x of
        (ConnectTo{}, _, _) ->
          error "initReForwarder:  unsupported mode of operation:  ConnectTo.  Use AcceptAt."
        (AcceptAt (LocalSocket socket), mFwdNames, forwConf) -> do
          (fwdsink, dpStore :: DataPointStore) <- withIOManager $ \iomgr -> do
            traceWith teTracer TracerStartedReforwarder
            initForwarding iomgr forwConf
                                 (NetworkMagic networkMagic)
                                 Nothing
                                 (Just (socket, Log.Responder))
          pure $ Just ( filteredWriteToSink
                          (traceObjectHasPrefixIn mFwdNames)
                          fwdsink
                      , dataPointTracer @IO dpStore
                      )

  let traceDP = case mForwarding of
                  Just (_,tr) -> tr
                  Nothing     -> mempty

  let writesToSink' =
        case mForwarding of
          Just (writeToSink',_) ->
            mapM_ writeToSink'
          _ ->
            const $ return ()

  return (writesToSink', traceDP)


traceObjectHasPrefixIn :: Maybe [[Text.Text]] -> Log.TraceObject -> Bool
traceObjectHasPrefixIn mFwdNames logObj =
  case mFwdNames of
    Nothing       -> True -- forward everything in this case
    Just fwdNames -> any (`isPrefixOf` Log.toNamespace logObj) fwdNames


filteredWriteToSink :: (Log.TraceObject -> Bool)
                    -> ForwardSink Log.TraceObject
                    -> Log.TraceObject -> IO ()
filteredWriteToSink p fwdsink logObj =
  when (p logObj) $ writeToSink fwdsink logObj

