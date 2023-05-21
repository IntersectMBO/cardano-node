{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | This  module initializes a reforwarding service for use by
--   cardano-tracer.  It could [re-] serve the three miniprotocols on
--   a new local socket.  Currently,
--     - it creates a new Datapoint store: this has a single datapoint
--       empty but datapoints could be added here.
--     - it reforwards trace messages to the new socket, optionally
--       filtering trace messages.
--     - it does not (currently) reforward EKG to the new socket.

module Cardano.Tracer.Handlers.ReForwarder
  ( initReForwarder
  ) where

import           Control.Monad(when)
import           Data.Aeson
import           Data.List (isPrefixOf)
import           Data.Functor.Contravariant
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           GHC.Generics
import           Network.HostName (HostName)

import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (withIOManager)

import           Cardano.Logging.Forwarding
import           Cardano.Logging.Trace
import           Cardano.Logging.Tracer.DataPoint
import           Cardano.Logging.Types qualified as Log
import           Trace.Forward.Utils.DataPoint
import           Trace.Forward.Utils.TraceObject (writeToSink,ForwardSink)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.MetaTrace

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
initReForwarder TracerConfig{networkMagic, hasForwarding, hasCnsaSink}
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
          pure $ Just ( filteredWriteToSink fwdsink mFwdNames
                      , dataPointTracer @IO dpStore
                      )
  
  let traceDP = case mForwarding of
                  Just (_,tr) -> tr
                  Nothing     -> mempty
                 
  modeDP :: Trace IO ReforwarderMode
    <- mkDataPointTracer traceDP
  traceWith modeDP $ RM "running"
    -- Note: currently the only trace for this datapoint

  cnsaSinkTracer :: Trace IO Log.TraceObject
    <- case hasCnsaSink of
         Just True -> mkCnsaSink traceDP
         _         -> return mempty

  let writesToSink' =
        case mForwarding of
          Just (writeToSink',_) ->
            mapM_ $ \os-> do
                       writeToSink' os
                       traceWith cnsaSinkTracer os
          _ ->
            const $ return ()
    
  return (writesToSink', traceDP)

filteredWriteToSink :: ForwardSink Log.TraceObject
                    -> Maybe [[Text.Text]]
                    -> Log.TraceObject -> IO ()
filteredWriteToSink fwdsink mFwdNames =
  case mFwdNames of
    Nothing ->
       writeToSink fwdsink
       
    Just fwdNames ->
       \logObj->
         when (any (`isPrefixOf` Log.toNamespace logObj) fwdNames) $
           writeToSink fwdsink logObj

------------------------------------------------------------------------------
-- ReforwarderMode datapoint: type and boilerplate
--

-- | Mode of the reforwarder
newtype ReforwarderMode = RM String
                          deriving (Eq,Ord,Read,Show,Generic)
 
deriving instance ToJSON ReforwarderMode

-- | give the 'ReforwarderMode' type a place in the Datapoint Namespace:
instance Log.MetaTrace ReforwarderMode
  where
  namespaceFor _  = Log.Namespace [] ["Reforwarder","Mode"]
  severityFor _ _ = Just Info
  documentFor _   = Just "the mode of the reforwarder"
  allNamespaces   = [ Log.namespaceFor (undefined :: ReforwarderMode) ]

------------------------------------------------------------------------------
-- Code for CNSA Sink Analysis
--

mkCnsaSink :: Trace IO DataPoint -> IO (Trace IO Log.TraceObject)
mkCnsaSink traceDP =
  do
  bfccbfDP :: Trace IO CompletedBlockFetchTimes
    <- mkDataPointTracer traceDP

  let hostTimesTr :: Trace IO HostTimes
      hostTimesTr = contramap CompletedBlockFetchTimes bfccbfDP
  
  mkLastHostTimeOf
    ["BlockFetch","Client","CompletedBlockFetch"]
    hostTimesTr

mkLastHostTimeOf :: [Text.Text]
                 -> Trace IO HostTimes
                 -> IO (Trace IO Log.TraceObject)
mkLastHostTimeOf ns tr =
  do
  traceWith tr Map.empty  -- Cause this datapoint to immediately have a value.
  foldTraceM compute Map.empty (contramap Log.unfold tr)

  where
  compute m _c to' =
    if ns == Log.toNamespace to' then
      Map.insert (Log.toHostname to') (Log.toTimestamp to') m
    else
      m

------------------------------------------------------------------------------
-- CompletedBlockFetchTimes datapoint: type and boilerplate
--

-- | map 'HostName' to a 'UTCTime'
type HostTimes = Map.Map Network.HostName.HostName UTCTime

-- Create a datapoint

-- | CompletedBlockFetchTimes - for tracking ["BlockFetch","Client","CompletedBlockFetch"] times
newtype CompletedBlockFetchTimes = CompletedBlockFetchTimes HostTimes
                                   deriving (Eq,Ord,Read,Show,Generic)
deriving instance ToJSON CompletedBlockFetchTimes

instance Log.MetaTrace CompletedBlockFetchTimes
  where
  namespaceFor _  = Log.Namespace [] ["BlockFetch","LastCompletedTimes"]
  severityFor _ _ = Just Info
  documentFor _   = Just "map of most recent CompletedBlockFetch times for all connected nodes"
  allNamespaces   = [Log.namespaceFor (undefined :: CompletedBlockFetchTimes)]


