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
import qualified Data.Text as Text
import           GHC.Generics

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

  let writesToSink' =
        case mForwarding of
          Just (writeToSink',_) ->
            mapM_ writeToSink'
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


