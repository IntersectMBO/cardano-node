{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.ConsensusTracer.Docu
  ( docChainSyncClientEvent
  , docChainSyncServerEvent
  , docBlockFetchDecision
  ) where

import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer(..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Network.Block
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Network.BlockFetch.Decision(FetchDecision)

import           Cardano.TraceDispatcher.OrphanInstances.Byron ()
import           Cardano.TraceDispatcher.OrphanInstances.Consensus ()
import           Cardano.TraceDispatcher.OrphanInstances.Network ()
import           Cardano.TraceDispatcher.OrphanInstances.Shelley ()

docHeader :: Header blk
docHeader = undefined

docPoint :: Point blk
docPoint = undefined

docOurTipBlock :: Our (Tip blk)
docOurTipBlock = undefined

docTheirTipBlock :: Their (Tip blk)
docTheirTipBlock = undefined

docChainSyncClientException :: ChainSyncClientException
docChainSyncClientException = undefined

docChainSyncClientResult :: ChainSyncClientResult
docChainSyncClientResult = undefined

docChainUpdate :: ChainUpdate block a
docChainUpdate = undefined

docTip :: Tip blk
docTip = undefined

docpeer :: remotePeer
docpeer = undefined

docFetchDecline :: FetchDecision [Point (Header blk)]
docFetchDecline = Left undefined

docFetchResult :: FetchDecision [Point (Header blk)]
docFetchResult = Right undefined

--------------------

docChainSyncClientEvent :: Documented (TraceChainSyncClientEvent blk)
docChainSyncClientEvent = Documented [
    DocMsg
      (TraceDownloadedHeader docHeader)
      []
      "While following a candidate chain, we rolled forward by downloading a\
      \ header."
  , DocMsg
      (TraceRolledBack docPoint)
      []
      "While following a candidate chain, we rolled back to the given point."
  , DocMsg
      (TraceFoundIntersection docPoint docOurTipBlock docTheirTipBlock)
      []
      "We found an intersection between our chain fragment and the\
      \ candidate's chain."
  , DocMsg
      (TraceException docChainSyncClientException)
      []
      "An exception was thrown by the Chain Sync Client."
  , DocMsg
      (TraceTermination docChainSyncClientResult)
      []
      "The client has terminated."
  ]

docChainSyncServerEvent :: Documented (TraceChainSyncServerEvent blk)
docChainSyncServerEvent = Documented [
    DocMsg
      (TraceChainSyncServerRead docTip docChainUpdate)
      []
      "TODO"
    , DocMsg
      (TraceChainSyncServerReadBlocked docTip docChainUpdate)
      []
      "TODO"
    , DocMsg
      (TraceChainSyncRollForward docPoint)
      []
      "TODO"
    , DocMsg
      (TraceChainSyncRollBackward docPoint)
      []
      "TODO"
  ]

docBlockFetchDecision ::
  Documented ([TraceLabelPeer remotePeer (FetchDecision [Point (Header blk)])])
docBlockFetchDecision = Documented [
    DocMsg
      [TraceLabelPeer docpeer docFetchDecline]
      []
      "TODO"
    , DocMsg
      [TraceLabelPeer docpeer docFetchResult]
      []
      "TODO"
  ]
