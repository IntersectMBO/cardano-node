{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.ConsensusTracer.Docu
  (  docChainSyncClientEvent
  ) where

import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Consensus.Block
import           Ouroboros.Network.Block
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client


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
