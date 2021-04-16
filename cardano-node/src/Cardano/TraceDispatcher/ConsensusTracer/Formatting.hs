{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.ConsensusTracer.Formatting
  (
  ) where

import           Data.Aeson (Value (String), toJSON, (.=))
import qualified Data.Text as Text
import           Text.Show


import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import           Cardano.TraceDispatcher.OrphanInstances.Byron ()
import           Cardano.TraceDispatcher.OrphanInstances.Consensus ()
import           Cardano.TraceDispatcher.OrphanInstances.Network ()
import           Cardano.TraceDispatcher.OrphanInstances.Shelley ()
import           Cardano.TraceDispatcher.Render

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server

import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound


instance (Show (Header blk), ConvertRawHash blk, LedgerSupportsProtocol blk)
      => LogFormatting (TraceChainSyncClientEvent blk) where
  forHuman (TraceDownloadedHeader pt) =
    "While following a candidate chain, we rolled forward by downloading a\
    \ header. " <> showT (headerPoint pt)
  forHuman (TraceRolledBack tip) =
    "While following a candidate chain, we rolled back to the given point: "
      <> showT tip
  forHuman (TraceException exc) =
    "An exception was thrown by the Chain Sync Client. "
      <> showT exc
  forHuman (TraceFoundIntersection _ _ _) =
      "We found an intersection between our chain fragment and the\
      \ candidate's chain."
  forHuman (TraceTermination res) =
      "The client has terminated. " <> showT res

  forMachine dtal (TraceDownloadedHeader pt) =
      mkObject [ "kind" .= String "DownloadedHeader"
               , "block" .= forMachine dtal (headerPoint pt) ]
  forMachine dtal (TraceRolledBack tip) =
      mkObject [ "kind" .= String "RolledBack"
               , "tip" .= forMachine dtal tip ]
  forMachine _dtal (TraceException exc) =
      mkObject [ "kind" .= String "Exception"
               , "exception" .= String (Text.pack $ show exc) ]
  forMachine _dtal (TraceFoundIntersection _ _ _) =
      mkObject [ "kind" .= String "FoundIntersection" ]
  forMachine _dtal (TraceTermination _) =
      mkObject [ "kind" .= String "Termination" ]


instance ConvertRawHash blk
      => LogFormatting (TraceChainSyncServerEvent blk) where
  forMachine dtal (TraceChainSyncServerRead tip (AddBlock hdr)) =
      mkObject [ "kind" .= String "ChainSyncServerRead.AddBlock"
               , "tip" .= String (renderTipForDetails dtal tip)
               , "addedBlock" .= String (renderPointForDetails dtal hdr)
               ]
  forMachine dtal (TraceChainSyncServerRead tip (RollBack pt)) =
      mkObject [ "kind" .= String "ChainSyncServerRead.RollBack"
               , "tip" .= String (renderTipForDetails dtal tip)
               , "rolledBackBlock" .= String (renderPointForDetails dtal pt)
               ]
  forMachine dtal (TraceChainSyncServerReadBlocked tip (AddBlock hdr)) =
      mkObject [ "kind" .= String "ChainSyncServerReadBlocked.RollForward"
               , "tip" .= String (renderTipForDetails dtal tip)
               , "addedBlock" .= String (renderPointForDetails dtal hdr)
               ]
  forMachine dtal (TraceChainSyncServerReadBlocked tip (RollBack pt)) =
      mkObject [ "kind" .= String "ChainSyncServerReadBlocked.RollBack"
               , "tip" .= String (renderTipForDetails dtal tip)
               , "rolledBackBlock" .= String (renderPointForDetails dtal pt)
               ]
  forMachine dtal (TraceChainSyncRollForward point) =
      mkObject [ "kind" .= String "ChainSyncRollForward"
               , "point" .= forMachine dtal point
               ]
  forMachine dtal (TraceChainSyncRollBackward point) =
      mkObject [ "kind" .= String "ChainSyncRollBackward"
               , "point" .= forMachine dtal point
               ]

instance (Show peer, LogFormatting a) => LogFormatting (TraceLabelPeer peer a) where
  forMachine dtal (TraceLabelPeer peerid a) =
    mkObject [ "peer" .= show peerid ] <> forMachine dtal a

instance Show peer
      => LogFormatting [TraceLabelPeer peer (FetchDecision [Point header])] where
  forMachine DBrief _ = emptyObject
  forMachine _ [] = emptyObject
  forMachine _ xs = mkObject
    [ "kind"  .= String "PeersFetch"
    , "peers" .= toJSON
      (foldl' (\acc x -> forMachine DDetailed x : acc) [] xs) ]

instance LogFormatting (FetchDecision [Point header]) where
  forMachine _dtal (Left decline) =
    mkObject [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (showT decline)
             ]
  forMachine _dtal (Right results) =
    mkObject [ "kind" .= String "FetchDecision results"
             , "length" .= String (showT $ length results)
             ]

instance LogFormatting (BlockFetch.TraceFetchClientState header) where
  forMachine _dtal BlockFetch.AddedFetchRequest {} =
    mkObject [ "kind" .= String "AddedFetchRequest" ]
  forMachine _dtal BlockFetch.AcknowledgedFetchRequest {} =
    mkObject [ "kind" .= String "AcknowledgedFetchRequest" ]
  forMachine _dtal BlockFetch.CompletedBlockFetch {} =
    mkObject [ "kind" .= String "CompletedBlockFetch" ]
  forMachine _dtal BlockFetch.CompletedFetchBatch {} =
    mkObject [ "kind" .= String "CompletedFetchBatch" ]
  forMachine _dtal BlockFetch.StartedFetchBatch {} =
    mkObject [ "kind" .= String "StartedFetchBatch" ]
  forMachine _dtal BlockFetch.RejectedFetchBatch {} =
    mkObject [ "kind" .= String "RejectedFetchBatch" ]
  forMachine _dtal BlockFetch.ClientTerminating {} =
    mkObject [ "kind" .= String "ClientTerminating" ]

instance LogFormatting (TraceBlockFetchServerEvent blk) where
  forMachine _dtal _ =
    mkObject [ "kind" .= String "BlockFetchServer" ]

-- TODO
--
-- instance LogFormatting a => LogFormatting (Consensus.TraceLabelCreds a) where
--   forMachine dtal (Consensus.TraceLabelCreds creds val) =
--     mkObject [ "credentials" .= toJSON creds
--              , "val"         .= forMachine dtal val
--             ]

instance LogFormatting (TraceTxSubmissionInbound txid tx) where
  forMachine _dtal (TraceTxSubmissionCollected count) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionCollected"
      , "count" .= toJSON count
      ]
  forMachine _dtal (TraceTxSubmissionProcessed processed) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionProcessed"
      , "accepted" .= toJSON (ptxcAccepted processed)
      , "rejected" .= toJSON (ptxcRejected processed)
      ]
  forMachine _dtal TraceTxInboundTerminated =
    mkObject
      [ "kind" .= String "TraceTxInboundTerminated"
      ]
  forMachine _dtal (TraceTxInboundCanRequestMoreTxs count) =
    mkObject
      [ "kind" .= String "TraceTxInboundCanRequestMoreTxs"
      , "count" .= toJSON count
      ]
  forMachine _dtal (TraceTxInboundCannotRequestMoreTxs count) =
    mkObject
      [ "kind" .= String "TraceTxInboundCannotRequestMoreTxs"
      , "count" .= toJSON count
      ]

instance (Show txid, Show tx)
      => LogFormatting (TraceTxSubmissionOutbound txid tx) where
  forMachine DDetailed (TraceTxSubmissionOutboundRecvMsgRequestTxs txids) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      , "txIds" .= String (Text.pack $ show txids)
      ]
  forMachine _dtal (TraceTxSubmissionOutboundRecvMsgRequestTxs _txids) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      ]
  forMachine DDetailed (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      , "txs" .= String (Text.pack $ show txs)
      ]
  forMachine _dtal (TraceTxSubmissionOutboundSendMsgReplyTxs _txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      ]
  forMachine _dtal (TraceControlMessage _msg) =
    mkObject
      [ "kind" .= String "TraceControlMessage"
      ]
