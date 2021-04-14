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

import           Data.Aeson (Value (String), (.=), toJSON)
import qualified Data.Text as Text
import           Text.Show

import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import           Cardano.TraceDispatcher.OrphanInstances.Byron ()
import           Cardano.TraceDispatcher.OrphanInstances.Consensus ()
import           Cardano.TraceDispatcher.OrphanInstances.Network ()
import           Cardano.TraceDispatcher.OrphanInstances.Shelley ()
import           Cardano.TraceDispatcher.Render

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.Decision


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
