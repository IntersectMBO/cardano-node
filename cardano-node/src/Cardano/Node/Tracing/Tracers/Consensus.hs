{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.Consensus
  (
    TraceStartLeadershipCheckPlus (..)
  , ForgeTracerType
  , forgeTracerTransform
  , initialClientMetrics
  , calculateBlockFetchClientMetrics
  , ClientMetrics
  ) where


import           Control.Monad.Class.MonadTime (Time (..))
import           Data.Aeson (ToJSON, Value (Number, String), toJSON, (.=))
import qualified Data.Aeson as Aeson
import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as Pq
import           Data.SOP.Strict
import qualified Data.Text as Text
import           Data.Time (DiffTime, NominalDiffTime)
import           Text.Show


import           Cardano.Slotting.Slot (WithOrigin (..))

import           Cardano.Logging
import           Cardano.Node.Queries (HasKESInfo (..))
import           Cardano.Node.Tracing.Era.Byron ()
import           Cardano.Node.Tracing.Era.Shelley ()
import           Cardano.Node.Tracing.Formatting ()
import           Cardano.Node.Tracing.Render
import           Cardano.Node.Tracing.Tracers.ConsensusStartupException ()
import           Cardano.Node.Tracing.Tracers.StartLeadershipCheck
import           Cardano.Prelude hiding (All, Show, show)

import           Cardano.Protocol.TPraos.OCert (KESPeriod (..))

import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block hiding (blockPrevHash)
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.TxSubmission.Inbound hiding (txId)
import           Ouroboros.Network.TxSubmission.Outbound


import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent (..), LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTxId, HasTxId,
                   LedgerSupportsMempool, txForgetValidated, txId)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API (MempoolSize (..), TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                   (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                   (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints, estimateBlockSize)
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Util.Enclose


instance Show adr => LogFormatting (ConnectionId adr) where
  forMachine _dtal (ConnectionId local' remote) =
    mconcat [ "connectionId" .= String ((Text.pack . show) local'
                                          <> " "
                                          <> (Text.pack . show) remote)
    ]
--------------------------------------------------------------------------------
--   TraceLabelCreds peer a
--------------------------------------------------------------------------------

instance LogFormatting a => LogFormatting (TraceLabelCreds a) where
  forMachine dtal (TraceLabelCreds creds a)  =
    mconcat $ ("credentials" .= toJSON creds) : [forMachine dtal a]

  forHuman (TraceLabelCreds creds a)         =
    "With label " <> (Text.pack . show) creds <> ", " <> forHuman a
  asMetrics (TraceLabelCreds _creds a)        =
    asMetrics a

instance MetaTrace a => MetaTrace (TraceLabelCreds a) where
  namespaceFor (TraceLabelCreds _label obj) = (nsCast . namespaceFor) obj
  severityFor ns Nothing = severityFor (nsCast ns :: Namespace a) Nothing
  severityFor ns (Just (TraceLabelCreds _label obj)) =
    severityFor (nsCast ns :: Namespace a) (Just obj)
  privacyFor ns Nothing = privacyFor (nsCast ns :: Namespace a) Nothing
  privacyFor ns (Just (TraceLabelCreds _label obj)) =
    privacyFor (nsCast ns :: Namespace a) (Just obj)
  detailsFor ns Nothing = detailsFor (nsCast ns :: Namespace a) Nothing
  detailsFor ns (Just (TraceLabelCreds _label obj)) =
    detailsFor (nsCast ns :: Namespace a) (Just obj)
  documentFor ns = documentFor (nsCast ns :: Namespace a)
  metricsDocFor ns = metricsDocFor (nsCast ns :: Namespace a)
  allNamespaces = map nsCast (allNamespaces :: [Namespace a])


--------------------------------------------------------------------------------
--   TraceLabelPeer peer a
--------------------------------------------------------------------------------

instance (LogFormatting peer, Show peer, LogFormatting a)
  => LogFormatting (TraceLabelPeer peer a) where
  forMachine dtal (TraceLabelPeer peerid a) =
    mconcat [ "peer" .= forMachine dtal peerid ] <> forMachine dtal a
  forHuman (TraceLabelPeer peerid a) = "Peer is " <> showT peerid
                                        <> ". " <> forHuman a
  asMetrics (TraceLabelPeer _peerid a) = asMetrics a

instance MetaTrace a => MetaTrace (TraceLabelPeer label a) where
  namespaceFor (TraceLabelPeer _label obj) = (nsCast . namespaceFor) obj
  severityFor ns Nothing = severityFor (nsCast ns :: Namespace a) Nothing
  severityFor ns (Just (TraceLabelPeer _label obj)) =
    severityFor (nsCast ns) (Just obj)
  privacyFor ns Nothing = privacyFor (nsCast ns :: Namespace a) Nothing
  privacyFor ns (Just (TraceLabelPeer _label obj)) =
    privacyFor (nsCast ns) (Just obj)
  detailsFor ns Nothing = detailsFor (nsCast ns :: Namespace a) Nothing
  detailsFor ns (Just (TraceLabelPeer _label obj)) =
    detailsFor (nsCast ns) (Just obj)
  documentFor ns = documentFor (nsCast ns :: Namespace a)
  metricsDocFor ns = metricsDocFor (nsCast ns :: Namespace a)
  allNamespaces = map nsCast (allNamespaces :: [Namespace a])

instance (LogFormatting (LedgerUpdate blk), LogFormatting (LedgerWarning blk))
      =>  LogFormatting (LedgerEvent blk) where
  forMachine dtal = \case
    LedgerUpdate  update  -> forMachine dtal update
    LedgerWarning warning -> forMachine dtal warning


--------------------------------------------------------------------------------
-- ChainSyncClient Tracer
--------------------------------------------------------------------------------

instance (ConvertRawHash blk, LedgerSupportsProtocol blk)
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
  forHuman TraceFoundIntersection {} =
      "We found an intersection between our chain fragment and the\
      \ candidate's chain."
  forHuman (TraceTermination res) =
      "The client has terminated. " <> showT res

  forMachine _dtal (TraceDownloadedHeader h) =
      mconcat [ "kind" .= String "DownloadedHeader"
              , tipToObject (tipFromHeader h)
              ]
  forMachine dtal (TraceRolledBack tip) =
      mconcat [ "kind" .= String "RolledBack"
               , "tip" .= forMachine dtal tip ]
  forMachine _dtal (TraceException exc) =
      mconcat [ "kind" .= String "Exception"
               , "exception" .= String (Text.pack $ show exc) ]
  forMachine _dtal TraceFoundIntersection {} =
      mconcat [ "kind" .= String "FoundIntersection" ]
  forMachine _dtal (TraceTermination reason) =
      mconcat [ "kind" .= String "Termination"
               , "reason" .= String (Text.pack $ show reason) ]

tipToObject :: forall blk. ConvertRawHash blk => Tip blk -> Aeson.Object
tipToObject = \case
  TipGenesis -> mconcat
    [ "slot"    .= toJSON (0 :: Int)
    , "block"   .= String "genesis"
    , "blockNo" .= toJSON ((-1) :: Int)
    ]
  Tip slot hash blockno -> mconcat
    [ "slot"    .= slot
    , "block"   .= String (renderHeaderHash (Proxy @blk) hash)
    , "blockNo" .= blockno
    ]

instance MetaTrace (TraceChainSyncClientEvent blk) where
  namespaceFor TraceDownloadedHeader {} = Namespace [] ["DownloadedHeader"]
  namespaceFor TraceRolledBack {} = Namespace [] ["RolledBack"]
  namespaceFor TraceException {} = Namespace [] ["Exception"]
  namespaceFor TraceFoundIntersection {} = Namespace [] ["FoundIntersection"]
  namespaceFor TraceTermination {} = Namespace [] ["Termination"]

  severityFor (Namespace _ ["DownloadedHeader"]) _ = Just Info
  severityFor (Namespace _ ["RolledBack"]) _ = Just Notice
  severityFor (Namespace _ ["Exception"]) _ = Just Warning
  severityFor (Namespace _ ["FoundIntersection"]) _ = Just Info
  severityFor (Namespace _ ["Termination"]) _ = Just Notice
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["DownloadedHeader"]) = Just
    "While following a candidate chain, we rolled forward by downloading a\
    \ header."
  documentFor (Namespace _ ["RolledBack"]) = Just
    "While following a candidate chain, we rolled back to the given point."
  documentFor (Namespace _ ["Exception"]) = Just
    "An exception was thrown by the Chain Sync Client."
  documentFor (Namespace _ ["FoundIntersection"]) = Just
    "We found an intersection between our chain fragment and the\
    \ candidate's chain."
  documentFor (Namespace _ ["Termination"]) = Just
    "The client has terminated."
  documentFor _ = Nothing

  allNamespaces =
    [
      Namespace [] ["DownloadedHeader"]
    , Namespace [] ["RolledBack"]
    , Namespace [] ["Exception"]
    , Namespace [] ["FoundIntersection"]
    , Namespace [] ["Termination"]
    ]

--------------------------------------------------------------------------------
-- ChainSyncServer Tracer
--------------------------------------------------------------------------------

instance ConvertRawHash blk
      => LogFormatting (TraceChainSyncServerEvent blk) where
  forMachine dtal (TraceChainSyncServerUpdate tip update blocking enclosing) =
      mconcat $
               [ "kind" .= String "ChainSyncServer.Update"
               , "tip" .= tipToObject tip
               , case update of
                   AddBlock pt -> "addBlock" .= renderPointForDetails dtal pt
                   RollBack pt -> "rollBackTo" .= renderPointForDetails dtal pt
               , "blockingRead" .= case blocking of Blocking -> True; NonBlocking -> False
               ]
               <> [ "risingEdge" .= True | RisingEdge <- [enclosing] ]

  asMetrics (TraceChainSyncServerUpdate _tip (AddBlock _pt) _blocking FallingEdge) =
      [CounterM "ChainSync.HeadersServed.Falling" Nothing]
  asMetrics _ = []

instance MetaTrace (TraceChainSyncServerEvent blk) where
  namespaceFor TraceChainSyncServerUpdate {} = Namespace [] ["Update"]

  severityFor (Namespace _ ["Update"])
              (Just (TraceChainSyncServerUpdate _tip _upd _blocking enclosing)) =
                case enclosing of
                  RisingEdge  -> Just Info
                  FallingEdge -> Just Debug
  severityFor (Namespace _ ["Update"]) Nothing = Just Info
  severityFor _ _ = Nothing

  metricsDocFor (Namespace _ ["Update"]) =
    [ ("ChainSync.HeadersServed", "A counter triggered on any header event")
    , ("ChainSync.HeadersServed.Falling",
        "A counter triggered only on header event with falling edge")]
  metricsDocFor _ = []

  documentFor (Namespace _ ["Update"]) = Just
    "A server read has occurred, either for an add block or a rollback"
  documentFor _ = Nothing

  allNamespaces = [Namespace [] ["Update"]]

--------------------------------------------------------------------------------
-- BlockFetchClient Metrics
--------------------------------------------------------------------------------

data CdfCounter = CdfCounter {
    limit :: Int64
  , counter :: Int64
}

decCdf :: Ord a => Num a => a -> CdfCounter -> CdfCounter
decCdf v cdf =
  if v < fromIntegral (limit cdf)
    then cdf {counter = counter cdf - 1}
    else cdf

incCdf ::Ord a => Num a => a -> CdfCounter -> CdfCounter
incCdf v cdf =
  if v < fromIntegral (limit cdf)
    then cdf {counter = counter cdf + 1}
    else cdf

data ClientMetrics = ClientMetrics {
    cmSlotMap  :: IntPSQ Word64 NominalDiffTime
  , cmCdf1sVar :: CdfCounter
  , cmCdf3sVar :: CdfCounter
  , cmCdf5sVar :: CdfCounter
  , cmDelay    :: Double
  , cmBlockSize :: Word32
  , cmTraceIt  :: Bool
}

instance LogFormatting ClientMetrics where
  forMachine _dtal _ = mempty
  asMetrics ClientMetrics {..} =
    if cmTraceIt
      then
        let  size = Pq.size cmSlotMap
             msgs =
               [ DoubleM
                    "Blockfetch.Client.Blockdelay"
                    cmDelay
               , IntM
                    "Blockfetch.Client.Blocksize"
                    (fromIntegral cmBlockSize)
               , DoubleM "Blockfetch.Client.Blockdelay.cdfOne"
                    (fromIntegral (counter cmCdf1sVar) / fromIntegral size)
               , DoubleM "Blockfetch.Client.Blockdelay.cdfThree"
                    (fromIntegral (counter cmCdf3sVar) / fromIntegral size)
               , DoubleM "Blockfetch.Client.Blockdelay.cdfFive"
                    (fromIntegral (counter cmCdf5sVar) / fromIntegral size)
               ]
        in if cmDelay > 5
             then
               CounterM "Blockfetch.Client.Lateblocks" Nothing
                 : msgs
             else msgs
      else []

initialClientMetrics :: ClientMetrics
initialClientMetrics =
    ClientMetrics
      Pq.empty
      (CdfCounter 1 0)
      (CdfCounter 3 0)
      (CdfCounter 5 0)
      0
      0
      False

calculateBlockFetchClientMetrics ::
     ClientMetrics
  -> LoggingContext
  -> BlockFetch.TraceLabelPeer peer (BlockFetch.TraceFetchClientState header)
  -> IO ClientMetrics
calculateBlockFetchClientMetrics cm@ClientMetrics {..} _lc
            (TraceLabelPeer _ (BlockFetch.CompletedBlockFetch p _ _ _ forgeDelay blockSize)) =
    case pointSlot p of
            Origin -> pure cm {cmTraceIt = False}  -- Nothing to do.
            At (SlotNo slotNo) -> do
               if Pq.null cmSlotMap && forgeDelay > 20
                  then pure cm {cmTraceIt = False} -- During startup wait until we are in sync
                  else case Pq.lookup (fromIntegral slotNo) cmSlotMap of
                        Just _ -> pure cm {cmTraceIt = False}  -- dupe, we only track the first
                        Nothing -> do
                          let slotMap' = Pq.insert (fromIntegral slotNo) slotNo forgeDelay cmSlotMap
                          if Pq.size slotMap' > 1080 -- TODO k/2, should come from config file
                            then case Pq.minView slotMap' of
                                 Nothing -> pure cm {cmTraceIt = False} -- Err. We just inserted an element!
                                 Just (_, minSlotNo, minDelay, slotMap'') ->
                                   if minSlotNo == slotNo
                                      then pure cm {cmTraceIt = False, cmSlotMap = slotMap'}
                                      else let
                                         cdf1sVar = decCdf minDelay cmCdf1sVar
                                         cdf3sVar = decCdf minDelay cmCdf3sVar
                                         cdf5sVar = decCdf minDelay cmCdf5sVar
                                         cdf1sVar' = incCdf forgeDelay cdf1sVar
                                         cdf3sVar' = incCdf forgeDelay cdf3sVar
                                         cdf5sVar' = incCdf forgeDelay cdf5sVar
                                         in pure cm {
                                              cmCdf1sVar  = cdf1sVar'
                                            , cmCdf3sVar  = cdf3sVar'
                                            , cmCdf5sVar  = cdf5sVar'
                                            , cmDelay     = realToFrac  forgeDelay
                                            , cmBlockSize = blockSize
                                            , cmTraceIt   = True
                                            , cmSlotMap   = slotMap''}
                            else let
                               cdf1sVar' = incCdf forgeDelay cmCdf1sVar
                               cdf3sVar' = incCdf forgeDelay cmCdf3sVar
                               cdf5sVar' = incCdf forgeDelay cmCdf5sVar
                                -- -- Wait until we have at least 45 samples before we start providing
                                -- -- cdf estimates.
                               in if Pq.size slotMap' >= 45
                                    then pure cm {
                                         cmCdf1sVar  = cdf1sVar'
                                       , cmCdf3sVar  = cdf3sVar'
                                       , cmCdf5sVar  = cdf5sVar'
                                       , cmDelay     = realToFrac forgeDelay
                                       , cmBlockSize = blockSize
                                       , cmTraceIt   = True
                                       , cmSlotMap   = slotMap'}
                                   else pure cm {
                                        cmCdf1sVar  = cdf1sVar'
                                      , cmCdf3sVar  = cdf3sVar'
                                      , cmCdf5sVar  = cdf5sVar'
                                      , cmTraceIt   = False
                                      , cmSlotMap   = slotMap'}

calculateBlockFetchClientMetrics cm _lc _ = pure cm

--------------------------------------------------------------------------------
-- BlockFetchDecision Tracer
--------------------------------------------------------------------------------

instance (LogFormatting peer, Show peer) =>
    LogFormatting [TraceLabelPeer peer (FetchDecision [Point header])] where
  forMachine DMinimal _ = mempty
  forMachine _ []       = mconcat
    [ "kind"  .= String "EmptyPeersFetch"]
  forMachine _ xs       = mconcat
    [ "kind"  .= String "PeersFetch"
    , "peers" .= toJSON
      (foldl' (\acc x -> forMachine DDetailed x : acc) [] xs) ]

  asMetrics peers = [IntM "BlockFetch.ConnectedPeers" (fromIntegral (length peers))]

instance MetaTrace [TraceLabelPeer peer (FetchDecision [Point header])] where
  namespaceFor (a : _tl) = (nsCast . namespaceFor) a
  namespaceFor [] = Namespace [] ["EmptyPeersFetch"]

  severityFor (Namespace [] ["EmptyPeersFetch"]) _ = Just Debug
  severityFor ns Nothing =
    severityFor (nsCast ns :: Namespace (FetchDecision [Point header])) Nothing
  severityFor ns (Just []) =
    severityFor (nsCast ns :: Namespace (FetchDecision [Point header])) Nothing
  severityFor ns (Just ((TraceLabelPeer _ a) : _tl)) =
    severityFor (nsCast ns) (Just a)

  privacyFor (Namespace _ ["EmptyPeersFetch"]) _ = Just Public
  privacyFor ns Nothing =
    privacyFor (nsCast ns :: Namespace (FetchDecision [Point header])) Nothing
  privacyFor ns (Just []) =
    privacyFor (nsCast ns :: Namespace (FetchDecision [Point header])) Nothing
  privacyFor ns (Just ((TraceLabelPeer _ a) : _tl)) =
    privacyFor (nsCast ns) (Just a)

  detailsFor (Namespace _ ["EmptyPeersFetch"]) _ = Just DNormal
  detailsFor ns Nothing =
    detailsFor (nsCast ns :: Namespace (FetchDecision [Point header])) Nothing
  detailsFor ns (Just []) =
    detailsFor (nsCast ns :: Namespace (FetchDecision [Point header])) Nothing
  detailsFor ns (Just ((TraceLabelPeer _ a) : _tl)) =
    detailsFor (nsCast ns) (Just a)
  documentFor ns = documentFor (nsCast ns :: Namespace (FetchDecision [Point header]))
  metricsDocFor ns = metricsDocFor (nsCast ns :: Namespace (FetchDecision [Point header]))
  allNamespaces = Namespace [] ["EmptyPeersFetch"]
    : map nsCast (allNamespaces :: [Namespace (FetchDecision [Point header])])

instance LogFormatting (FetchDecision [Point header]) where
  forMachine _dtal (Left decline) =
    mconcat [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (showT decline)
             ]
  forMachine _dtal (Right results) =
    mconcat [ "kind" .= String "FetchDecision results"
             , "length" .= String (showT $ length results)
             ]

instance MetaTrace (FetchDecision [Point header]) where
    namespaceFor (Left _) = Namespace [] ["Decline"]
    namespaceFor (Right _) = Namespace [] ["Accept"]

    severityFor (Namespace _ ["Decline"]) _ = Just Info
    severityFor (Namespace _ ["Accept"])  _ = Just Info
    severityFor _ _ = Nothing

    metricsDocFor (Namespace _ ["Decline"]) =
      [("Blockfetch.ConnectedPeers", "Number of connected peers")]
    metricsDocFor (Namespace _ ["Accept"]) =
      [("Blockfetch.ConnectedPeers", "Number of connected peers")]
    metricsDocFor _ = []

    documentFor _ =  Just
      "Throughout the decision making process we accumulate reasons to decline\
      \ to fetch any blocks. This message carries the intermediate and final\
      \ results."
    allNamespaces =
      [ Namespace [] ["Decline"]
      , Namespace [] ["Accept"]]


--------------------------------------------------------------------------------
-- BlockFetchClientState Tracer
--------------------------------------------------------------------------------

instance (HasHeader header, ConvertRawHash header) =>
  LogFormatting (BlockFetch.TraceFetchClientState header) where
    forMachine _dtal BlockFetch.AddedFetchRequest {} =
      mconcat [ "kind" .= String "AddedFetchRequest" ]
    forMachine _dtal BlockFetch.AcknowledgedFetchRequest {} =
      mconcat [ "kind" .= String "AcknowledgedFetchRequest" ]
    forMachine _dtal (BlockFetch.SendFetchRequest af _) =
      mconcat [ "kind" .= String "SendFetchRequest"
              , "head" .= String (renderChainHash
                                  (renderHeaderHash (Proxy @header))
                                  (AF.headHash af))
              , "length" .= toJSON (fragmentLength af)]
        where
          -- NOTE: this ignores the Byron era with its EBB complication:
          -- the length would be underestimated by 1, if the AF is anchored
          -- at the epoch boundary.
          fragmentLength :: AF.AnchoredFragment header -> Int
          fragmentLength f = fromIntegral . unBlockNo $
              case (f, f) of
                (AS.Empty{}, AS.Empty{}) -> 0
                (firstHdr AS.:< _, _ AS.:> lastHdr) ->
                  blockNo lastHdr - blockNo firstHdr + 1
    forMachine _dtal (BlockFetch.CompletedBlockFetch pt _ _ _ delay blockSize) =
      mconcat [ "kind"  .= String "CompletedBlockFetch"
              , "delay" .= (realToFrac delay :: Double)
              , "size"  .= blockSize
              , "block" .= String
                (case pt of
                  GenesisPoint -> "Genesis"
                  BlockPoint _ h -> renderHeaderHash (Proxy @header) h)
              ]
    forMachine _dtal BlockFetch.CompletedFetchBatch {} =
      mconcat [ "kind" .= String "CompletedFetchBatch" ]
    forMachine _dtal BlockFetch.StartedFetchBatch {} =
      mconcat [ "kind" .= String "StartedFetchBatch" ]
    forMachine _dtal BlockFetch.RejectedFetchBatch {} =
      mconcat [ "kind" .= String "RejectedFetchBatch" ]
    forMachine _dtal (BlockFetch.ClientTerminating outstanding) =
      mconcat [ "kind" .= String "ClientTerminating"
              , "outstanding" .= outstanding
              ]

instance MetaTrace (BlockFetch.TraceFetchClientState header) where
    namespaceFor BlockFetch.AddedFetchRequest {} =
      Namespace [] ["AddedFetchRequest"]
    namespaceFor BlockFetch.AcknowledgedFetchRequest {} =
      Namespace [] ["AcknowledgedFetchRequest"]
    namespaceFor BlockFetch.SendFetchRequest {} =
      Namespace [] ["SendFetchRequest"]
    namespaceFor BlockFetch.StartedFetchBatch {} =
      Namespace [] ["StartedFetchBatch"]
    namespaceFor BlockFetch.CompletedFetchBatch {} =
      Namespace [] ["CompletedFetchBatch"]
    namespaceFor BlockFetch.CompletedBlockFetch {} =
      Namespace [] ["CompletedBlockFetch"]
    namespaceFor BlockFetch.RejectedFetchBatch {} =
      Namespace [] ["RejectedFetchBatch"]
    namespaceFor BlockFetch.ClientTerminating {} =
      Namespace [] ["ClientTerminating"]

    severityFor (Namespace _ ["AddedFetchRequest"]) _ = Just Info
    severityFor (Namespace _ ["AcknowledgedFetchRequest"]) _ = Just Info
    severityFor (Namespace _ ["SendFetchRequest"]) _ = Just Info
    severityFor (Namespace _ ["StartedFetchBatch"]) _ = Just Info
    severityFor (Namespace _ ["CompletedFetchBatch"]) _ = Just Info
    severityFor (Namespace _ ["CompletedBlockFetch"]) _ = Just Info
    severityFor (Namespace _ ["RejectedFetchBatch"]) _ = Just Info
    severityFor (Namespace _ ["ClientTerminating"]) _ = Just Notice
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["AddedFetchRequest"]) = Just
      "The block fetch decision thread has added a new fetch instruction\
        \ consisting of one or more individual request ranges."
    documentFor (Namespace _ ["AcknowledgedFetchRequest"]) = Just
      "Mark the point when the fetch client picks up the request added\
            \ by the block fetch decision thread. Note that this event can happen\
            \ fewer times than the 'AddedFetchRequest' due to fetch request merging."
    documentFor (Namespace _ ["SendFetchRequest"]) = Just
      "Mark the point when fetch request for a fragment is actually sent\
       \ over the wire."
    documentFor (Namespace _ ["StartedFetchBatch"]) = Just
      "Mark the start of receiving a streaming batch of blocks. This will\
      \ be followed by one or more 'CompletedBlockFetch' and a final\
      \ 'CompletedFetchBatch'"
    documentFor (Namespace _ ["CompletedFetchBatch"]) = Just
      "Mark the successful end of receiving a streaming batch of blocks."
    documentFor (Namespace _ ["CompletedBlockFetch"]) = Just
      ""
    documentFor (Namespace _ ["RejectedFetchBatch"]) = Just
      "If the other peer rejects our request then we have this event\
       \ instead of 'StartedFetchBatch' and 'CompletedFetchBatch'."
    documentFor (Namespace _ ["ClientTerminating"]) = Just
      "The client is terminating.  Log the number of outstanding\
       \ requests."
    documentFor _ = Nothing

    allNamespaces = [
         Namespace [] ["AddedFetchRequest"]
       , Namespace [] ["AcknowledgedFetchRequest"]
       , Namespace [] ["SendFetchRequest"]
       , Namespace [] ["StartedFetchBatch"]
       , Namespace [] ["CompletedFetchBatch"]
       , Namespace [] ["CompletedBlockFetch"]
       , Namespace [] ["RejectedFetchBatch"]
       , Namespace [] ["ClientTerminating"]
      ]

--------------------------------------------------------------------------------
-- BlockFetchServerEvent
--------------------------------------------------------------------------------

instance ConvertRawHash blk => LogFormatting (TraceBlockFetchServerEvent blk) where
  forMachine _dtal (TraceBlockFetchServerSendBlock blk) =
    mconcat [ "kind" .= String "BlockFetchServer"
             , "block" .= String (renderChainHash
                                    @blk
                                    (renderHeaderHash (Proxy @blk))
                                    $ pointHash blk)]
  asMetrics (TraceBlockFetchServerSendBlock _p) =
    [CounterM "BlockFetch.BlocksServed" Nothing]

instance MetaTrace (TraceBlockFetchServerEvent blk) where
    namespaceFor TraceBlockFetchServerSendBlock {} =
      Namespace [] ["SendBlock"]

    severityFor (Namespace [] ["SendBlock"]) _ = Just
      Info
    severityFor _ _ = Nothing

    metricsDocFor (Namespace [] ["SendBlock"]) =
      [("Blockfetch.BlocksServed", "")]
    metricsDocFor _ = []

    documentFor (Namespace [] ["SendBlock"]) = Just
      "The server sent a block to the peer."
    documentFor _ = Nothing

    allNamespaces = [Namespace [] ["SendBlock"]]

--------------------------------------------------------------------------------
-- TxInbound Tracer
--------------------------------------------------------------------------------

instance LogFormatting (TraceTxSubmissionInbound txid tx) where
  forMachine _dtal (TraceTxSubmissionCollected count) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionCollected"
      , "count" .= toJSON count
      ]
  forMachine _dtal (TraceTxSubmissionProcessed processed) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionProcessed"
      , "accepted" .= toJSON (ptxcAccepted processed)
      , "rejected" .= toJSON (ptxcRejected processed)
      ]
  forMachine _dtal TraceTxInboundTerminated =
    mconcat
      [ "kind" .= String "TraceTxInboundTerminated"
      ]
  forMachine _dtal (TraceTxInboundCanRequestMoreTxs count) =
    mconcat
      [ "kind" .= String "TraceTxInboundCanRequestMoreTxs"
      , "count" .= toJSON count
      ]
  forMachine _dtal (TraceTxInboundCannotRequestMoreTxs count) =
    mconcat
      [ "kind" .= String "TraceTxInboundCannotRequestMoreTxs"
      , "count" .= toJSON count
      ]

  asMetrics (TraceTxSubmissionCollected count)=
    [CounterM "TxSubmission.Submitted" (Just count)]
  asMetrics (TraceTxSubmissionProcessed processed) =
    [ CounterM "TxSubmission.Accepted"
        (Just (ptxcAccepted processed))
    , CounterM "TxSubmission.Rejected"
        (Just (ptxcRejected processed))
    ]
  asMetrics _ = []

instance MetaTrace (TraceTxSubmissionInbound txid tx) where
    namespaceFor TraceTxSubmissionCollected {} = Namespace [] ["Collected"]
    namespaceFor TraceTxSubmissionProcessed {} = Namespace [] ["Processed"]
    namespaceFor TraceTxInboundTerminated {} = Namespace [] ["Terminated"]
    namespaceFor TraceTxInboundCanRequestMoreTxs {} = Namespace [] ["CanRequestMoreTxs"]
    namespaceFor TraceTxInboundCannotRequestMoreTxs {} = Namespace [] ["CannotRequestMoreTxs"]

    severityFor (Namespace _ ["Collected"]) _ = Just Debug
    severityFor (Namespace _ ["Processed"]) _ = Just Debug
    severityFor (Namespace _ ["Terminated"]) _ = Just Notice
    severityFor (Namespace _ ["CanRequestMoreTxs"]) _ = Just Debug
    severityFor (Namespace _ ["CannotRequestMoreTxs"]) _ = Just Debug
    severityFor _ _ = Nothing

    metricsDocFor (Namespace _ ["Collected"]) =
      [ ("TxSubmission.Submitted", "")]
    metricsDocFor (Namespace _ ["Processed"]) =
      [ ("TxSubmission.Accepted", "")
      , ("TxSubmission.Rejected", "")
      ]
    metricsDocFor _ = []

    documentFor (Namespace _ ["Collected"]) = Just
      "Number of transactions just about to be inserted."
    documentFor (Namespace _ ["Processed"]) = Just
      "Just processed transaction pass/fail breakdown."
    documentFor (Namespace _ ["Terminated"]) = Just
      "Server received 'MsgDone'."
    documentFor (Namespace _ ["CanRequestMoreTxs"]) = Just
      "There are no replies in flight, but we do know some more txs we\
      \ can ask for, so lets ask for them and more txids."
    documentFor (Namespace _ ["CannotRequestMoreTxs"]) = Just
      "There's no replies in flight, and we have no more txs we can\
      \ ask for so the only remaining thing to do is to ask for more\
      \ txids. Since this is the only thing to do now, we make this a\
      \ blocking call."
    documentFor _ = Nothing

    allNamespaces = [
          Namespace [] ["Collected"]
        , Namespace [] ["Processed"]
        , Namespace [] ["Terminated"]
        , Namespace [] ["CanRequestMoreTxs"]
        , Namespace [] ["CannotRequestMoreTxs"]
        ]

--------------------------------------------------------------------------------
-- TxOutbound Tracer
--------------------------------------------------------------------------------

instance (Show txid, Show tx)
      => LogFormatting (TraceTxSubmissionOutbound txid tx) where
  forMachine DDetailed (TraceTxSubmissionOutboundRecvMsgRequestTxs txids) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      , "txIds" .= String (Text.pack $ show txids)
      ]
  forMachine _dtal (TraceTxSubmissionOutboundRecvMsgRequestTxs _txids) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      ]
  forMachine DDetailed (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      , "txs" .= String (Text.pack $ show txs)
      ]
  forMachine _dtal (TraceTxSubmissionOutboundSendMsgReplyTxs _txs) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      ]
  forMachine _dtal (TraceControlMessage _msg) =
    mconcat
      [ "kind" .= String "TraceControlMessage"
      ]

instance MetaTrace (TraceTxSubmissionOutbound txid tx) where
    namespaceFor TraceTxSubmissionOutboundRecvMsgRequestTxs {} =
      Namespace [] ["RecvMsgRequest"]
    namespaceFor TraceTxSubmissionOutboundSendMsgReplyTxs {} =
      Namespace [] ["SendMsgReply"]
    namespaceFor TraceControlMessage {} =
      Namespace [] ["ControlMessage"]

    severityFor (Namespace _ ["RecvMsgRequest"]) _ =
      Just Info
    severityFor (Namespace _ ["SendMsgReply"]) _ =
      Just Info
    severityFor (Namespace _ ["ControlMessage"]) _ =
      Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["RecvMsgRequest"]) = Just
      "The IDs of the transactions requested."
    documentFor (Namespace _ ["SendMsgReply"]) = Just
      "The transactions to be sent in the response."
    documentFor (Namespace _ ["ControlMessage"]) = Just
      ""
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["RecvMsgRequest"]
      , Namespace [] ["SendMsgReply"]
      , Namespace [] ["ControlMessage"]
      ]


--------------------------------------------------------------------------------
-- TxSubmissionServer Tracer
--------------------------------------------------------------------------------

instance LogFormatting (TraceLocalTxSubmissionServerEvent blk) where
  forMachine _dtal (TraceReceivedTx _gtx) =
    mconcat [ "kind" .= String "ReceivedTx" ]


instance MetaTrace (TraceLocalTxSubmissionServerEvent blk) where

    namespaceFor TraceReceivedTx {} =
      Namespace [] ["ReceivedTx"]

    severityFor (Namespace _ ["ReceivedTx"]) _ =
      Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["ReceivedTx"]) = Just
      "A transaction was received."
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["ReceivedTx"]
      ]

--------------------------------------------------------------------------------
-- Mempool Tracer
--------------------------------------------------------------------------------

instance
  ( LogFormatting (ApplyTxErr blk)
  , LogFormatting (GenTx blk)
  , ToJSON (GenTxId blk)
  , LedgerSupportsMempool blk
  ) => LogFormatting (TraceEventMempool blk) where
  forMachine dtal (TraceMempoolAddedTx tx _mpSzBefore mpSzAfter) =
    mconcat
      [ "kind" .= String "TraceMempoolAddedTx"
      , "tx" .= forMachine dtal (txForgetValidated tx)
      , "mempoolSize" .= forMachine dtal mpSzAfter
      ]
  forMachine dtal (TraceMempoolRejectedTx tx txApplyErr mpSz) =
    mconcat
      [ "kind" .= String "TraceMempoolRejectedTx"
      , "err" .= forMachine dtal txApplyErr
      , "tx" .= forMachine dtal tx
      , "mempoolSize" .= forMachine dtal mpSz
      ]
  forMachine dtal (TraceMempoolRemoveTxs txs mpSz) =
    mconcat
      [ "kind" .= String "TraceMempoolRemoveTxs"
      , "txs" .= map (forMachine dtal . txForgetValidated) txs
      , "mempoolSize" .= forMachine dtal mpSz
      ]
  forMachine dtal (TraceMempoolManuallyRemovedTxs txs0 txs1 mpSz) =
    mconcat
      [ "kind" .= String "TraceMempoolManuallyRemovedTxs"
      , "txsRemoved" .= txs0
      , "txsInvalidated" .= map (forMachine dtal . txForgetValidated) txs1
      , "mempoolSize" .= forMachine dtal mpSz
      ]

  asMetrics (TraceMempoolAddedTx _tx _mpSzBefore mpSz) =
    [ IntM "Mempool.TxsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "Mempool.MempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolRejectedTx _tx _txApplyErr mpSz) =
    [ IntM "Mempool.TxsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "Mempool.MempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolRemoveTxs _txs mpSz) =
    [ IntM "Mempool.TxsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "Mempool.MempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolManuallyRemovedTxs [] _txs1 mpSz) =
    [ IntM "Mempool.TxsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "Mempool.MempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolManuallyRemovedTxs txs _txs1 mpSz) =
    [ IntM "Mempool.TxsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "Mempool.MempoolBytes" (fromIntegral $ msNumBytes mpSz)
    , CounterM "Mempool.TxsProcessedNum" (Just (fromIntegral $ length txs))
    ]

instance LogFormatting MempoolSize where
  forMachine _dtal MempoolSize{msNumTxs, msNumBytes} =
    mconcat
      [ "numTxs" .= msNumTxs
      , "bytes" .= msNumBytes
      ]


instance MetaTrace (TraceEventMempool blk) where
    namespaceFor TraceMempoolAddedTx {} = Namespace [] ["AddedTx"]
    namespaceFor TraceMempoolRejectedTx {} = Namespace [] ["RejectedTx"]
    namespaceFor TraceMempoolRemoveTxs {} = Namespace [] ["RemoveTxs"]
    namespaceFor TraceMempoolManuallyRemovedTxs {} = Namespace [] ["ManuallyRemovedTxs"]

    severityFor (Namespace _ ["AddedTx"]) _ = Just Info
    severityFor (Namespace _ ["RejectedTx"]) _ = Just Info
    severityFor (Namespace _ ["RemoveTxs"]) _ = Just Info
    severityFor (Namespace _ ["ManuallyRemovedTxs"]) _ = Just Info
    severityFor _ _ = Nothing

    metricsDocFor (Namespace _ ["AddedTx"]) =
      [ ("Mempool.TxsInMempool","Transactions in mempool")
      , ("Mempool.MempoolBytes", "Byte size of the mempool")
      ]
    metricsDocFor (Namespace _ ["RejectedTx"]) =
      [ ("Mempool.TxsInMempool","Transactions in mempool")
      , ("Mempool.MempoolBytes", "Byte size of the mempool")
      ]
    metricsDocFor (Namespace _ ["RemoveTxs"]) =
      [ ("Mempool.TxsInMempool","Transactions in mempool")
      , ("Mempool.MempoolBytes", "Byte size of the mempool")
      ]
    metricsDocFor (Namespace _ ["ManuallyRemovedTxs"]) =
      [ ("Mempool.TxsInMempool","Transactions in mempool")
      , ("Mempool.MempoolBytes", "Byte size of the mempool")
      , ("Mempool.TxsProcessedNum", "")
      ]
    metricsDocFor _ = []

    documentFor (Namespace _ ["AddedTx"]) = Just
      "New, valid transaction that was added to the Mempool."
    documentFor (Namespace _ ["RejectedTx"]) = Just
      "New, invalid transaction thas was rejected and thus not added to\
       \ the Mempool."
    documentFor (Namespace _ ["RemoveTxs"]) = Just
      "Previously valid transactions that are no longer valid because of\
      \ changes in the ledger state. These transactions have been removed\
      \ from the Mempool."
    documentFor (Namespace _ ["ManuallyRemovedTxs"]) = Just
      "Transactions that have been manually removed from the Mempool."
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["AddedTx"]
      , Namespace [] ["RejectedTx"]
      , Namespace [] ["RemoveTxs"]
      , Namespace [] ["ManuallyRemovedTxs"]
      ]

--------------------------------------------------------------------------------
-- ForgeTracerType
--------------------------------------------------------------------------------

instance ( tx ~ GenTx blk
         , ConvertRawHash blk
         , GetHeader blk
         , HasHeader blk
         , HasKESInfo blk
         , LedgerSupportsProtocol blk
         , LedgerSupportsMempool blk
         , SerialiseNodeToNodeConstraints blk
         , HasTxId (GenTx blk)
         , Show (ForgeStateUpdateError blk)
         , Show (CannotForge blk)
         , LogFormatting (InvalidBlockReason blk)
         , LogFormatting (CannotForge blk)
         , LogFormatting (ForgeStateUpdateError blk))
         => LogFormatting (ForgeTracerType blk) where
  forMachine dtal (Left i)  = forMachine dtal i
  forMachine dtal (Right i) = forMachine dtal i
  forHuman (Left i)  = forHuman i
  forHuman (Right i) = forHuman i
  asMetrics (Left i)  = asMetrics i
  asMetrics (Right i) = asMetrics i

instance MetaTrace  (ForgeTracerType blk) where
  namespaceFor (Left ev) =
    nsCast (namespaceFor ev)
  namespaceFor (Right _ev) =
    Namespace [] ["StartLeadershipCheckPlus"]

  severityFor (Namespace _ ["StartLeadershipCheckPlus"]) _ = Just
    Info
  severityFor ns (Just (Left ev')) =
    severityFor (nsCast ns) (Just ev')
  severityFor ns Nothing =
    severityFor (nsCast ns :: Namespace (TraceForgeEvent blk)) Nothing
  severityFor _ _ = Nothing

  detailsFor (Namespace _ ["StartLeadershipCheckPlus"]) _ = Just
    DNormal
  detailsFor ns (Just (Left ev')) =
    detailsFor (nsCast ns) (Just ev')
  detailsFor ns Nothing =
    detailsFor (nsCast ns :: Namespace (TraceForgeEvent blk)) Nothing
  detailsFor _ _ = Nothing

  privacyFor (Namespace _ ["StartLeadershipCheckPlus"]) _ = Just
    Public
  privacyFor ns (Just (Left ev')) =
    privacyFor (nsCast ns) (Just ev')
  privacyFor ns Nothing =
    privacyFor (nsCast ns :: Namespace (TraceForgeEvent blk)) Nothing
  privacyFor _ _ = Nothing

  metricsDocFor (Namespace _ ["StartLeadershipCheckPlus"]) =
      [ ("Forge.UtxoSize", "")
      , ("Forge.DelegMapSize", "")
      ]
  metricsDocFor ns =
    metricsDocFor (nsCast ns :: Namespace (TraceForgeEvent blk))

  documentFor (Namespace _ ["StartLeadershipCheckPlus"]) = Just
    "We adopted the block we produced, we also trace the transactions\
    \  that were adopted."
  documentFor ns =
    documentFor (nsCast ns :: Namespace (TraceForgeEvent blk))

  allNamespaces =
    Namespace [] ["StartLeadershipCheckPlus"]
    : map nsCast (allNamespaces :: [Namespace (TraceForgeEvent blk)])

--------------------------------------------------------------------------------
-- TraceStartLeadershipCheck
--------------------------------------------------------------------------------

instance LogFormatting TraceStartLeadershipCheckPlus where
  forMachine _dtal TraceStartLeadershipCheckPlus {..} =
        mconcat [ "kind" .= String "TraceStartLeadershipCheck"
                , "slot" .= toJSON (unSlotNo tsSlotNo)
                , "utxoSize" .= Number (fromIntegral tsUtxoSize)
                , "delegMapSize" .= Number (fromIntegral tsUtxoSize)
                , "chainDensity" .= Number (fromRational (toRational tsChainDensity))
                ]
  forHuman TraceStartLeadershipCheckPlus {..} =
      "Checking for leadership in slot " <> showT (unSlotNo tsSlotNo)
      <> " utxoSize " <> showT tsUtxoSize
      <> " delegMapSize " <> showT tsDelegMapSize
      <> " chainDensity " <> showT tsChainDensity
  asMetrics TraceStartLeadershipCheckPlus {..} =
    [IntM "Forge.UtxoSize" (fromIntegral tsUtxoSize),
     IntM "Forge.DelegMapSize" (fromIntegral tsDelegMapSize)]

--------------------------------------------------------------------------------
-- ForgeEvent Tracer
--------------------------------------------------------------------------------

instance ( tx ~ GenTx blk
         , ConvertRawHash blk
         , GetHeader blk
         , HasHeader blk
         , HasKESInfo blk
         , HasTxId (GenTx blk)
         , LedgerSupportsProtocol blk
         , LedgerSupportsMempool blk
         , SerialiseNodeToNodeConstraints blk
         , Show (ForgeStateUpdateError blk)
         , Show (CannotForge blk)
         , Show (TxId (GenTx blk))
         , LogFormatting (InvalidBlockReason blk)
         , LogFormatting (CannotForge blk)
         , LogFormatting (ForgeStateUpdateError blk))
      => LogFormatting (TraceForgeEvent blk) where
  forMachine _dtal (TraceStartLeadershipCheck slotNo) =
    mconcat
      [ "kind" .= String "TraceStartLeadershipCheck"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceSlotIsImmutable slotNo tipPoint tipBlkNo) =
    mconcat
      [ "kind" .= String "TraceSlotIsImmutable"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "tip" .= renderPointForDetails dtal tipPoint
      , "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  forMachine _dtal (TraceBlockFromFuture currentSlot tip) =
    mconcat
      [ "kind" .= String "TraceBlockFromFuture"
      , "current slot" .= toJSON (unSlotNo currentSlot)
      , "tip" .= toJSON (unSlotNo tip)
      ]
  forMachine dtal (TraceBlockContext currentSlot tipBlkNo tipPoint) =
    mconcat
      [ "kind" .= String "TraceBlockContext"
      , "current slot" .= toJSON (unSlotNo currentSlot)
      , "tip" .= renderPointForDetails dtal tipPoint
      , "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  forMachine _dtal (TraceNoLedgerState slotNo _pt) =
    mconcat
      [ "kind" .= String "TraceNoLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceLedgerState slotNo _pt) =
    mconcat
      [ "kind" .= String "TraceLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceNoLedgerView slotNo _) =
    mconcat
      [ "kind" .= String "TraceNoLedgerView"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceLedgerView slotNo) =
    mconcat
      [ "kind" .= String "TraceLedgerView"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceForgeStateUpdateError slotNo reason) =
    mconcat
      [ "kind" .= String "TraceForgeStateUpdateError"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine dtal (TraceNodeCannotForge slotNo reason) =
    mconcat
      [ "kind" .= String "TraceNodeCannotForge"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine _dtal (TraceNodeNotLeader slotNo) =
    mconcat
      [ "kind" .= String "TraceNodeNotLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceNodeIsLeader slotNo) =
    mconcat
      [ "kind" .= String "TraceNodeIsLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceForgeTickedLedgerState slotNo prevPt) =
    mconcat
      [ "kind" .= String "TraceForgeTickedLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "prev" .= renderPointForDetails dtal prevPt
      ]
  forMachine dtal (TraceForgingMempoolSnapshot slotNo prevPt mpHash mpSlot) =
    mconcat
      [ "kind"        .= String "TraceForgingMempoolSnapshot"
      , "slot"        .= toJSON (unSlotNo slotNo)
      , "prev"        .= renderPointForDetails dtal prevPt
      , "mempoolHash" .= String (renderChainHash @blk (renderHeaderHash (Proxy @blk)) mpHash)
      , "mempoolSlot" .= toJSON (unSlotNo mpSlot)
      ]
  forMachine _dtal (TraceForgedBlock slotNo _ blk _) =
    mconcat
      [ "kind" .= String "TraceForgedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "block"     .= String (renderHeaderHash (Proxy @blk) $ blockHash blk)
      , "blockNo"   .= toJSON (unBlockNo $ blockNo blk)
      , "blockPrev" .= String (renderChainHash
                                @blk
                                (renderHeaderHash (Proxy @blk))
                                $ blockPrevHash blk)
      ]
  forMachine _dtal (TraceDidntAdoptBlock slotNo _) =
    mconcat
      [ "kind" .= String "TraceDidntAdoptBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceForgedInvalidBlock slotNo _ reason) =
    mconcat
      [ "kind" .= String "TraceForgedInvalidBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine DDetailed (TraceAdoptedBlock slotNo blk txs) =
    mconcat
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "blockHash" .= renderHeaderHashForDetails
          (Proxy @blk)
          DDetailed
          (blockHash blk)
      , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
      , "txIds" .= toJSON (map (show . txId . txForgetValidated) txs)
      ]
  forMachine dtal (TraceAdoptedBlock slotNo blk _txs) =
    mconcat
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "blockHash" .= renderHeaderHashForDetails
          (Proxy @blk)
          dtal
          (blockHash blk)
      , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
      ]

  forHuman (TraceStartLeadershipCheck slotNo) =
      "Checking for leadership in slot " <> showT (unSlotNo slotNo)
  forHuman (TraceSlotIsImmutable slotNo immutableTipPoint immutableTipBlkNo) =
      "Couldn't forge block because current slot is immutable: "
        <> "immutable tip: " <> renderPointAsPhrase immutableTipPoint
        <> ", immutable tip block no: " <> showT (unBlockNo immutableTipBlkNo)
        <> ", current slot: " <> showT (unSlotNo slotNo)
  forHuman (TraceBlockFromFuture currentSlot tipSlot) =
      "Couldn't forge block because current tip is in the future: "
        <> "current tip slot: " <> showT (unSlotNo tipSlot)
        <> ", current slot: " <> showT (unSlotNo currentSlot)
  forHuman (TraceBlockContext currentSlot tipBlockNo tipPoint) =
      "New block will fit onto: "
        <> "tip: " <> renderPointAsPhrase tipPoint
        <> ", tip block no: " <> showT (unBlockNo tipBlockNo)
        <> ", current slot: " <> showT (unSlotNo currentSlot)
  forHuman (TraceNoLedgerState slotNo pt) =
      "Could not obtain ledger state for point "
        <> renderPointAsPhrase pt
        <> ", current slot: "
        <> showT (unSlotNo slotNo)
  forHuman (TraceLedgerState slotNo pt) =
      "Obtained a ledger state for point "
        <> renderPointAsPhrase pt
        <> ", current slot: "
        <> showT (unSlotNo slotNo)
  forHuman (TraceNoLedgerView slotNo _) =
      "Could not obtain ledger view for slot " <> showT (unSlotNo slotNo)
  forHuman (TraceLedgerView slotNo) =
      "Obtained a ledger view for slot " <> showT (unSlotNo slotNo)
  forHuman (TraceForgeStateUpdateError slotNo reason) =
      "Updating the forge state in slot "
        <> showT (unSlotNo slotNo)
        <> " failed because: "
        <> showT reason
  forHuman (TraceNodeCannotForge slotNo reason) =
      "We are the leader in slot "
        <> showT (unSlotNo slotNo)
        <> ", but we cannot forge because: "
        <> showT reason
  forHuman (TraceNodeNotLeader slotNo) =
      "Not leading slot " <> showT (unSlotNo slotNo)
  forHuman (TraceNodeIsLeader slotNo) =
      "Leading slot " <> showT (unSlotNo slotNo)
  forHuman (TraceForgeTickedLedgerState slotNo prevPt) =
      "While forging in slot "
        <> showT (unSlotNo slotNo)
        <> " we ticked the ledger state ahead from "
        <> renderPointAsPhrase prevPt
  forHuman (TraceForgingMempoolSnapshot slotNo prevPt mpHash mpSlot) =
      "While forging in slot "
        <> showT (unSlotNo slotNo)
        <> " we acquired a mempool snapshot valid against "
        <> renderPointAsPhrase prevPt
        <> " from a mempool that was prepared for "
        <> renderChainHash @blk (renderHeaderHash (Proxy @blk)) mpHash
        <> " ticked to slot "
        <> showT (unSlotNo mpSlot)
  forHuman (TraceForgedBlock slotNo _ _ _) =
      "Forged block in slot " <> showT (unSlotNo slotNo)
  forHuman (TraceDidntAdoptBlock slotNo _) =
      "Didn't adopt forged block in slot " <> showT (unSlotNo slotNo)
  forHuman (TraceForgedInvalidBlock slotNo _ reason) =
      "Forged invalid block in slot "
        <> showT (unSlotNo slotNo)
        <> ", reason: " <> showT reason
  forHuman (TraceAdoptedBlock slotNo blk _txs) =
      "Adopted block forged in slot "
        <> showT (unSlotNo slotNo)
        <> ": " <> renderHeaderHash (Proxy @blk) (blockHash blk)

  asMetrics (TraceForgeStateUpdateError slot reason) =
    IntM "Forge.StateUpdateError" (fromIntegral $ unSlotNo slot) :
      (case getKESInfo (Proxy @blk) reason of
        Nothing -> []
        Just kesInfo ->
          [ IntM
              "Forge.OperationalCertificateStartKESPeriod"
              (fromIntegral . unKESPeriod . HotKey.kesStartPeriod $ kesInfo)
          , IntM
              "Forge.OperationalCertificateExpiryKESPeriod"
              (fromIntegral . unKESPeriod . HotKey.kesEndPeriod $ kesInfo)
          , IntM
              "Forge.CurrentKESPeriod"
              0
          , IntM
              "Forge.RemainingKESPeriods"
              0
          ])

  asMetrics (TraceStartLeadershipCheck slot) =
    [IntM "Forge.AboutToLeadSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceSlotIsImmutable slot _tipPoint _tipBlkNo) =
    [IntM "Forge.SlotIsImmutable" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceBlockFromFuture slot _slotNo) =
    [IntM "Forge.BlockFromFuture" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceBlockContext slot _tipBlkNo _tipPoint) =
    [IntM "Forge.BlockContext" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNoLedgerState slot _) =
    [IntM "Forge.CouldNotForgeSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceLedgerState slot _) =
    [IntM "Forge.LedgerState" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNoLedgerView slot _) =
    [IntM "Forge.CouldNotForgeSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceLedgerView slot) =
    [IntM "Forge.LedgerView" (fromIntegral $ unSlotNo slot)]
  -- see above
  asMetrics (TraceNodeCannotForge slot _reason) =
    [IntM "Forge.NodeCannotForge" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNodeNotLeader slot) =
    [IntM "Forge.NodeNotLeader" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNodeIsLeader slot) =
    [IntM "Forge.NodeIsLeader" (fromIntegral $ unSlotNo slot)]
  asMetrics TraceForgeTickedLedgerState {} = []
  asMetrics TraceForgingMempoolSnapshot {} = []
  asMetrics (TraceForgedBlock slot _ _ _) =
    [IntM "Forge.ForgedSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceDidntAdoptBlock slot _) =
    [IntM "Forge.NotAdoptedSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceForgedInvalidBlock slot _ _) =
    [IntM "Forge.ForgedInvalidSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceAdoptedBlock slot _ _) =
    [IntM "Forge.AdoptedOwnBlockSlotLast" (fromIntegral $ unSlotNo slot)]

instance MetaTrace (TraceForgeEvent blk) where
  namespaceFor TraceStartLeadershipCheck {} =
    Namespace [] ["StartLeadershipCheck"]
  namespaceFor TraceSlotIsImmutable {} =
    Namespace [] ["SlotIsImmutable"]
  namespaceFor TraceBlockFromFuture {} =
    Namespace [] ["BlockFromFuture"]
  namespaceFor TraceBlockContext {} =
    Namespace [] ["BlockContext"]
  namespaceFor TraceNoLedgerState {} =
    Namespace [] ["NoLedgerState"]
  namespaceFor TraceLedgerState {} =
    Namespace [] ["LedgerState"]
  namespaceFor TraceNoLedgerView {} =
    Namespace [] ["NoLedgerView"]
  namespaceFor TraceLedgerView {} =
    Namespace [] ["LedgerView"]
  namespaceFor TraceForgeStateUpdateError {} =
    Namespace [] ["ForgeStateUpdateError"]
  namespaceFor TraceNodeCannotForge {} =
    Namespace [] ["NodeCannotForge"]
  namespaceFor TraceNodeNotLeader {} =
    Namespace [] ["NodeNotLeader"]
  namespaceFor TraceNodeIsLeader {} =
    Namespace [] ["NodeIsLeader"]
  namespaceFor TraceForgeTickedLedgerState {} =
    Namespace [] ["ForgeTickedLedgerState"]
  namespaceFor TraceForgingMempoolSnapshot {} =
    Namespace [] ["ForgingMempoolSnapshot"]
  namespaceFor TraceForgedBlock {} =
    Namespace [] ["ForgedBlock"]
  namespaceFor TraceDidntAdoptBlock {} =
    Namespace [] ["DidntAdoptBlock"]
  namespaceFor TraceForgedInvalidBlock {} =
    Namespace [] ["ForgedInvalidBlock"]
  namespaceFor TraceAdoptedBlock {} =
    Namespace [] ["AdoptedBlock"]

  severityFor (Namespace _ ["StartLeadershipCheck"]) _ = Just Info
  severityFor (Namespace _ ["SlotIsImmutable"]) _ = Just Error
  severityFor (Namespace _ ["BlockFromFuture"]) _ = Just Error
  severityFor (Namespace _ ["BlockContext"]) _ = Just Debug
  severityFor (Namespace _ ["NoLedgerState"]) _ = Just Error
  severityFor (Namespace _ ["LedgerState"]) _ = Just Debug
  severityFor (Namespace _ ["NoLedgerView"]) _ = Just Error
  severityFor (Namespace _ ["LedgerView"]) _ = Just Debug
  severityFor (Namespace _ ["ForgeStateUpdateError"]) _ = Just Error
  severityFor (Namespace _ ["NodeCannotForge"]) _ = Just Error
  severityFor (Namespace _ ["NodeNotLeader"]) _ = Just Info
  severityFor (Namespace _ ["NodeIsLeader"]) _ = Just Info
  severityFor (Namespace _ ["ForgeTickedLedgerState"]) _ = Just Debug
  severityFor (Namespace _ ["ForgingMempoolSnapshot"]) _ = Just Debug
  severityFor (Namespace _ ["ForgedBlock"]) _ = Just Info
  severityFor (Namespace _ ["DidntAdoptBlock"]) _ = Just Error
  severityFor (Namespace _ ["ForgedInvalidBlock"]) _ = Just Error
  severityFor (Namespace _ ["AdoptedBlock"]) _ = Just Info
  severityFor _ _ = Nothing

  metricsDocFor (Namespace _ ["StartLeadershipCheck"]) =
    [("Forge.AboutToLeadSlotLast", "")]
  metricsDocFor (Namespace _ ["SlotIsImmutable"]) =
    [("Forge.SlotIsImmutable", "")]
  metricsDocFor (Namespace _ ["BlockFromFuture"]) =
    [("Forge.BlockFromFuture", "")]
  metricsDocFor (Namespace _ ["BlockContext"]) =
    [("Forge.BlockContext", "")]
  metricsDocFor (Namespace _ ["NoLedgerState"]) =
    [("Forge.CouldNotForgeSlotLast", "")]
  metricsDocFor (Namespace _ ["LedgerState"]) =
    [("Forge.LedgerState", "")]
  metricsDocFor (Namespace _ ["NoLedgerView"]) =
    [("Forge.CouldNotForgeSlotLast", "")]
  metricsDocFor (Namespace _ ["LedgerView"]) =
    [("Forge.LedgerView", "")]
  metricsDocFor (Namespace _ ["ForgeStateUpdateError"]) =
    [ ("Forge.OperationalCertificateStartKESPeriod", "")
    , ("Forge.OperationalCertificateExpiryKESPeriod", "")
    , ("Forge.CurrentKESPeriod", "")
    , ("Forge.RemainingKESPeriods", "")
    ]
  metricsDocFor (Namespace _ ["NodeCannotForge"]) =
    [("Forge.NodeCannotForge", "")]
  metricsDocFor (Namespace _ ["NodeNotLeader"]) =
    [("Forge.NodeNotLeader", "")]
  metricsDocFor (Namespace _ ["NodeIsLeader"]) =
    [("Forge.NodeIsLeader", "")]
  metricsDocFor (Namespace _ ["ForgeTickedLedgerState"]) = []
  metricsDocFor (Namespace _ ["ForgingMempoolSnapshot"]) = []
  metricsDocFor (Namespace _ ["ForgedBlock"]) =
    [("Forge.ForgedSlotLast", "")]
  metricsDocFor (Namespace _ ["DidntAdoptBlock"]) =
    [("Forge.NotAdoptedSlotLast", "")]
  metricsDocFor (Namespace _ ["ForgedInvalidBlock"]) =
    [("Forge.ForgedInvalidSlotLast", "")]
  metricsDocFor (Namespace _ ["AdoptedBlock"]) =
    [("Forge.AdoptedOwnBlockSlotLast", "")]
  metricsDocFor _ = []

  documentFor (Namespace _ ["StartLeadershipCheck"]) = Just
    "Start of the leadership check."
  documentFor (Namespace _ ["SlotIsImmutable"]) = Just
    "Leadership check failed: the tip of the ImmutableDB inhabits the\
        \  current slot\
        \ \
        \  This might happen in two cases.\
        \ \
        \   1. the clock moved backwards, on restart we ignored everything from the\
        \      VolatileDB since it's all in the future, and now the tip of the\
        \      ImmutableDB points to a block produced in the same slot we're trying\
        \      to produce a block in\
        \ \
        \   2. k = 0 and we already adopted a block from another leader of the same\
        \      slot.\
        \ \
        \  We record both the current slot number as well as the tip of the\
        \  ImmutableDB.\
        \ \
        \ See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
  documentFor (Namespace _ ["BlockFromFuture"]) = Just
    "Leadership check failed: the current chain contains a block from a slot\
      \  /after/ the current slot\
      \ \
      \  This can only happen if the system is under heavy load.\
      \ \
      \  We record both the current slot number as well as the slot number of the\
      \  block at the tip of the chain.\
      \ \
      \  See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
  documentFor (Namespace _ ["BlockContext"]) = Just
    "We found out to which block we are going to connect the block we are about\
      \  to forge.\
      \ \
      \  We record the current slot number, the block number of the block to\
      \  connect to and its point.\
      \ \
      \  Note that block number of the block we will try to forge is one more than\
      \  the recorded block number."
  documentFor (Namespace _ ["NoLedgerState"]) = Just
    "Leadership check failed: we were unable to get the ledger state for the\
      \  point of the block we want to connect to\
      \ \
      \  This can happen if after choosing which block to connect to the node\
      \  switched to a different fork. We expect this to happen only rather\
      \  rarely, so this certainly merits a warning; if it happens a lot, that\
      \  merits an investigation.\
      \ \
      \  We record both the current slot number as well as the point of the block\
      \  we attempt to connect the new block to (that we requested the ledger\
      \  state for)."
  documentFor (Namespace _ ["LedgerState"]) = Just
     "We obtained a ledger state for the point of the block we want to\
      \  connect to\
      \ \
      \  We record both the current slot number as well as the point of the block\
      \  we attempt to connect the new block to (that we requested the ledger\
      \  state for)."
  documentFor (Namespace _ ["NoLedgerView"]) = Just
    "Leadership check failed: we were unable to get the ledger view for the\
      \  current slot number\
      \ \
      \  This will only happen if there are many missing blocks between the tip of\
      \  our chain and the current slot.\
      \ \
      \  We record also the failure returned by 'forecastFor'."
  documentFor (Namespace _ ["LedgerView"]) = Just
    "We obtained a ledger view for the current slot number\
      \ \
      \  We record the current slot number."
  documentFor (Namespace _ ["ForgeStateUpdateError"]) =  Just
    "Updating the forge state failed.\
      \ \
      \  For example, the KES key could not be evolved anymore.\
      \ \
      \  We record the error returned by 'updateForgeState'."
  documentFor (Namespace _ ["NodeCannotForge"]) =  Just
   "We did the leadership check and concluded that we should lead and forge\
      \  a block, but cannot.\
      \ \
      \  This should only happen rarely and should be logged with warning severity.\
      \ \
      \  Records why we cannot forge a block."
  documentFor (Namespace _ ["NodeNotLeader"]) =  Just
    "We did the leadership check and concluded we are not the leader\
      \ \
      \  We record the current slot number"
  documentFor (Namespace _ ["NodeIsLeader"]) =  Just
    "We did the leadership check and concluded we /are/ the leader\
      \\n\
      \  The node will soon forge; it is about to read its transactions from the\
      \  Mempool. This will be followed by ForgedBlock."
  documentFor (Namespace _ ["ForgeTickedLedgerState"]) = Just ""
  documentFor (Namespace _ ["ForgingMempoolSnapshot"]) = Just ""
  documentFor (Namespace _ ["ForgedBlock"]) = Just
    "We forged a block.\
      \\n\
      \  We record the current slot number, the point of the predecessor, the block\
      \  itself, and the total size of the mempool snapshot at the time we produced\
      \  the block (which may be significantly larger than the block, due to\
      \  maximum block size)\
      \\n\
      \  This will be followed by one of three messages:\
      \\n\
      \  * AdoptedBlock (normally)\
      \\n\
      \  * DidntAdoptBlock (rarely)\
      \\n\
      \  * ForgedInvalidBlock (hopefully never, this would indicate a bug)"
  documentFor (Namespace _ ["DidntAdoptBlock"]) = Just
    "We did not adopt the block we produced, but the block was valid. We\
      \  must have adopted a block that another leader of the same slot produced\
      \  before we got the chance of adopting our own block. This is very rare,\
      \  this warrants a warning."
  documentFor (Namespace _ ["ForgedInvalidBlock"]) = Just
    "We forged a block that is invalid according to the ledger in the\
      \  ChainDB. This means there is an inconsistency between the mempool\
      \  validation and the ledger validation. This is a serious error!"
  documentFor (Namespace _ ["AdoptedBlock"]) = Just
    "We adopted the block we produced, we also trace the transactions\
       \  that were adopted."
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["StartLeadershipCheck"]
    , Namespace [] ["SlotIsImmutable"]
    , Namespace [] ["BlockFromFuture"]
    , Namespace [] ["BlockContext"]
    , Namespace [] ["NoLedgerState"]
    , Namespace [] ["LedgerState"]
    , Namespace [] ["NoLedgerView"]
    , Namespace [] ["LedgerView"]
    , Namespace [] ["ForgeStateUpdateError"]
    , Namespace [] ["NodeCannotForge"]
    , Namespace [] ["NodeNotLeader"]
    , Namespace [] ["NodeIsLeader"]
    , Namespace [] ["ForgeTickedLedgerState"]
    , Namespace [] ["ForgingMempoolSnapshot"]
    , Namespace [] ["ForgedBlock"]
    , Namespace [] ["DidntAdoptBlock"]
    , Namespace [] ["ForgedInvalidBlock"]
    , Namespace [] ["AdoptedBlock"]
    ]

--------------------------------------------------------------------------------
-- BlockchainTimeEvent Tracer
--------------------------------------------------------------------------------

instance Show t => LogFormatting (TraceBlockchainTimeEvent t) where
    forMachine _dtal (TraceStartTimeInTheFuture (SystemStart start) toWait) =
        mconcat [ "kind" .= String "TStartTimeInTheFuture"
                 , "systemStart" .= String (showT start)
                 , "toWait" .= String (showT toWait)
                 ]
    forMachine _dtal (TraceCurrentSlotUnknown time _) =
        mconcat [ "kind" .= String "CurrentSlotUnknown"
                 , "time" .= String (showT time)
                 ]
    forMachine _dtal (TraceSystemClockMovedBack prevTime newTime) =
        mconcat [ "kind" .= String "SystemClockMovedBack"
                 , "prevTime" .= String (showT prevTime)
                 , "newTime" .= String (showT newTime)
                 ]
    forHuman (TraceStartTimeInTheFuture (SystemStart start) toWait) =
      "Waiting "
      <> (Text.pack . show) toWait
      <> " until genesis start time at "
      <> (Text.pack . show) start
    forHuman (TraceCurrentSlotUnknown time _) =
      "Too far from the chain tip to determine the current slot number for the time "
       <> (Text.pack . show) time
    forHuman (TraceSystemClockMovedBack prevTime newTime) =
      "The system wall clock time moved backwards, but within our tolerance "
      <> "threshold. Previous 'current' time: "
      <> (Text.pack . show) prevTime
      <> ". New 'current' time: "
      <> (Text.pack . show) newTime

instance MetaTrace (TraceBlockchainTimeEvent t) where
  namespaceFor TraceStartTimeInTheFuture {} = Namespace [] ["StartTimeInTheFuture"]
  namespaceFor TraceCurrentSlotUnknown {} = Namespace [] ["CurrentSlotUnknown"]
  namespaceFor TraceSystemClockMovedBack {} = Namespace [] ["SystemClockMovedBack"]

  severityFor (Namespace _ ["StartTimeInTheFuture"]) _ = Just Warning
  severityFor (Namespace _ ["CurrentSlotUnknown"]) _ = Just Warning
  severityFor (Namespace _ ["SystemClockMovedBack"]) _ = Just Warning
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["StartTimeInTheFuture"]) = Just
    "The start time of the blockchain time is in the future\
      \\n\
      \ We have to block (for 'NominalDiffTime') until that time comes."
  documentFor (Namespace _ ["CurrentSlotUnknown"]) = Just
    "Current slot is not yet known\
      \\n\
      \ This happens when the tip of our current chain is so far in the past that\
      \ we cannot translate the current wallclock to a slot number, typically\
      \ during syncing. Until the current slot number is known, we cannot\
      \ produce blocks. Seeing this message during syncing therefore is\
      \ normal and to be expected.\
      \\n\
      \ We record the current time (the time we tried to translate to a 'SlotNo')\
      \ as well as the 'PastHorizonException', which provides detail on the\
      \ bounds between which we /can/ do conversions. The distance between the\
      \ current time and the upper bound should rapidly decrease with consecutive\
      \ 'CurrentSlotUnknown' messages during syncing."
  documentFor (Namespace _ ["SystemClockMovedBack"]) = Just
    "The system clock moved back an acceptable time span, e.g., because of\
      \ an NTP sync.\
      \\n\
      \ The system clock moved back such that the new current slot would be\
      \ smaller than the previous one. If this is within the configured limit, we\
      \ trace this warning but *do not change the current slot*. The current slot\
      \ never decreases, but the current slot may stay the same longer than\
      \ expected.\
      \\n\
      \ When the system clock moved back more than the configured limit, we shut\
      \ down with a fatal exception."
  documentFor _ = Nothing

  allNamespaces =
    [
      Namespace [] ["StartTimeInTheFuture"]
    , Namespace [] ["CurrentSlotUnknown"]
    , Namespace [] ["SystemClockMovedBack"]
    ]

--------------------------------------------------------------------------------
-- KeepAliveClient Tracer
--------------------------------------------------------------------------------

instance Show remotePeer => LogFormatting (TraceKeepAliveClient remotePeer) where
    forMachine _dtal (AddSample peer rtt pgsv) =
        mconcat
          [ "kind" .= String "AddSample"
          , "address" .= show peer
          , "rtt" .= rtt
          , "sampleTime" .= show (dTime $ sampleTime pgsv)
          , "outboundG" .= (realToFrac $ gGSV (outboundGSV pgsv) :: Double)
          , "inboundG" .= (realToFrac $ gGSV (inboundGSV pgsv) :: Double)
          ]
        where
          gGSV :: GSV -> DiffTime
          gGSV (GSV g _ _) = g

          dTime :: Time -> Double
          dTime (Time d) = realToFrac d

    forHuman = showT

instance MetaTrace (TraceKeepAliveClient remotePeer) where
  namespaceFor AddSample {} = Namespace [] ["KeepAliveClient"]

  severityFor (Namespace _ ["KeepAliveClient"]) Nothing = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["KeepAliveClient"]) = Just
    "A server read has occurred, either for an add block or a rollback"
  documentFor _ = Just ""

  allNamespaces = [Namespace [] ["KeepAliveClient"]]



