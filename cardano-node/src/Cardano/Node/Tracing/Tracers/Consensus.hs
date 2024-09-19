{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}
{-# LANGUAGE InstanceSigs #-}

module Cardano.Node.Tracing.Tracers.Consensus
  (
    TraceStartLeadershipCheckPlus (..)
  , ForgeTracerType
  , forgeTracerTransform
  , initialClientMetrics
  , calculateBlockFetchClientMetrics
  , ClientMetrics
  ) where


import           Cardano.Logging
import           Cardano.Node.Queries (HasKESInfo (..))
import           Cardano.Node.Tracing.Era.Byron ()
import           Cardano.Node.Tracing.Era.Shelley ()
import           Cardano.Node.Tracing.Formatting ()
import           Cardano.Node.Tracing.Render
import           Cardano.Node.Tracing.Tracers.ConsensusStartupException ()
import           Cardano.Node.Tracing.Tracers.StartLeadershipCheck
import           Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Genesis.Governor (DensityBounds (..), TraceGDDEvent (..))
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent (..), LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTxId, HasTxId,
                   LedgerSupportsMempool, txForgetValidated, txId)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool (MempoolSize (..), TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                   (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping (Instruction (..),
                   JumpInstruction (..), JumpResult (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State (JumpInfo (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                   (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.GSM
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints, estimateBlockSize)
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Util.Enclose
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block hiding (blockPrevHash)
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.SizeInBytes (SizeInBytes (..))
import           Ouroboros.Network.TxSubmission.Inbound hiding (txId)
import           Ouroboros.Network.TxSubmission.Outbound

import           Control.Monad (guard)
import           Control.Monad.Class.MonadTime.SI (Time (..))
import           Data.Aeson (ToJSON, Value (Number, String), toJSON, (.=))
import qualified Data.Aeson as Aeson
import           Data.Int (Int64)
import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as Pq
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Time (DiffTime, NominalDiffTime)
import           Data.Word (Word32, Word64)

instance (LogFormatting adr, Show adr) => LogFormatting (ConnectionId adr) where
  forMachine _dtal (ConnectionId local' remote) =
    mconcat [ "connectionId" .= String (showT local'
                                          <> " "
                                          <> showT remote)
    ]
  forHuman (ConnectionId local' remote) =
    "ConnectionId " <>  showT local' <> " " <> showT remote

--------------------------------------------------------------------------------
--   TraceLabelCreds peer a
--------------------------------------------------------------------------------

instance LogFormatting a => LogFormatting (TraceLabelCreds a) where
  forMachine dtal (TraceLabelCreds creds a)  =
    mconcat $ ("credentials" .= toJSON creds) : [forMachine dtal a]

  forHuman (TraceLabelCreds creds a)         =
    "With label " <> (Text.pack . show) creds <> ", " <> forHumanOrMachine a
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
  forHuman (TraceLabelPeer peerid a) = "Peer is: (" <> showT peerid
                                        <> "). " <> forHumanOrMachine a
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
  forHuman = \case
    TraceDownloadedHeader pt ->
      mconcat
        [ "While following a candidate chain, we rolled forward by downloading a"
        , " header. "
        , showT (headerPoint pt)
        ]
    TraceRolledBack tip ->
      "While following a candidate chain, we rolled back to the given point: " <> showT tip
    TraceException exc ->
      "An exception was thrown by the Chain Sync Client. " <> showT exc
    TraceFoundIntersection {} ->
      mconcat
        [ "We found an intersection between our chain fragment and the"
        , " candidate's chain."
        ]
    TraceTermination res ->
      "The client has terminated. " <> showT res
    TraceValidatedHeader header ->
      "The header has been validated" <> showT (headerHash header)
    TraceWaitingBeyondForecastHorizon slotNo ->
      mconcat
        [ "The slot number " <> showT slotNo <> " is beyond the forecast horizon, the ChainSync client"
        , " cannot yet validate a header in this slot and therefore is waiting"
        ]
    TraceAccessingForecastHorizon slotNo ->
      mconcat
        [ "The slot number " <> showT slotNo <> ", which was previously beyond the forecast horizon, has now"
        , " entered it, and we can resume processing."
        ]
    TraceGaveLoPToken {} ->
      mconcat
        [ "Whether we added a token to the LoP bucket of the peer. Also carries"
        , "the considered header and the best block number known prior to this"
        , "header"
        ]
    TraceOfferJump point ->
      mconcat
        [ "ChainSync Jumping -- we are offering a jump to the server, to point: "
        , showT point
        ]
    TraceJumpResult (AcceptedJump instruction) ->
      mconcat
        [ "ChainSync Jumping -- the client accepted the jump to "
        , showT (jumpInstructionToPoint instruction)
        ]
    TraceJumpResult (RejectedJump instruction) ->
      mconcat
        [ "ChainSync Jumping -- the client rejected the jump to "
        , showT (jumpInstructionToPoint instruction)
        ]
    TraceJumpingWaitingForNextInstruction ->
      "ChainSync Jumping -- the client is blocked, waiting for its next instruction."
    TraceJumpingInstructionIs RunNormally ->
      "ChainSyncJumping -- the client is asked to run normally"
    TraceJumpingInstructionIs Restart ->
      mconcat
        [ "ChainSyncJumping -- the client is asked to restart. This is necessary"
        , "when disengaging a peer of which we know no point that we could set"
        , "the intersection of the ChainSync server to."
        ]
    TraceJumpingInstructionIs (JumpInstruction instruction) ->
      mconcat
        [ "ChainSync Jumping -- the client is asked to jump to "
        , showT (jumpInstructionToPoint instruction)
        ]
    where
      jumpInstructionToPoint = AF.headPoint . jTheirFragment . \case
        JumpTo ji          -> ji
        JumpToGoodPoint ji -> ji

  forMachine dtal = \case
    TraceDownloadedHeader h ->
      mconcat
        [ "kind" .= String "DownloadedHeader"
        , tipToObject (tipFromHeader h)
        ]
    TraceRolledBack tip ->
      mconcat
        [ "kind" .= String "RolledBack"
        , "tip" .= forMachine dtal tip
        ]
    TraceException exc ->
      mconcat
        [ "kind" .= String "Exception"
        , "exception" .= String (Text.pack $ show exc)
        ]
    TraceFoundIntersection {} ->
      mconcat
        [ "kind" .= String "FoundIntersection"
        ]
    TraceTermination reason ->
      mconcat
        [ "kind" .= String "Termination"
        , "reason" .= String (Text.pack $ show reason)
        ]
    TraceValidatedHeader header ->
      mconcat
        [ "kind" .= String "ValidatedHeader"
        , "headerHash" .= showT (headerHash header)
        ]
    TraceWaitingBeyondForecastHorizon slotNo ->
      mconcat
        [ "kind" .= String "WaitingBeyondForecastHorizon"
        , "slotNo" .= slotNo
        ]
    TraceAccessingForecastHorizon slotNo ->
      mconcat
        [ "kind" .= String "AccessingForecastHorizon"
        , "slotNo" .= slotNo
        ]
    TraceGaveLoPToken tokenAdded header aBlockNo ->
      mconcat
        [ "kind" .= String "TraceGaveLoPToken"
        , "tokenAdded" .= tokenAdded
        , "headerHash" .= showT (headerHash header)
        , "blockNo" .= aBlockNo
        ]
    TraceOfferJump point ->
      mconcat
        [ "kind" .= String "TraceOfferJump"
        , "point" .= showT point
        ]
    TraceJumpResult jumpResult ->
      mconcat
        [ "kind" .= String "TraceJumpResult"
        , "result" .= case jumpResult of
            AcceptedJump _ -> String "AcceptedJump"
            RejectedJump _ -> String "RejectedJump"
        ]
    TraceJumpingWaitingForNextInstruction ->
      mconcat
        [ "kind" .= String "TraceJumpingWaitingForNextInstruction"
        ]
    TraceJumpingInstructionIs instruction ->
      mconcat
        [ "kind" .= String "TraceJumpingInstructionIs"
        , "instr" .= instructionToObject instruction
        ]
    where
      instructionToObject :: Instruction blk -> Aeson.Object
      instructionToObject = \case
        RunNormally ->
          mconcat ["kind" .= String "RunNormally"]
        Restart ->
          mconcat ["kind" .= String "Restart"]
        JumpInstruction info ->
          mconcat [ "kind" .= String "JumpInstruction"
                  , "payload" .= jumpInstructionToObject info
                  ]

      jumpInstructionToObject :: JumpInstruction blk -> Aeson.Object
      jumpInstructionToObject = \case
        JumpTo info ->
          mconcat [ "kind" .= String "JumpTo"
                  , "point" .= showT (jumpInfoToPoint info) ]
        JumpToGoodPoint info ->
          mconcat [ "kind" .= String "JumpToGoodPoint"
                  , "point" .= showT (jumpInfoToPoint info) ]

      jumpInfoToPoint = AF.headPoint . jTheirFragment

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
  namespaceFor = \case
    TraceDownloadedHeader {} ->
      Namespace [] ["DownloadedHeader"]
    TraceRolledBack {} ->
      Namespace [] ["RolledBack"]
    TraceException {} ->
      Namespace [] ["Exception"]
    TraceFoundIntersection {} ->
      Namespace [] ["FoundIntersection"]
    TraceTermination {} ->
      Namespace [] ["Termination"]
    TraceValidatedHeader {} ->
      Namespace [] ["ValidatedHeader"]
    TraceWaitingBeyondForecastHorizon {} ->
      Namespace [] ["WaitingBeyondForecastHorizon"]
    TraceAccessingForecastHorizon {} ->
      Namespace [] ["AccessingForecastHorizon"]
    TraceGaveLoPToken {} ->
      Namespace [] ["GaveLoPToken"]
    TraceOfferJump _ ->
      Namespace [] ["OfferJump"]
    TraceJumpResult _ ->
      Namespace [] ["JumpResult"]
    TraceJumpingWaitingForNextInstruction ->
      Namespace [] ["JumpingWaitingForNextInstruction"]
    TraceJumpingInstructionIs _ ->
      Namespace [] ["JumpingInstructionIs"]

  severityFor ns _ =
    case ns of
      Namespace _ ["DownloadedHeader"] ->
        Just Info
      Namespace _ ["RolledBack"] ->
        Just Notice
      Namespace _ ["Exception"] ->
        Just Warning
      Namespace _ ["FoundIntersection"] ->
        Just Info
      Namespace _ ["Termination"] ->
        Just Notice
      Namespace _ ["ValidatedHeader"] ->
        Just Debug
      Namespace _ ["WaitingBeyondForecastHorizon"] ->
        Just Debug
      Namespace _ ["AccessingForecastHorizon"] ->
        Just Debug
      Namespace _ ["GaveLoPToken"] ->
        Just Debug
      Namespace _ ["OfferJump"] ->
        Just Debug
      Namespace _ ["JumpResult"] ->
        Just Debug
      Namespace _ ["JumpingWaitingForNextInstruction"] ->
        Just Debug
      Namespace _ ["JumpingInstructionIs"] ->
        Just Debug
      _ ->
        Nothing

  documentFor ns =
    case ns of
      Namespace _ ["DownloadedHeader"] ->
        Just $ mconcat
          [ "While following a candidate chain, we rolled forward by downloading a"
          , " header."
          ]
      Namespace _ ["RolledBack"] ->
        Just "While following a candidate chain, we rolled back to the given point."
      Namespace _ ["Exception"] ->
        Just "An exception was thrown by the Chain Sync Client."
      Namespace _ ["FoundIntersection"] ->
        Just $ mconcat
          [ "We found an intersection between our chain fragment and the"
          , " candidate's chain."
          ]
      Namespace _ ["Termination"] ->
        Just "The client has terminated."
      Namespace _ ["ValidatedHeader"] ->
        Just "The header has been validated"
      Namespace _ ["WaitingBeyondForecastHorizon"] ->
        Just "The slot number is beyond the forecast horizon"
      Namespace _ ["AccessingForecastHorizon"] ->
        Just "The slot number, which was previously beyond the forecast horizon, has now entered it"
      Namespace _ ["GaveLoPToken"] ->
        Just "May have added atoken to the LoP bucket of the peer"
      Namespace _ ["OfferJump"] ->
        Just "Offering a jump to the remote peer"
      Namespace _ ["JumpResult"] ->
        Just "Response to a jump offer (accept or reject)"
      Namespace _ ["JumpingWaitingForNextInstruction"] ->
        Just "The client is waiting for the next instruction"
      Namespace _ ["JumpingInstructionIs"] ->
        Just "The client got its next instruction"
      _ ->
        Nothing

  allNamespaces =
    [ Namespace [] ["DownloadedHeader"]
    , Namespace [] ["RolledBack"]
    , Namespace [] ["Exception"]
    , Namespace [] ["FoundIntersection"]
    , Namespace [] ["Termination"]
    , Namespace [] ["ValidatedHeader"]
    , Namespace [] ["WaitingBeyondForecastHorizon"]
    , Namespace [] ["AccessingForecastHorizon"]
    , Namespace [] ["GaveLoPToken"]
    , Namespace [] ["OfferJump"]
    , Namespace [] ["JumpResult"]
    , Namespace [] ["JumpingWaitingForNextInstruction"]
    , Namespace [] ["JumpingInstructionIs"]
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
      [CounterM "headersServed.falling" Nothing]
  asMetrics (TraceChainSyncServerUpdate _tip (AddBlock _pt) _blocking _) =
      [CounterM "headersServed" Nothing]
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
    [ ("headersServed", "A counter triggered on any header event")
    , ("headersServed.falling",
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
               [ DoubleM "blockfetchclient.blockdelay"
                    cmDelay
               , IntM "blockfetchclient.blocksize"
                    (fromIntegral cmBlockSize)
               , DoubleM "blockfetchclient.blockdelay.cdfOne"
                    (fromIntegral (counter cmCdf1sVar) / fromIntegral size)
               , DoubleM "blockfetchclient.blockdelay.cdfThree"
                    (fromIntegral (counter cmCdf3sVar) / fromIntegral size)
               , DoubleM "blockfetchclient.blockdelay.cdfFive"
                    (fromIntegral (counter cmCdf5sVar) / fromIntegral size)
               ]
        in if cmDelay > 5
             then
               CounterM "blockfetchclient.lateblocks" Nothing
                 : msgs
             else msgs
      else []

instance MetaTrace ClientMetrics where
  namespaceFor _ = Namespace [] ["ClientMetrics"]
  severityFor _ _ = Just Info
  documentFor _ = Just ""

  metricsDocFor (Namespace _ ["ClientMetrics"]) =
      [ ("blockfetchclient.blockdelay", "")
      , ("blockfetchclient.blocksize", "")
      , ("blockfetchclient.lateblocks", "")
      , ("blockfetchclient.blockdelay.cdfOne", "")
      , ("blockfetchclient.blockdelay.cdfThree", "")
      , ("blockfetchclient.blockdelay.cdfFive", "")
      ]
  metricsDocFor _ = []

  allNamespaces = [
          Namespace [] ["ClientMetrics"]
        ]

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
                                            , cmBlockSize = getSizeInBytes blockSize
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
                                       , cmBlockSize = getSizeInBytes blockSize
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
      (List.foldl' (\acc x -> forMachine DDetailed x : acc) [] xs) ]

  asMetrics peers = [IntM "connectedPeers" (fromIntegral (length peers))]

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
      [("connectedPeers", "Number of connected peers")]
    metricsDocFor (Namespace _ ["Accept"]) =
      [("connectedPeers", "Number of connected peers")]
    metricsDocFor _ = []

    documentFor _ =  Just $ mconcat
      [ "Throughout the decision making process we accumulate reasons to decline"
      , " to fetch any blocks. This message carries the intermediate and final"
      , " results."
      ]
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
              , "length" .= toJSON (fragmentLength' af)]
        where
          -- NOTE: this ignores the Byron era with its EBB complication:
          -- the length would be underestimated by 1, if the AF is anchored
          -- at the epoch boundary.
          fragmentLength' :: AF.AnchoredFragment header -> Int
          fragmentLength' f = fromIntegral . unBlockNo $
              case (f, f) of
                (AS.Empty{}, AS.Empty{}) -> 0
                (firstHdr AS.:< _, _ AS.:> lastHdr) ->
                  blockNo lastHdr - blockNo firstHdr + 1
    forMachine _dtal (BlockFetch.CompletedBlockFetch pt _ _ _ delay blockSize) =
      mconcat [ "kind"  .= String "CompletedBlockFetch"
              , "delay" .= (realToFrac delay :: Double)
              , "size"  .= getSizeInBytes blockSize
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

    documentFor (Namespace _ ["AddedFetchRequest"]) = Just $ mconcat
      [ "The block fetch decision thread has added a new fetch instruction"
      , " consisting of one or more individual request ranges."
      ]
    documentFor (Namespace _ ["AcknowledgedFetchRequest"]) = Just $ mconcat
      [ "Mark the point when the fetch client picks up the request added"
      , " by the block fetch decision thread. Note that this event can happen"
      , " fewer times than the 'AddedFetchRequest' due to fetch request merging."
      ]
    documentFor (Namespace _ ["SendFetchRequest"]) = Just $ mconcat
      [ "Mark the point when fetch request for a fragment is actually sent"
      , " over the wire."
      ]
    documentFor (Namespace _ ["StartedFetchBatch"]) = Just $ mconcat
      [ "Mark the start of receiving a streaming batch of blocks. This will"
      , " be followed by one or more 'CompletedBlockFetch' and a final"
      , " 'CompletedFetchBatch'"
      ]
    documentFor (Namespace _ ["CompletedFetchBatch"]) = Just
      "Mark the successful end of receiving a streaming batch of blocks."
    documentFor (Namespace _ ["CompletedBlockFetch"]) = Just
      ""
    documentFor (Namespace _ ["RejectedFetchBatch"]) = Just $ mconcat
      [ "If the other peer rejects our request then we have this event"
      , " instead of 'StartedFetchBatch' and 'CompletedFetchBatch'."
      ]
    documentFor (Namespace _ ["ClientTerminating"]) = Just $ mconcat
      [ "The client is terminating.  Log the number of outstanding"
      , " requests."
      ]
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
    [CounterM "served.block" Nothing]

instance MetaTrace (TraceBlockFetchServerEvent blk) where
    namespaceFor TraceBlockFetchServerSendBlock {} =
      Namespace [] ["SendBlock"]

    severityFor (Namespace [] ["SendBlock"]) _ = Just
      Info
    severityFor _ _ = Nothing

    metricsDocFor (Namespace [] ["SendBlock"]) =
      [("served.block", "")]
    metricsDocFor _ = []

    documentFor (Namespace [] ["SendBlock"]) = Just
      "The server sent a block to the peer."
    documentFor _ = Nothing

    allNamespaces = [Namespace [] ["SendBlock"]]

--------------------------------------------------------------------------------
-- Gdd Tracer
--------------------------------------------------------------------------------

instance ( LogFormatting peer
         , HasHeader blk
         , HasHeader (Header blk)
         , ConvertRawHash (Header blk)
         ) => LogFormatting (TraceGDDEvent peer blk) where
  forMachine dtal TraceGDDEvent {..} = mconcat $
    [ "kind" .= String "TraceGDDEvent"
    , "losingPeers".= toJSON (map (forMachine dtal) losingPeers)
    , "loeHead" .= forMachine dtal loeHead
    , "sgen" .= toJSON (unGenesisWindow sgen)
    ] <> do
      guard $ dtal >= DMaximum
      [ "bounds" .= toJSON (
           map
           ( \(peer, density) -> Aeson.object
             [ "kind" .= String "PeerDensityBound"
             , "peer" .= forMachine dtal peer
             , "densityBounds" .= forMachine dtal density
             ]
           )
           bounds
         )
       , "curChain" .= forMachine dtal curChain
       , "candidates" .= toJSON (
           map
           ( \(peer, frag) -> Aeson.object
             [ "kind" .= String "PeerCandidateFragment"
             , "peer" .= forMachine dtal peer
             , "candidateFragment" .= forMachine dtal frag
             ]
           )
           candidates
         )
       , "candidateSuffixes" .= toJSON (
           map
           ( \(peer, frag) -> Aeson.object
             [ "kind" .= String "PeerCandidateSuffix"
             , "peer" .= forMachine dtal peer
             , "candidateSuffix" .= forMachine dtal frag
             ]
           )
           candidateSuffixes
         )
       ]

  forHuman = forHumanOrMachine

instance MetaTrace (TraceGDDEvent peer blk) where
  namespaceFor _ = Namespace [] ["TraceGDDEvent"]

  severityFor _ _ = Just Debug

  documentFor _ = Just "The Genesis Density Disconnection governor has updated its state"

  allNamespaces = [Namespace [] ["TraceGDDEvent"]]

instance ( HasHeader blk
         , HasHeader (Header blk)
         , ConvertRawHash (Header blk)
         ) => LogFormatting (DensityBounds blk) where
  forMachine dtal DensityBounds {..} = mconcat
    [ "kind" .= String "DensityBounds"
    , "clippedFragment" .= forMachine dtal clippedFragment
    , "offersMoreThanK" .= toJSON offersMoreThanK
    , "lowerBound" .= toJSON lowerBound
    , "upperBound" .= toJSON upperBound
    , "hasBlockAfter" .= toJSON hasBlockAfter
    , "latestSlot" .= toJSON (unSlotNo <$> withOriginToMaybe latestSlot)
    , "idling" .= toJSON idling
    ]

  forHuman = forHumanOrMachine


--------------------------------------------------------------------------------
-- SanityCheckIssue Tracer
--------------------------------------------------------------------------------

instance MetaTrace SanityCheckIssue where

  namespaceFor InconsistentSecurityParam {} = Namespace [] ["SanityCheckIssue"]

  severityFor (Namespace _ ["SanityCheckIssue"]) _ = Just Error
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["SanityCheckIssue"]) = Nothing
  documentFor _ = Nothing

  allNamespaces = [Namespace [] ["SanityCheckIssue"]]

instance LogFormatting SanityCheckIssue where
  forMachine _dtal (InconsistentSecurityParam e) =
    mconcat [ "kind" .= String "InconsistentSecurityParam"
            , "error" .= String (Text.pack $ show e)
            ]
  forHuman (InconsistentSecurityParam e) =
    "Configuration contains multiple security parameters: " <> Text.pack (show e)


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
    [CounterM "submissions.submitted" (Just count)]
  asMetrics (TraceTxSubmissionProcessed processed) =
    [ CounterM "submissions.accepted"
        (Just (ptxcAccepted processed))
    , CounterM "submissions.rejected"
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
      [ ("submissions.submitted", "")]
    metricsDocFor (Namespace _ ["Processed"]) =
      [ ("submissions.accepted", "")
      , ("submissions.rejected", "")
      ]
    metricsDocFor _ = []

    documentFor (Namespace _ ["Collected"]) = Just
      "Number of transactions just about to be inserted."
    documentFor (Namespace _ ["Processed"]) = Just
      "Just processed transaction pass/fail breakdown."
    documentFor (Namespace _ ["Terminated"]) = Just
      "Server received 'MsgDone'."
    documentFor (Namespace _ ["CanRequestMoreTxs"]) = Just $ mconcat
      [ "There are no replies in flight, but we do know some more txs we"
      , " can ask for, so lets ask for them and more txids."
      ]
    documentFor (Namespace _ ["CannotRequestMoreTxs"]) = Just $ mconcat
      [ "There's no replies in flight, and we have no more txs we can"
      , " ask for so the only remaining thing to do is to ask for more"
      , " txids. Since this is the only thing to do now, we make this a"
      , " blocking call."
      ]
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
    mconcat $
      [ "kind" .= String "TraceMempoolRejectedTx"
      , "tx" .= forMachine dtal tx
      , "mempoolSize" .= forMachine dtal mpSz
      ] <>
      [ "err" .= forMachine dtal txApplyErr
      | dtal >= DDetailed
      ]
  forMachine dtal (TraceMempoolRemoveTxs txs mpSz) =
    mconcat
      [ "kind" .= String "TraceMempoolRemoveTxs"
      , "txs"
          .= map
            ( \(tx, err) ->
                Aeson.object $
                  [ "tx" .= forMachine dtal (txForgetValidated tx)
                  ] <>
                  [ "err" .= forMachine dtal err
                  | dtal >= DDetailed
                  ]
            )
            txs
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
    [ IntM "txsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "mempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolRejectedTx _tx _txApplyErr mpSz) =
    [ IntM "txsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "mempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolRemoveTxs _txs mpSz) =
    [ IntM "txsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "mempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolManuallyRemovedTxs [] _txs1 mpSz) =
    [ IntM "txsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "mempoolBytes" (fromIntegral $ msNumBytes mpSz)
    ]
  asMetrics (TraceMempoolManuallyRemovedTxs txs _txs1 mpSz) =
    [ IntM "txsInMempool" (fromIntegral $ msNumTxs mpSz)
    , IntM "mempoolBytes" (fromIntegral $ msNumBytes mpSz)
    , CounterM "txsProcessedNum" (Just (fromIntegral $ length txs))
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
      [ ("txsInMempool","Transactions in mempool")
      , ("mempoolBytes", "Byte size of the mempool")
      ]
    metricsDocFor (Namespace _ ["RejectedTx"]) =
      [ ("txsInMempool","Transactions in mempool")
      , ("mempoolBytes", "Byte size of the mempool")
      ]
    metricsDocFor (Namespace _ ["RemoveTxs"]) =
      [ ("txsInMempool","Transactions in mempool")
      , ("mempoolBytes", "Byte size of the mempool")
      ]
    metricsDocFor (Namespace _ ["ManuallyRemovedTxs"]) =
      [ ("txsInMempool","Transactions in mempool")
      , ("mempoolBytes", "Byte size of the mempool")
      , ("txsProcessedNum", "")
      ]
    metricsDocFor _ = []

    documentFor (Namespace _ ["AddedTx"]) = Just
      "New, valid transaction that was added to the Mempool."
    documentFor (Namespace _ ["RejectedTx"]) = Just $ mconcat
      [ "New, invalid transaction thas was rejected and thus not added to"
      , " the Mempool."
      ]
    documentFor (Namespace _ ["RemoveTxs"]) = Just $ mconcat
      [ "Previously valid transactions that are no longer valid because of"
      , " changes in the ledger state. These transactions have been removed"
      , " from the Mempool."
      ]
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
  forHuman (Left i)  = forHumanOrMachine i
  forHuman (Right i) = forHumanOrMachine i
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

  documentFor (Namespace _ ["StartLeadershipCheckPlus"]) = Just $ mconcat
    [ "We adopted the block we produced, we also trace the transactions"
    , "  that were adopted."
    ]
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
                , "delegMapSize" .= Number (fromIntegral tsDelegMapSize)
                , "chainDensity" .= Number (fromRational (toRational tsChainDensity))
                ]
  forHuman TraceStartLeadershipCheckPlus {..} =
      "Checking for leadership in slot " <> showT (unSlotNo tsSlotNo)
      <> " utxoSize "     <> showT tsUtxoSize
      <> " delegMapSize " <> showT tsDelegMapSize
      <> " chainDensity " <> showT tsChainDensity
  asMetrics TraceStartLeadershipCheckPlus {..} =
    [IntM "utxoSize"     (fromIntegral tsUtxoSize),
     IntM "delegMapSize" (fromIntegral tsDelegMapSize)]


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
      , "blockSize" .= toJSON (getSizeInBytes $ estimateBlockSize (getHeader blk))
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
      , "blockSize" .= toJSON (getSizeInBytes $ estimateBlockSize (getHeader blk))
      ]
  forMachine dtal (TraceAdoptionThreadDied slotNo blk) =
    mconcat
      [ "kind" .= String "TraceAdoptionThreadDied"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "blockHash" .= renderHeaderHashForDetails
          (Proxy @blk)
          dtal
          (blockHash blk)
      , "blockSize" .= toJSON (getSizeInBytes $ estimateBlockSize (getHeader blk))
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
  forHuman (TraceAdoptionThreadDied slotNo blk) =
      "Adoption thread died in slot "
        <> showT (unSlotNo slotNo)
        <> ": " <> renderHeaderHash (Proxy @blk) (blockHash blk)

  asMetrics (TraceForgeStateUpdateError slot reason) =
    IntM "Forge.StateUpdateError" (fromIntegral $ unSlotNo slot) :
      (case getKESInfo (Proxy @blk) reason of
        Nothing -> []
        Just kesInfo ->
          [ IntM
              "operationalCertificateStartKESPeriod"
              (fromIntegral . unKESPeriod . HotKey.kesStartPeriod $ kesInfo)
          , IntM
              "operationalCertificateExpiryKESPeriod"
              (fromIntegral . unKESPeriod . HotKey.kesEndPeriod $ kesInfo)
          , IntM
              "currentKESPeriod"
              0
          , IntM
              "remainingKESPeriods"
              0
          ])

  asMetrics (TraceStartLeadershipCheck slot) =
    [IntM "aboutToLeadSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceSlotIsImmutable slot _tipPoint _tipBlkNo) =
    [IntM "slotIsImmutable" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceBlockFromFuture slot _slotNo) =
    [IntM "blockFromFuture" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceBlockContext slot _tipBlkNo _tipPoint) =
    [IntM "blockContext" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNoLedgerState slot _) =
    [IntM "couldNotForgeSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceLedgerState slot _) =
    [IntM "ledgerState" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNoLedgerView slot _) =
    [IntM "couldNotForgeSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceLedgerView slot) =
    [IntM "ledgerView" (fromIntegral $ unSlotNo slot)]
  -- see above
  asMetrics (TraceNodeCannotForge slot _reason) =
    [IntM "nodeCannotForge" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNodeNotLeader slot) =
    [IntM "nodeNotLeader" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceNodeIsLeader slot) =
    [IntM "nodeIsLeader" (fromIntegral $ unSlotNo slot)]
  asMetrics TraceForgeTickedLedgerState {} = []
  asMetrics TraceForgingMempoolSnapshot {} = []
  asMetrics (TraceForgedBlock slot _ _ _) =
    [IntM "forgedSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceDidntAdoptBlock slot _) =
    [IntM "notAdoptedSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceForgedInvalidBlock slot _ _) =
    [IntM "forgedInvalidSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceAdoptedBlock slot _ _) =
    [IntM "adoptedOwnBlockSlotLast" (fromIntegral $ unSlotNo slot)]
  asMetrics (TraceAdoptionThreadDied slot _) =
    [IntM "adoptionThreadDied" (fromIntegral $ unSlotNo slot)]

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
  namespaceFor TraceAdoptionThreadDied {} =
    Namespace [] ["AdoptionThreadDied"]

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
  severityFor (Namespace _ ["AdoptionThreadDied"]) _ = Just Error
  severityFor _ _ = Nothing

  metricsDocFor (Namespace _ ["StartLeadershipCheck"]) =
    [("aboutToLeadSlotLast", "")]
  metricsDocFor (Namespace _ ["SlotIsImmutable"]) =
    [("slotIsImmutable", "")]
  metricsDocFor (Namespace _ ["BlockFromFuture"]) =
    [("blockFromFuture", "")]
  metricsDocFor (Namespace _ ["BlockContext"]) =
    [("blockContext", "")]
  metricsDocFor (Namespace _ ["NoLedgerState"]) =
    [("couldNotForgeSlotLast", "")]
  metricsDocFor (Namespace _ ["LedgerState"]) =
    [("ledgerState", "")]
  metricsDocFor (Namespace _ ["NoLedgerView"]) =
    [("couldNotForgeSlotLast", "")]
  metricsDocFor (Namespace _ ["LedgerView"]) =
    [("ledgerView", "")]
  metricsDocFor (Namespace _ ["ForgeStateUpdateError"]) =
    [ ("operationalCertificateStartKESPeriod", "")
    , ("operationalCertificateExpiryKESPeriod", "")
    , ("currentKESPeriod", "")
    , ("remainingKESPeriods", "")
    ]
  metricsDocFor (Namespace _ ["NodeCannotForge"]) =
    [("nodeCannotForge", "")]
  metricsDocFor (Namespace _ ["NodeNotLeader"]) =
    [("nodeNotLeader", "")]
  metricsDocFor (Namespace _ ["NodeIsLeader"]) =
    [("nodeIsLeader", "")]
  metricsDocFor (Namespace _ ["ForgeTickedLedgerState"]) = []
  metricsDocFor (Namespace _ ["ForgingMempoolSnapshot"]) = []
  metricsDocFor (Namespace _ ["ForgedBlock"]) =
    [("forgedSlotLast", "")]
  metricsDocFor (Namespace _ ["DidntAdoptBlock"]) =
    [("notAdoptedSlotLast", "")]
  metricsDocFor (Namespace _ ["ForgedInvalidBlock"]) =
    [("forgedInvalidSlotLast", "")]
  metricsDocFor (Namespace _ ["AdoptedBlock"]) =
    [("adoptedOwnBlockSlotLast", "")]
  metricsDocFor (Namespace _ ["AdoptionThreadDied"]) =
    [("adoptionThreadDied", "")]
  metricsDocFor _ = []

  documentFor (Namespace _ ["StartLeadershipCheck"]) = Just
    "Start of the leadership check."
  documentFor (Namespace _ ["SlotIsImmutable"]) = Just $ mconcat
    [ "Leadership check failed: the tip of the ImmutableDB inhabits the"
    , "  current slot"
    , " "
    , "  This might happen in two cases."
    , " "
    , "   1. the clock moved backwards, on restart we ignored everything from the"
    , "      VolatileDB since it's all in the future, and now the tip of the"
    , "      ImmutableDB points to a block produced in the same slot we're trying"
    , "      to produce a block in"
    , " "
    , "   2. k = 0 and we already adopted a block from another leader of the same"
    , "      slot."
    , " "
    , "  We record both the current slot number as well as the tip of the"
    , "  ImmutableDB."
    , " "
    , " See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
    ]
  documentFor (Namespace _ ["BlockFromFuture"]) = Just $ mconcat
    [ "Leadership check failed: the current chain contains a block from a slot"
    , "  /after/ the current slot"
    , " "
    , "  This can only happen if the system is under heavy load."
    , " "
    , "  We record both the current slot number as well as the slot number of the"
    , "  block at the tip of the chain."
    , " "
    , "  See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
    ]
  documentFor (Namespace _ ["BlockContext"]) = Just $ mconcat
    [ "We found out to which block we are going to connect the block we are about"
    , "  to forge."
    , " "
    , "  We record the current slot number, the block number of the block to"
    , "  connect to and its point."
    , " "
    , "  Note that block number of the block we will try to forge is one more than"
    , "  the recorded block number."
    ]
  documentFor (Namespace _ ["NoLedgerState"]) = Just $ mconcat
    [ "Leadership check failed: we were unable to get the ledger state for the"
    , "  point of the block we want to connect to"
    , " "
    , "  This can happen if after choosing which block to connect to the node"
    , "  switched to a different fork. We expect this to happen only rather"
    , "  rarely, so this certainly merits a warning; if it happens a lot, that"
    , "  merits an investigation."
    , " "
    , "  We record both the current slot number as well as the point of the block"
    , "  we attempt to connect the new block to (that we requested the ledger"
    , "  state for)."
    ]
  documentFor (Namespace _ ["LedgerState"]) = Just $ mconcat
    [ "We obtained a ledger state for the point of the block we want to"
    , "  connect to"
    , " "
    , "  We record both the current slot number as well as the point of the block"
    , "  we attempt to connect the new block to (that we requested the ledger"
    , "  state for)."
    ]
  documentFor (Namespace _ ["NoLedgerView"]) = Just $ mconcat
    [ "Leadership check failed: we were unable to get the ledger view for the"
    , "  current slot number"
    , " "
    , "  This will only happen if there are many missing blocks between the tip of"
    , "  our chain and the current slot."
    , " "
    , "  We record also the failure returned by 'forecastFor'."
    ]
  documentFor (Namespace _ ["LedgerView"]) = Just $ mconcat
    [ "We obtained a ledger view for the current slot number"
    , " "
    , "  We record the current slot number."
    ]
  documentFor (Namespace _ ["ForgeStateUpdateError"]) = Just $ mconcat
    [ "Updating the forge state failed."
    , " "
    , "  For example, the KES key could not be evolved anymore."
    , " "
    , "  We record the error returned by 'updateForgeState'."
    ]
  documentFor (Namespace _ ["NodeCannotForge"]) = Just $ mconcat
    [ "We did the leadership check and concluded that we should lead and forge"
    , "  a block, but cannot."
    , " "
    , "  This should only happen rarely and should be logged with warning severity."
    , " "
    , "  Records why we cannot forge a block."
    ]
  documentFor (Namespace _ ["NodeNotLeader"]) = Just $ mconcat
    [ "We did the leadership check and concluded we are not the leader"
    , " "
    , "  We record the current slot number"
    ]
  documentFor (Namespace _ ["NodeIsLeader"]) = Just $ mconcat
    [ "We did the leadership check and concluded we /are/ the leader"
    , "\n"
    , "  The node will soon forge; it is about to read its transactions from the"
    , "  Mempool. This will be followed by ForgedBlock."
    ]
  documentFor (Namespace _ ["ForgeTickedLedgerState"]) = Just ""
  documentFor (Namespace _ ["ForgingMempoolSnapshot"]) = Just ""
  documentFor (Namespace _ ["ForgedBlock"]) = Just $ mconcat
    [ "We forged a block."
    , "\n"
    , "  We record the current slot number, the point of the predecessor, the block"
    , "  itself, and the total size of the mempool snapshot at the time we produced"
    , "  the block (which may be significantly larger than the block, due to"
    , "  maximum block size)"
    , "\n"
    , "  This will be followed by one of three messages:"
    , "\n"
    , "  * AdoptedBlock (normally)"
    , "\n"
    , "  * DidntAdoptBlock (rarely)"
    , "\n"
    , "  * ForgedInvalidBlock (hopefully never, this would indicate a bug)"
    ]
  documentFor (Namespace _ ["DidntAdoptBlock"]) = Just $ mconcat
    [ "We did not adopt the block we produced, but the block was valid. We"
    , "  must have adopted a block that another leader of the same slot produced"
    , "  before we got the chance of adopting our own block. This is very rare,"
    , "  this warrants a warning."
    ]
  documentFor (Namespace _ ["ForgedInvalidBlock"]) = Just $ mconcat
    [ "We forged a block that is invalid according to the ledger in the"
    , "  ChainDB. This means there is an inconsistency between the mempool"
    , "  validation and the ledger validation. This is a serious error!"
    ]
  documentFor (Namespace _ ["AdoptedBlock"]) = Just $ mconcat
    [ "We adopted the block we produced, we also trace the transactions"
    , "  that were adopted."
    ]
  documentFor (Namespace _ ["AdoptionThreadDied"]) = Just $ mconcat
    [ "Block adoption thread died" ]
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
    , Namespace [] ["AdoptionThreadDied"]
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

  documentFor (Namespace _ ["StartTimeInTheFuture"]) = Just $ mconcat
    [ "The start time of the blockchain time is in the future"
    , "\n"
    , " We have to block (for 'NominalDiffTime') until that time comes."
    ]
  documentFor (Namespace _ ["CurrentSlotUnknown"]) = Just $ mconcat
    [ "Current slot is not yet known"
    , "\n"
    , " This happens when the tip of our current chain is so far in the past that"
    , " we cannot translate the current wallclock to a slot number, typically"
    , " during syncing. Until the current slot number is known, we cannot"
    , " produce blocks. Seeing this message during syncing therefore is"
    , " normal and to be expected."
    , "\n"
    , " We record the current time (the time we tried to translate to a 'SlotNo')"
    , " as well as the 'PastHorizonException', which provides detail on the"
    , " bounds between which we /can/ do conversions. The distance between the"
    , " current time and the upper bound should rapidly decrease with consecutive"
    , " 'CurrentSlotUnknown' messages during syncing."
    ]
  documentFor (Namespace _ ["SystemClockMovedBack"]) = Just $ mconcat
    [ "The system clock moved back an acceptable time span, e.g., because of"
    , " an NTP sync."
    , "\n"
    , " The system clock moved back such that the new current slot would be"
    , " smaller than the previous one. If this is within the configured limit, we"
    , " trace this warning but *do not change the current slot*. The current slot"
    , " never decreases, but the current slot may stay the same longer than"
    , " expected."
    , "\n"
    , " When the system clock moved back more than the configured limit, we shut"
    , " down with a fatal exception."
    ]
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

--------------------------------------------------------------------------------
-- Gsm Tracer
--------------------------------------------------------------------------------

instance ( LogFormatting selection
         , Show selection
         ) => LogFormatting (TraceGsmEvent selection) where
  forMachine dtal =
    \case
      GsmEventEnterCaughtUp i s ->
        mconcat
          [ "kind" .= String "GsmEventEnterCaughtUp"
          , "peerNumber" .= i
          , "currentSelection" .= forMachine dtal s
          ]
      GsmEventLeaveCaughtUp s a ->
        mconcat
          [ "kind" .= String "GsmEventLeaveCaughtUp"
          , "currentSelection" .= forMachine dtal s
          , "age" .= toJSON (show a)
          ]
      GsmEventPreSyncingToSyncing ->
        mconcat
          [ "kind" .= String "GsmEventPreSyncingToSyncing"
          ]
      GsmEventSyncingToPreSyncing ->
        mconcat
          [ "kind" .= String "GsmEventSyncingToPreSyncing"
          ]

  forHuman = showT

instance MetaTrace (TraceGsmEvent selection) where
  namespaceFor =
    \case
      GsmEventEnterCaughtUp {}        -> Namespace [] ["EnterCaughtUp"]
      GsmEventLeaveCaughtUp {}        -> Namespace [] ["LeaveCaughtUp"]
      GsmEventPreSyncingToSyncing {}  -> Namespace [] ["GsmEventPreSyncingToSyncing"]
      GsmEventSyncingToPreSyncing {}  -> Namespace [] ["GsmEventSyncingToPreSyncing"]

  severityFor ns _ =
    case ns of
      Namespace _ ["EnterCaughtUp"]               -> Just Info
      Namespace _ ["LeaveCaughtUp"]               -> Just Info
      Namespace _ ["GsmEventPreSyncingToSyncing"] -> Just Info
      Namespace _ ["GsmEventSyncingToPreSyncing"] -> Just Info
      Namespace _ _                               -> Nothing

  documentFor = \case
    Namespace _ ["EnterCaughtUp"] ->
      Just "Node is caught up"
    Namespace _ ["LeaveCaughtUp"] ->
      Just "Node is not caught up"

    Namespace _ ["GsmEventPreSyncingToSyncing"] ->
      Just "The Honest Availability Assumption is now satisfied"
    Namespace _ ["GsmEventSyncingToPreSyncing"] ->
      Just "The Honest Availability Assumption is no longer satisfied"

    Namespace _ _ ->
      Nothing

  allNamespaces =
    [ Namespace [] ["EnterCaughtUp"]
    , Namespace [] ["LeaveCaughtUp"]
    , Namespace [] ["GsmEventPreSyncingToSyncing"]
    , Namespace [] ["GsmEventSyncingToPreSyncing"]
    ]

--------------------------------------------------------------------------------
-- Chain tip tracer
--------------------------------------------------------------------------------

instance ( StandardHash blk
         , ConvertRawHash blk
         ) => LogFormatting (Tip blk) where
  forMachine _dtal TipGenesis =
    mconcat [ "kind" .= String "TipGenesis" ]
  forMachine _dtal (Tip slotNo hash bNo) =
    mconcat [ "kind" .= String "Tip"
            , "tipSlotNo" .= toJSON (unSlotNo slotNo)
            , "tipHash" .= renderHeaderHash (Proxy @blk) hash
            , "tipBlockNo" .= toJSON bNo
            ]

  forHuman = showT
