{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Tracing.Receiving (tests) where

import           Cardano.Api (ByronEra, DijkstraEra, NetworkMagic (..),
                   ShelleyBasedEra (ShelleyBasedEraDijkstra), textShow)
import qualified Cardano.Api.Consensus as Api
import           Cardano.Api.Serialise.SerialiseUsing (UsingRawBytesHex (..))

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Alonzo.Scripts (AsItem (..))
import           Cardano.Ledger.BaseTypes (Mismatch (..), unsafeNonZero)
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Dijkstra as Ledger
import qualified Cardano.Ledger.Dijkstra.Rules as Dijkstra
import           Cardano.Ledger.Dijkstra.Scripts (DijkstraPlutusPurpose (..))
import           Cardano.Ledger.Hashes (ScriptHash (..))
import           Cardano.Ledger.Keys (KeyHash (..))
import           Cardano.Logging (DetailLevel (..), LogFormatting (..), MetaTrace (..), Metric (..),
                   Namespace (..), SeverityS (..))
import           Cardano.Node.Startup (StartupTrace (..))
import           Cardano.Node.Tracing.Consistency (getAllNamespaces)
import           Cardano.Node.Tracing.Era.Shelley ()
import           Cardano.Node.Tracing.Render (renderMissingRedeemers, renderScriptHash)
import           Cardano.Node.Tracing.Tracers.ChainDB ()
import           Cardano.Node.Tracing.Tracers.Consensus ()
import           Cardano.Node.Tracing.Tracers.Rpc ()
import           Cardano.Node.Tracing.Tracers.Startup (ppStartupInfoTrace)
import           Cardano.Rpc.Server (TraceRpc (..), TraceRpcSubmit (..), TraceSpanEvent (..))
import           Cardano.Slotting.Time (mkSlotLength)
import           Ouroboros.Consensus.Block (VoteWeight (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..), IgnoringOverflow (..),
                   TrivialTxMeasurePhase2 (..), TxMeasure (..))
import           Ouroboros.Consensus.Mempool (TraceEventMempool (..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                   (SupportedNetworkProtocolVersion (..))
import           Ouroboros.Consensus.Node.Tracers (TracePerasCertInclusionEvent (..),
                   TracePerasVoteForgingEvent (..))
import           Ouroboros.Consensus.Peras.Context (PerasEpochContextNotFoundForRound (..))
import qualified Ouroboros.Consensus.Peras.Error.V1 as Peras
import qualified Ouroboros.Consensus.Storage.LedgerDB.Snapshots as LedgerDB
import qualified Ouroboros.Network.Block as Network
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (BigLedgerPeers,
                   LedgerPeerSnapshot (..), RawBlockHash (..))

import           Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import           Data.Word (Word32)

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Hedgehog.Extras.Test.Base (propertyOnce)

-- Repeated hashes must not overwrite distinct body-local Receiving indexes.
-- Preserve the old singleton JSON value while extending duplicates to an array.
hprop_missingReceivingRedeemers :: Property
hprop_missingReceivingRedeemers = propertyOnce $ do
  hash <- H.evalMaybe $ Crypto.hashFromBytes (BS.replicate 28 1)
  let scriptHash = ScriptHash hash
      second = (DijkstraReceiving (AsItem 2), scriptHash)
      third = (DijkstraReceiving (AsItem 3), scriptHash)
      outputIndex index = Aeson.object ["receiving" Aeson..= (index :: Word32)]
      keyed value = Aeson.object [Aeson.fromText (renderScriptHash scriptHash) Aeson..= value]
  renderMissingRedeemers ShelleyBasedEraDijkstra (second :| []) === keyed (outputIndex 2)
  renderMissingRedeemers ShelleyBasedEraDijkstra (second :| [third])
    === keyed (Aeson.toJSON [outputIndex 2, outputIndex 3])
  renderMissingRedeemers ShelleyBasedEraDijkstra (third :| [second])
    === keyed (Aeson.toJSON [outputIndex 3, outputIndex 2])

-- A new RPC span must be selectable and documented, and count each request once.
hprop_mempoolRpcSpans :: Property
hprop_mempoolRpcSpans = propertyOnce $
  forM_ [("ReadMempool", TraceRpcReadMempoolSpan), ("WatchMempool", TraceRpcWatchMempoolSpan)] $ \(methodName, makeSpan) -> do
    let spanId = UsingRawBytesHex 1
        begin = TraceRpcSubmit $ makeSpan $ SpanBegin spanId
        end = TraceRpcSubmit $ makeSpan $ SpanEnd spanId
        namespace = Namespace [] ["SubmitService", methodName, "Span"]
        metricName = "rpc.request.SubmitService." <> methodName
        description = "Span for the " <> methodName <> " UTXORPC method."
    namespaceFor begin === namespace
    namespaceFor end === namespace
    H.assert $ namespace `elem` allNamespaces @TraceRpc
    severityFor @TraceRpc namespace Nothing === Just Debug
    documentFor @TraceRpc namespace === Just description
    metricsDocFor @TraceRpc namespace === [(metricName, description)]
    asMetrics begin === [CounterM metricName Nothing]
    asMetrics end === []
    forM_ [(begin, "begin"), (end, "end")] $ \(event, spanKind) -> do
      let rendered = forMachine DNormal event
      KeyMap.lookup "kind" rendered === Just (Aeson.String "SubmitService")
      KeyMap.lookup "span" rendered === Just (Aeson.String spanKind)
      KeyMap.lookup "spanId" rendered === Just (Aeson.toJSON spanId)

hprop_snapshotPolicyTrace :: Property
hprop_snapshotPolicyTrace = propertyOnce $ do
  let policy = LedgerDB.snapshotPolicyInfo (Api.SecurityParam $ unsafeNonZero 2160)
        LedgerDB.defaultSnapshotPolicyArgs (mkSlotLength 1)
      shortPolicy = policy {LedgerDB.spiSlotLengthAtTip = mkSlotLength 0.001}
      disabledPolicy = policy
        { LedgerDB.spiArgs = LedgerDB.defaultSnapshotPolicyArgs {LedgerDB.spaFrequency = LedgerDB.DisableSnapshots}
        , LedgerDB.spiIntervalSlots = Nothing
        }
      event info = LedgerDB.ConfiguredSnapshotPolicy info :: LedgerDB.TraceSnapshotEvent (Api.CardanoBlock Api.StandardCrypto)
      normal = event policy
      short = event shortPolicy
      disabled = event disabledPolicy
      rendered = forMachine DNormal normal
      expectedFields =
        [ ("kind", Aeson.String "ConfiguredSnapshotPolicy")
        , ("securityParam", Aeson.toJSON (2160 :: Integer))
        , ("slotLengthSeconds", Aeson.toJSON (1 :: Double))
        , ("numOfDiskSnapshots", Aeson.toJSON (2 :: Integer))
        , ("intervalSlots", Aeson.toJSON (86400 :: Integer))
        , ("intervalSeconds", Aeson.toJSON (86400 :: Double))
        , ("offsetSlots", Aeson.toJSON (0 :: Integer))
        , ("rateLimitSeconds", Aeson.toJSON (600 :: Double))
        , ("writeDelayMinSeconds", Aeson.toJSON (300 :: Double))
        , ("writeDelayMaxSeconds", Aeson.toJSON (21600 :: Double))
        , ("mismatches", Aeson.toJSON ([] :: [Text.Text]))
        ]
  forM_ expectedFields $ \(key, value) -> KeyMap.lookup key rendered === Just value
  namespaceFor normal === Namespace [] ["ConfiguredSnapshotPolicy"]
  namespaceFor short === Namespace [] ["ImplausibleSnapshotPolicy"]
  severityFor (namespaceFor normal) Nothing === Just Info
  severityFor (namespaceFor short) Nothing === Just Warning
  LedgerDB.snapshotPolicyMismatches shortPolicy
    === [LedgerDB.WriteDelayExceedsInterval, LedgerDB.RateLimitExceedsInterval]
  KeyMap.lookup "mismatches" (forMachine DNormal short)
    === Just (Aeson.toJSON (["WriteDelayExceedsInterval", "RateLimitExceedsInterval"] :: [Text.Text]))
  KeyMap.lookup "snapshotsDisabled" (forMachine DNormal disabled) === Just (Aeson.Bool True)
  KeyMap.lookup "intervalSlots" (forMachine DNormal disabled) === Nothing
  forM_ [normal, short, disabled] $ \trace -> do
    H.assert $ namespaceFor trace `elem` allNamespaces
    H.assert $ maybe False (not . Text.null) $ documentFor $ namespaceFor trace
    H.assert $ not $ Text.null $ forHuman trace

hprop_perasValidationErrors :: Property
hprop_perasValidationErrors = propertyOnce $ do
  let contextError = PerasEpochContextNotFoundForRound 5 "context unavailable"
      certificateError = Peras.PerasQuorumNotReachedError (VoteWeight 7)
        :: Peras.PerasError (Api.ConsensusBlockForEra DijkstraEra)
      contextEvent = ExtValidationErrorPerasEpochContextResolver contextError
        :: ExtValidationError (Api.ConsensusBlockForEra DijkstraEra)
      certificateEvent = ExtValidationErrorPerasCertInBlock certificateError
        :: ExtValidationError (Api.ConsensusBlockForEra DijkstraEra)
  forM_ [(contextEvent, "ExtValidationErrorPerasEpochContextResolver", textShow contextError),
         (certificateEvent, "ExtValidationErrorPerasCertInBlock", textShow certificateError)] $ \(event, kind, message) -> do
    let rendered = forMachine DNormal event
    KeyMap.lookup "kind" rendered === Just (Aeson.String kind)
    KeyMap.lookup "error" rendered === Just (Aeson.String message)
    H.assert $ message `Text.isInfixOf` forHuman event
    asMetrics event === []

hprop_mempoolCapacityTrace :: Property
hprop_mempoolCapacityTrace = propertyOnce $ do
  let before = TxMeasure (IgnoringOverflow $ ByteSize32 1024) TrivialTxMeasurePhase2
        :: TxMeasure (Api.ConsensusBlockForEra ByronEra)
      after = TxMeasure (IgnoringOverflow $ ByteSize32 2048) TrivialTxMeasurePhase2
        :: TxMeasure (Api.ConsensusBlockForEra ByronEra)
      event = TraceMempoolCapacityChanged before after
      capacity bytes = Aeson.object
        [ "txSizeBytes" Aeson..= (bytes :: Integer)
        , "exUnitsMemory" Aeson..= (0 :: Integer)
        , "exUnitsSteps" Aeson..= (0 :: Integer)
        , "refScriptsSizeBytes" Aeson..= (0 :: Integer)
        ]
      rendered = forMachine DNormal event
  KeyMap.lookup "kind" rendered === Just (Aeson.String "TraceMempoolCapacityChanged")
  KeyMap.lookup "capacityBefore" rendered === Just (capacity 1024)
  KeyMap.lookup "capacityAfter" rendered === Just (capacity 2048)
  namespaceFor event === Namespace [] ["CapacityChanged"]
  H.assert $ namespaceFor event `elem` allNamespaces
  severityFor (namespaceFor event) Nothing === Just Debug
  H.assert $ maybe False (not . Text.null) $ documentFor $ namespaceFor event
  asMetrics event === []

hprop_dijkstraPoolFailures :: Property
hprop_dijkstraPoolFailures = propertyOnce $ do
  hash <- H.evalMaybe $ Crypto.hashFromBytes (BS.replicate 28 2)
  let poolId = KeyHash hash
      missing = Dijkstra.StakePoolNotRegisteredOnKeyPOOL poolId
        :: Dijkstra.DijkstraPoolPredFailure Ledger.DijkstraEra
      lowCost = Dijkstra.StakePoolCostTooLowPOOL (Mismatch (Coin 2) (Coin 3))
        :: Dijkstra.DijkstraPoolPredFailure Ledger.DijkstraEra
      missingRendered = forMachine DNormal missing
      costRendered = forMachine DNormal lowCost
  KeyMap.lookup "kind" missingRendered === Just (Aeson.String "StakePoolNotRegisteredOnKeyPOOL")
  KeyMap.lookup "unregisteredKeyHash" missingRendered === Just (Aeson.String $ textShow hash)
  KeyMap.lookup "kind" costRendered === Just (Aeson.String "StakePoolCostTooLowPOOL")
  KeyMap.lookup "certificateCost" costRendered === Just (Aeson.String $ textShow $ Coin 2)
  KeyMap.lookup "protocolParCost" costRendered === Just (Aeson.String $ textShow $ Coin 3)

hprop_startupProtocolVersions :: Property
hprop_startupProtocolVersions = propertyOnce $ do
  let block = Proxy :: Proxy (Api.CardanoBlock Api.StandardCrypto)
      event = StartupInfo [] Nothing
        (supportedNodeToNodeVersions block) (supportedNodeToClientVersions block)
        :: StartupTrace (Api.CardanoBlock Api.StandardCrypto)
      rendered = ppStartupInfoTrace event
  H.assert $ "node-to-client versions: [23]" `Text.isInfixOf` rendered

hprop_perasForgingTraces :: Property
hprop_perasForgingTraces = propertyOnce $ do
  let certificateEvent = TracePerasCertInclusionNoCertToInclude 17
        :: TracePerasCertInclusionEvent (Api.CardanoBlock Api.StandardCrypto)
      certificateNamespace = Namespace [] ["NoCertToInclude"]
      inventory = [outer <> inner | (outer, inner) <- getAllNamespaces]
      voteEvents =
        [ (TracePerasVotingNoVoteAfterFirstSlotInRound 3 2, "NoVoteAfterFirstSlotInRound", Debug)
        , (TracePerasVotingNotAVoterInRound 5, "NotAVoterInRound", Debug)
        , (TracePerasVotingCantReadEnv "environment unavailable", "CantReadEnv", Error)
        ] :: [(TracePerasVoteForgingEvent (Api.CardanoBlock Api.StandardCrypto), Text.Text, SeverityS)]
  KeyMap.lookup "slot" (forMachine DNormal certificateEvent) === Just (Aeson.toJSON (17 :: Integer))
  namespaceFor certificateEvent === certificateNamespace
  severityFor @(TracePerasCertInclusionEvent (Api.CardanoBlock Api.StandardCrypto)) certificateNamespace Nothing === Just Debug
  H.assert $ maybe False (not . Text.null) $ documentFor @(TracePerasCertInclusionEvent (Api.CardanoBlock Api.StandardCrypto)) certificateNamespace
  H.assert $ ["Peras", "Cert", "Inclusion", "NoCertToInclude"] `elem` inventory
  asMetrics certificateEvent === []
  forM_ voteEvents $ \(event, suffix, severity) -> do
    let namespace = Namespace [] [suffix]
        rendered = forMachine DNormal event
    namespaceFor event === namespace
    severityFor @(TracePerasVoteForgingEvent (Api.CardanoBlock Api.StandardCrypto)) namespace Nothing === Just severity
    H.assert $ namespace `elem` allNamespaces @(TracePerasVoteForgingEvent (Api.CardanoBlock Api.StandardCrypto))
    H.assert $ maybe False (not . Text.null) $ documentFor @(TracePerasVoteForgingEvent (Api.CardanoBlock Api.StandardCrypto)) namespace
    H.assert $ ["Peras", "Vote", "Forging", suffix] `elem` inventory
    H.assert $ not $ Text.null $ forHuman event
    asMetrics event === []
    case event of
      TracePerasVotingNoVoteAfterFirstSlotInRound _ _ -> do
        KeyMap.lookup "round" rendered === Just (Aeson.toJSON (3 :: Integer))
        KeyMap.lookup "slotInRound" rendered === Just (Aeson.toJSON (2 :: Integer))
      TracePerasVotingNotAVoterInRound _ ->
        KeyMap.lookup "round" rendered === Just (Aeson.toJSON (5 :: Integer))
      TracePerasVotingCantReadEnv _ ->
        KeyMap.lookup "error" rendered === Just (Aeson.String "environment unavailable")
      _ -> H.failure

hprop_peerSnapshotKinds :: Property
hprop_peerSnapshotKinds = propertyOnce $ do
  let point = Network.BlockPoint 17 (RawBlockHash $ SBS.toShort $ BS.replicate 32 3)
      big = LedgerBigPeerSnapshotV23 point (NetworkMagic 42) []
      allPeers = LedgerAllPeerSnapshotV23 point (NetworkMagic 42) []
      decodeBig bytes = Aeson.eitherDecode bytes :: Either String (LedgerPeerSnapshot BigLedgerPeers)
      legacy = case Aeson.toJSON big of
        Aeson.Object fields -> Aeson.Object $ KeyMap.insert "NodeToClientVersion" (Aeson.toJSON (22 :: Int)) fields
        value -> value
  -- The node file reader uses this typed decoder. The all-peers constructor
  -- cannot inhabit its BigLedgerPeers result, and legacy versions fail here.
  decodeBig (Aeson.encode big) === Right big
  case decodeBig (Aeson.encode legacy) of
    Left err -> H.assert $ "unsupported version 22" `Text.isInfixOf` Text.pack err
    Right _ -> H.failure
  case decodeBig (Aeson.encode allPeers) of
    Left err -> H.assert $ "bigLedgerPools" `Text.isInfixOf` Text.pack err
    Right _ -> H.failure

tests :: IO Bool
tests = H.checkSequential $ H.Group "Test.Tracing.Receiving"
  [ ("missing same-hash Receiving redeemers", hprop_missingReceivingRedeemers)
  , ("mempool RPC span metadata", hprop_mempoolRpcSpans)
  , ("snapshot policy payload and metadata", hprop_snapshotPolicyTrace)
  , ("Peras validation error payloads", hprop_perasValidationErrors)
  , ("mempool capacity payload and metadata", hprop_mempoolCapacityTrace)
  , ("Dijkstra pool failure payloads", hprop_dijkstraPoolFailures)
  , ("startup supported protocol versions", hprop_startupProtocolVersions)
  , ("Peras forging payloads and registration", hprop_perasForgingTraces)
  , ("peer snapshot version and kind rejection", hprop_peerSnapshotKinds)
  ]
