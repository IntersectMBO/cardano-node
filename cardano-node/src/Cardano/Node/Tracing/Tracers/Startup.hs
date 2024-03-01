{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Tracing.Tracers.Startup

  ( getStartupInfo
  , ppStartupInfoTrace
  )  where

import           Cardano.Api (NetworkMagic (..), SlotNo (..))
import qualified Cardano.Api as Api

import qualified Cardano.Chain.Genesis as Gen
import           Cardano.Git.Rev (gitRev)
import           Cardano.Ledger.Shelley.API as SL
import           Cardano.Logging
import           Cardano.Node.Configuration.POM (NodeConfiguration, ncProtocol)
import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Protocol (SomeConsensusProtocol (..))
import           Cardano.Node.Startup
import           Cardano.Slotting.Slot (EpochSize (..))
import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as WCT
import           Ouroboros.Consensus.Byron.Ledger.Conversions (fromByronEpochSlots,
                   fromByronSlotLength, genesisSlotLength)
import           Ouroboros.Consensus.Cardano.Block (HardForkLedgerConfig (..))
import           Ouroboros.Consensus.Cardano.CanHardFork (ByronPartialLedgerConfig (..),
                   ShelleyPartialLedgerConfig (..))
import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (..))
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate (HardForkLedgerConfig (..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Shelley.Ledger.Ledger (shelleyLedgerGenesis)
import           Ouroboros.Network.NodeToClient (LocalAddress (..), LocalSocket (..))
import           Ouroboros.Network.NodeToNode (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (AfterSlot (..),
                   UseLedgerPeers (..))

import           Prelude

import           Data.Aeson (ToJSON (..), Value (..), (.=))
import qualified Data.Aeson as Aeson
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Text (Text, pack)
import           Data.Time (getCurrentTime)
import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Version (showVersion)
import           Network.Socket (SockAddr)

import           Paths_cardano_node (version)


getStartupInfo
  :: NodeConfiguration
  -> SomeConsensusProtocol
  -> FilePath
  -> IO [StartupTrace blk]
getStartupInfo nc (SomeConsensusProtocol whichP pForInfo) fp = do
  nodeStartTime <- getCurrentTime
  let cfg = pInfoConfig $ fst $ Api.protocolInfo @IO pForInfo
      basicInfoCommon = BICommon $ BasicInfoCommon {
                biProtocol = pack . show $ ncProtocol nc
              , biVersion  = pack . showVersion $ version
              , biCommit   = gitRev
              , biNodeStartTime = nodeStartTime
              , biConfigPath = fp
              , biNetworkMagic = getNetworkMagic $ Consensus.configBlock cfg
              }
      protocolDependentItems =
        case whichP of
          Api.ByronBlockType ->
            let DegenLedgerConfig cfgByron = Consensus.configLedger cfg
            in [getGenesisValuesByron cfg cfgByron]
          Api.ShelleyBlockType ->
            let DegenLedgerConfig cfgShelley = Consensus.configLedger cfg
            in [getGenesisValues "Shelley" cfgShelley]
          Api.CardanoBlockType ->
            let CardanoLedgerConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo
                                    cfgBabbage cfgConway = Consensus.configLedger cfg
            in [ getGenesisValuesByron cfg cfgByron
               , getGenesisValues "Shelley" cfgShelley
               , getGenesisValues "Allegra" cfgAllegra
               , getGenesisValues "Mary"    cfgMary
               , getGenesisValues "Alonzo"  cfgAlonzo
               , getGenesisValues "Babbage" cfgBabbage
               , getGenesisValues "Conway"  cfgConway
               ]
  pure (basicInfoCommon : protocolDependentItems)
    where
      getGenesisValues era config =
        let genesis = shelleyLedgerGenesis $ shelleyLedgerConfig config
        in BIShelley $ BasicInfoShelleyBased {
            bisEra               = era
          , bisSystemStartTime   = SL.sgSystemStart genesis
          , bisSlotLength        = WCT.getSlotLength . WCT.mkSlotLength
                                      . SL.fromNominalDiffTimeMicro
                                      $ SL.sgSlotLength genesis
          , bisEpochLength       = unEpochSize . SL.sgEpochLength $ genesis
          , bisSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesis
        }
      getGenesisValuesByron cfg config =
        let genesis = byronLedgerConfig config
        in BIByron $ BasicInfoByron {
            bibSystemStartTime = WCT.getSystemStart . getSystemStart
                                  $ Consensus.configBlock cfg
          , bibSlotLength      = WCT.getSlotLength . fromByronSlotLength
                                  $ genesisSlotLength genesis
          , bibEpochLength     = unEpochSize . fromByronEpochSlots
                                  $ Gen.configEpochSlots genesis
          }

-- --------------------------------------------------------------------------------
-- -- StartupInfo Tracer
-- --------------------------------------------------------------------------------


-- | A tuple of consensus and network versions.  It's used to derive a custom
-- `FromJSON` and `ToJSON` instances.
--
data ConsensusNetworkVersionTuple a b = ConsensusNetworkVersionTuple a b

instance ToJSON blkVersion => ToJSON (ConsensusNetworkVersionTuple NodeToClientVersion blkVersion) where
    toJSON (ConsensusNetworkVersionTuple nodeToClientVersion blockVersion) =
      Aeson.object [ "nodeToClientVersion" .= nodeToClientVersion
                   , "blockVersion" .= blockVersion
                   ]

instance ToJSON blkVersion => ToJSON (ConsensusNetworkVersionTuple NodeToNodeVersion blkVersion) where
    toJSON (ConsensusNetworkVersionTuple nodeToClientVersion blockVersion) =
      Aeson.object [ "nodeToNodeVersion" .= nodeToClientVersion
                   , "blockVersion" .= blockVersion
                   ]


instance ( Show (BlockNodeToNodeVersion blk)
         , Show (BlockNodeToClientVersion blk)
         , ToJSON (BlockNodeToNodeVersion blk)
         , ToJSON (BlockNodeToClientVersion blk)
         )
        => LogFormatting (StartupTrace blk) where
  forHuman = ppStartupInfoTrace

  forMachine dtal (StartupInfo addresses
                                 localSocket
                                 supportedNodeToNodeVersions
                                 supportedNodeToClientVersions)
      = mconcat (
        [ "kind" .= String "StartupInfo"
        , "nodeAddresses" .= toJSON (map ppN2NSocketInfo addresses)
        , "localSocket" .= case localSocket of
              Nothing -> Null
              Just a  -> String (pack . ppN2CSocketInfo $ a)
        ]
        ++
        case dtal of
          DMaximum ->
            [ "nodeToNodeVersions" .=
                toJSON (map (uncurry ConsensusNetworkVersionTuple) . Map.assocs $ supportedNodeToNodeVersions)
            , "nodeToClientVersions" .=
                toJSON (map (uncurry ConsensusNetworkVersionTuple) . Map.assocs $ supportedNodeToClientVersions)
            ]
          _ ->
            [ "maxNodeToNodeVersion" .=
                case Map.maxViewWithKey supportedNodeToNodeVersions of
                  Nothing     -> String "no-supported-version"
                  Just (v, _) -> String (pack . show $ v)
            , "maxNodeToClientVersion" .=
                case Map.maxViewWithKey supportedNodeToClientVersions of
                  Nothing     -> String "no-supported-version"
                  Just (v, _) -> String (pack . show $ v)
            ])
  forMachine _dtal (StartupP2PInfo diffusionMode) =
      mconcat [ "kind" .= String "StartupP2PInfo"
               , "diffusionMode" .= String (showT diffusionMode) ]
  forMachine _dtal (StartupTime time) =
      mconcat [ "kind" .= String "StartupTime"
               , "startupTime" .= String ( showT
                                         . (ceiling :: POSIXTime -> Int)
                                         . utcTimeToPOSIXSeconds
                                         $ time
                                         )
               ]
  forMachine _dtal (StartupNetworkMagic networkMagic) =
      mconcat [ "kind" .= String "StartupNetworkMagic"
               , "networkMagic" .= String (showT . unNetworkMagic
                                          $ networkMagic) ]
  forMachine _dtal (StartupSocketConfigError err) =
      mconcat [ "kind" .= String "StartupSocketConfigError"
               , "error" .= String (showT err) ]
  forMachine _dtal StartupDBValidation =
      mconcat [ "kind" .= String "StartupDBValidation"
               , "message" .= String "start db validation" ]
  forMachine _dtal (BlockForgingUpdate b) =
      mconcat [ "kind" .= String "BlockForgingUpdate"
              , "enabled" .= String (showT b)
              ]
  forMachine _dtal (BlockForgingUpdateError err) =
      mconcat [ "kind" .= String "BlockForgingUpdateError"
              , "error" .= String (showT err)
              ]
  forMachine _dtal (BlockForgingBlockTypeMismatch expected provided) =
      mconcat [ "kind" .= String "BlockForgingBlockTypeMismatch"
              , "expected" .= String (showT expected)
              , "provided" .= String (showT provided)
              ]
  forMachine _dtal NetworkConfigUpdate =
      mconcat [ "kind" .= String "NetworkConfigUpdate"
               , "message" .= String "network configuration update" ]
  forMachine _dtal NetworkConfigUpdateUnsupported =
      mconcat [ "kind" .= String "NetworkConfigUpdate"
              , "message" .= String "network topology reconfiguration is not supported in non-p2p mode" ]
  forMachine _dtal (NetworkConfigUpdateError err) =
      mconcat [ "kind" .= String "NetworkConfigUpdateError"
               , "error" .= String err ]
  forMachine _dtal (NetworkConfig localRoots publicRoots useLedgerPeers) =
      mconcat [ "kind" .= String "NetworkConfig"
               , "localRoots" .= toJSON localRoots
               , "publicRoots" .= toJSON publicRoots
               , "useLedgerAfter" .= useLedgerPeers
               ]
  forMachine _dtal NetworkConfigLegacy =
      mconcat [ "kind" .= String "NetworkConfigLegacy"
              , "message" .= String p2pNetworkConfigLegacyMessage
              ]
  forMachine _dtal P2PWarning =
      mconcat [ "kind" .= String "P2PWarning"
               , "message" .= String p2pWarningMessage ]
  forMachine _ver (WarningDevelopmentNodeToNodeVersions ntnVersions) =
      mconcat [ "kind" .= String "WarningDevelopmentNodeToNodeVersions"
               , "message" .= String "enabled development network protocols"
               , "versions" .= String (showT ntnVersions)
               ]
  forMachine _ver (WarningDevelopmentNodeToClientVersions ntcVersions) =
      mconcat [ "kind" .= String "WarningDevelopmentNodeToClientVersions"
               , "message" .= String "enabled development network protocols"
               , "versions" .= String (showT ntcVersions)
               ]
  forMachine _dtal (BINetwork BasicInfoNetwork {..}) =
      mconcat [ "kind" .= String "BasicInfoNetwork"
               , "addresses" .= String (showT niAddresses)
               , "diffusionMode"  .= String (showT niDiffusionMode)
               , "dnsProducers" .= String (showT niDnsProducers)
               , "ipProducers" .= String (showT niIpProducers)
               ]
  forMachine _dtal (BIByron BasicInfoByron {..}) =
      mconcat [ "kind" .= String "BasicInfoByron"
               , "systemStartTime" .= String (showT bibSystemStartTime)
               , "slotLength"  .= String (showT bibSlotLength)
               , "epochLength" .= String (showT bibEpochLength)
               ]
  forMachine _dtal (BIShelley BasicInfoShelleyBased {..}) =
      mconcat [ "kind" .= String "BasicInfoShelleyBased"
               , "era"  .= String bisEra
               , "systemStartTime" .= String (showT bisSystemStartTime)
               , "slotLength"  .= String (showT bisSlotLength)
               , "epochLength" .= String (showT bisEpochLength)
               , "slotsPerKESPeriod" .= String (showT bisSlotsPerKESPeriod)
               ]
  forMachine _dtal (BICommon BasicInfoCommon {..}) =
      mconcat [ "kind" .= String "BasicInfoCommon"
               , "configPath" .= String (pack biConfigPath)
               , "networkMagic"  .= String (showT biNetworkMagic)
               , "protocol" .= String biProtocol
               , "version" .= String biVersion
               , "commit" .= String biCommit
               , "nodeStartTime" .= biNodeStartTime
               ]

instance MetaTrace  (StartupTrace blk) where
  namespaceFor StartupInfo {}  =
    Namespace [] ["Info"]
  namespaceFor StartupP2PInfo {}  =
    Namespace [] ["P2PInfo"]
  namespaceFor StartupTime {}  =
    Namespace [] ["Time"]
  namespaceFor StartupNetworkMagic {}  =
    Namespace [] ["NetworkMagic"]
  namespaceFor StartupSocketConfigError {}  =
     Namespace [] ["SocketConfigError"]
  namespaceFor StartupDBValidation {}  =
    Namespace [] ["DBValidation"]
  namespaceFor BlockForgingUpdate {} =
    Namespace [] ["BlockForgingUpdate"]
  namespaceFor BlockForgingUpdateError {} =
    Namespace [] ["BlockForgingUpdateError"]
  namespaceFor BlockForgingBlockTypeMismatch {} =
    Namespace [] ["BlockForgingBlockTypeMismatch"]
  namespaceFor NetworkConfigUpdate {}  =
    Namespace [] ["NetworkConfigUpdate"]
  namespaceFor NetworkConfigUpdateUnsupported {}  =
    Namespace [] ["NetworkConfigUpdateUnsupported"]
  namespaceFor NetworkConfigUpdateError {}  =
    Namespace [] ["NetworkConfigUpdateError"]
  namespaceFor NetworkConfig {}  =
    Namespace [] ["NetworkConfig"]
  namespaceFor NetworkConfigLegacy {}  =
    Namespace [] ["NetworkConfigLegacy"]
  namespaceFor P2PWarning {}  =
    Namespace [] ["P2PWarning"]
  namespaceFor WarningDevelopmentNodeToNodeVersions {}  =
    Namespace [] ["WarningDevelopmentNodeToNodeVersions"]
  namespaceFor WarningDevelopmentNodeToClientVersions {}  =
    Namespace [] ["WarningDevelopmentNodeToClientVersions"]
  namespaceFor BICommon {}  =
    Namespace [] ["Common"]
  namespaceFor BIShelley {}  =
    Namespace [] ["ShelleyBased"]
  namespaceFor BIByron {}  =
    Namespace [] ["Byron"]
  namespaceFor BINetwork {}  =
    Namespace [] ["Network"]

  severityFor (Namespace _ ["SocketConfigError"]) _ = Just Error
  severityFor (Namespace _ ["NetworkConfigUpdate"]) _ = Just Notice
  severityFor (Namespace _ ["NetworkConfigUpdateError"]) _ = Just Error
  severityFor (Namespace _ ["NetworkConfigUpdateUnsupported"]) _ = Just Warning
  severityFor (Namespace _ ["P2PWarning"]) _ = Just Warning
  severityFor (Namespace _ ["WarningDevelopmentNodeToNodeVersions"]) _ = Just Warning
  severityFor (Namespace _ ["WarningDevelopmentNodeToClientVersions"]) _ = Just Warning
  severityFor (Namespace _ ["BlockForgingUpdateError"]) _ = Just Error
  severityFor (Namespace _ ["BlockForgingBlockTypeMismatch"]) _ = Just Error
  severityFor _ _ = Just Info

  documentFor (Namespace [] ["Info"]) = Just
    ""
  documentFor (Namespace [] ["P2PInfo"]) = Just
    ""
  documentFor (Namespace [] ["Time"]) = Just
    ""
  documentFor (Namespace [] ["NetworkMagic"]) = Just
    ""
  documentFor (Namespace [] ["SocketConfigError"]) = Just
    ""
  documentFor (Namespace [] ["DBValidation"]) = Just
    ""
  documentFor (Namespace [] ["BlockForgingUpdate"]) = Just
    ""
  documentFor (Namespace [] ["BlockForgingUpdateError"]) = Just
    ""
  documentFor (Namespace [] ["BlockForgingBlockTypeMismatch"]) = Just
    ""
  documentFor (Namespace [] ["NetworkConfigUpdate"]) = Just
    ""
  documentFor (Namespace [] ["NetworkConfigUpdateUnsupported"]) = Just
    ""
  documentFor (Namespace [] ["NetworkConfigUpdateError"]) = Just
    ""
  documentFor (Namespace [] ["NetworkConfig"]) = Just
    ""
  documentFor (Namespace [] ["NetworkConfigLegacy"]) = Just
    ""
  documentFor (Namespace [] ["P2PWarning"]) = Just
    ""
  documentFor (Namespace [] ["WarningDevelopmentNodeToNodeVersions"]) = Just
    ""
  documentFor (Namespace [] ["WarningDevelopmentNodeToClientVersions"]) = Just
    ""
  documentFor (Namespace [] ["Common"]) = Just $ mconcat
    [ "_biConfigPath_: is the path to the config in use. "
    , "\n_biProtocol_: is the name of the protocol, e.g. \"Byron\", \"Shelley\" "
    , "or \"Byron; Shelley\". "
    , "\n_biVersion_: is the version of the node software running. "
    , "\n_biCommit_: is the commit revision of the software running. "
    , "\n_biNodeStartTime_: gives the time this node was started."
    ]
  documentFor (Namespace [] ["ShelleyBased"]) = Just $ mconcat
    [ "bisEra is the current era, e.g. \"Shelley\", \"Allegra\", \"Mary\" "
    , "or \"Alonzo\". "
    , "\n_bisSystemStartTime_: "
    , "\n_bisSlotLength_: gives the length of a slot as time interval. "
    , "\n_bisEpochLength_: gives the number of slots which forms an epoch. "
    , "\n_bisSlotsPerKESPeriod_: gives the slots per KES period."
    ]
  documentFor (Namespace [] ["Byron"]) = Just $ mconcat
    [ "_bibSystemStartTime_: "
    , "\n_bibSlotLength_: gives the length of a slot as time interval. "
    , "\n_bibEpochLength_: gives the number of slots which forms an epoch."
    ]
  documentFor (Namespace [] ["Network"]) = Just $ mconcat
    [ "_niAddresses_: IPv4 or IPv6 socket ready to accept connections"
    , "or diffusion addresses. "
    , "\n_niDiffusionMode_: shows if the node runs only initiator or both"
    , "initiator or responder node. "
    , "\n_niDnsProducers_: shows the list of domain names to subscribe to. "
    , "\n_niIpProducers_: shows the list of ip subscription addresses."
    ]
  documentFor _ns = Nothing

  allNamespaces =
    [ Namespace [] ["Info"]
    , Namespace [] ["P2PInfo"]
    , Namespace [] ["Time"]
    , Namespace [] ["NetworkMagic"]
    , Namespace [] ["SocketConfigError"]
    , Namespace [] ["DBValidation"]
    , Namespace [] ["BlockForgingUpdate"]
    , Namespace [] ["BlockForgingBlockTypeMismatch"]
    , Namespace [] ["NetworkConfigUpdate"]
    , Namespace [] ["NetworkConfigUpdateUnsupported"]
    , Namespace [] ["NetworkConfigUpdateError"]
    , Namespace [] ["NetworkConfig"]
    , Namespace [] ["NetworkConfigLegacy"]
    , Namespace [] ["P2PWarning"]
    , Namespace [] ["WarningDevelopmentNodeToNodeVersions"]
    , Namespace [] ["WarningDevelopmentNodeToClientVersions"]
    , Namespace [] ["Common"]
    , Namespace [] ["ShelleyBased"]
    , Namespace [] ["Byron"]
    , Namespace [] ["Network"]
    ]


nodeToClientVersionToInt :: NodeToClientVersion -> Int
nodeToClientVersionToInt = \case
  NodeToClientV_9 -> 9
  NodeToClientV_10 -> 10
  NodeToClientV_11 -> 11
  NodeToClientV_12 -> 12
  NodeToClientV_13 -> 13
  NodeToClientV_14 -> 14
  NodeToClientV_15 -> 15
  NodeToClientV_16 -> 16

nodeToNodeVersionToInt :: NodeToNodeVersion -> Int
nodeToNodeVersionToInt = \case
  NodeToNodeV_7 -> 7
  NodeToNodeV_8 -> 8
  NodeToNodeV_9 -> 9
  NodeToNodeV_10 -> 10
  NodeToNodeV_11 -> 11
  NodeToNodeV_12 -> 12
  NodeToNodeV_13 -> 13

-- | Pretty print 'StartupInfoTrace'
--
ppStartupInfoTrace :: StartupTrace blk -> Text
ppStartupInfoTrace (StartupInfo addresses
                                localSocket
                                supportedNodeToNodeVersions
                                supportedNodeToClientVersions)
  = pack
  $ "\n" ++ intercalate "\n"
    [ "node addresses:          " ++ intercalate ", " (map ppN2NSocketInfo addresses)
    , "local socket:            " ++ maybe "NONE" ppN2CSocketInfo localSocket
    , "node-to-node versions:   " ++ show (fmap nodeToNodeVersionToInt (Map.keys supportedNodeToNodeVersions))
    , "node-to-client versions: " ++ show (fmap nodeToClientVersionToInt (Map.keys supportedNodeToClientVersions))
    ]

ppStartupInfoTrace (StartupP2PInfo diffusionMode) =
        case diffusionMode of
          InitiatorAndResponderDiffusionMode -> "initiator and responder diffusion mode"
          InitiatorOnlyDiffusionMode         -> "initaitor only diffusion mode"

ppStartupInfoTrace (StartupTime time) =
  "startup time: "
  <> ( showT
       . (ceiling :: POSIXTime -> Int)
       . utcTimeToPOSIXSeconds
       $ time
     )
ppStartupInfoTrace (StartupNetworkMagic networkMagic) =
  "network magic: " <> showT (unNetworkMagic networkMagic)

ppStartupInfoTrace (StartupSocketConfigError err) =
  pack $ renderSocketConfigError err

ppStartupInfoTrace StartupDBValidation = "Performing DB validation"

ppStartupInfoTrace (BlockForgingUpdate b) =
  "Performing block forging reconfiguration: "
    <> case b of
        EnabledBlockForging  ->
            "Enabling block forging. To disable it please move/rename/remove "
          <> "the credentials files and then trigger reconfiguration via SIGHUP "
          <> "signal."
        DisabledBlockForging ->
            "Disabling block forging, to enable it please make the credentials "
          <> "files available again and then re-trigger reconfiguration via SIGHUP "
          <> "signal."
        NotEffective ->
             "Changing block forging is not effective until consensus has started. "

ppStartupInfoTrace (BlockForgingUpdateError err) =
  "Block forging reconfiguration error: "
    <> showT err <> "\n"
    <> "Block forging is not reconfigured."
ppStartupInfoTrace (BlockForgingBlockTypeMismatch expected provided) =
  "Block forging reconfiguration block type mismatch: expected "
    <> showT expected
    <> " provided "
    <> showT provided

ppStartupInfoTrace NetworkConfigUpdate = "Performing topology configuration update"
ppStartupInfoTrace NetworkConfigUpdateUnsupported =
  "Network topology reconfiguration is not supported in non-p2p mode"
ppStartupInfoTrace (NetworkConfigUpdateError err) = err
ppStartupInfoTrace (NetworkConfig localRoots publicRoots useLedgerPeers) =
    pack
  $ intercalate "\n"
  [ "\nLocal Root Groups:"
  , "  " ++ intercalate "\n  " (map (\(x,y,z) -> show (x, y, Map.assocs z))
                                    localRoots)
  , "Public Roots:"
  , "  " ++ intercalate "\n  " (map show $ Map.assocs publicRoots)
  , case useLedgerPeers of
      DontUseLedgerPeers            ->
        "Don't use ledger to get root peers."
      UseLedgerPeers (After slotNo) ->
        "Get root peers from the ledger after slot "
        ++ show (unSlotNo slotNo)
      UseLedgerPeers Always         ->
        "Use ledger peers in any slot."
  ]
ppStartupInfoTrace NetworkConfigLegacy = p2pNetworkConfigLegacyMessage

ppStartupInfoTrace P2PWarning = p2pWarningMessage

ppStartupInfoTrace (WarningDevelopmentNodeToNodeVersions ntnVersions) =
     "enabled development node-to-node versions: "
  <> showT ntnVersions

ppStartupInfoTrace (WarningDevelopmentNodeToClientVersions ntcVersions) =
     "enabled development node-to-client versions: "
  <> showT ntcVersions

ppStartupInfoTrace (BINetwork BasicInfoNetwork {..}) =
  "Addresses " <> showT niAddresses
  <> ", DiffusionMode " <> showT niDiffusionMode
  <> ", DnsProducers " <> showT niDnsProducers
  <> ", IpProducers " <> showT niIpProducers

ppStartupInfoTrace (BIByron BasicInfoByron {..}) =
  "Era Byron"
  <> ", Slot length " <> showT bibSlotLength
  <> ", Epoch length " <> showT bibEpochLength

ppStartupInfoTrace (BIShelley BasicInfoShelleyBased {..}) =
  "Era " <> bisEra
  <> ", Slot length " <> showT bisSlotLength
  <> ", Epoch length " <> showT bisEpochLength
  <> ", Slots per KESPeriod " <> showT bisSlotsPerKESPeriod

ppStartupInfoTrace (BICommon BasicInfoCommon {..}) =
  "Config path " <> pack biConfigPath
  <> ", Network magic " <> showT biNetworkMagic
  <> ", Protocol " <> showT biProtocol
  <> ", Version " <> showT biVersion
  <> ", Commit " <> showT biCommit
  <> ", Node start time " <> showT biNodeStartTime

p2pWarningMessage :: Text
p2pWarningMessage =
      "You are using an early release of peer-to-peer capabilities, "
   <> "please report any issues."

p2pNetworkConfigLegacyMessage :: Text
p2pNetworkConfigLegacyMessage =
    pack
  $ intercalate "\n"
  [ "You are using legacy p2p topology file format."
  , "See https://github.com/intersectmbo/cardano-node/issues/4559"
  , "Note that the legacy p2p format will be removed in `1.37` release."
  ]

-- | Pretty print 'SocketOrSocketInfo'.
--
ppSocketInfo :: Show sock
             => (info -> String)
             -> SocketOrSocketInfo sock info -> String
ppSocketInfo  ppInfo (SocketInfo addr)   = ppInfo addr
ppSocketInfo _ppInfo (ActualSocket sock) = show sock

ppN2CSocketInfo :: SocketOrSocketInfo LocalSocket LocalAddress
                -> String
ppN2CSocketInfo = ppSocketInfo getFilePath

ppN2NSocketInfo :: SocketOrSocketInfo SockAddr SockAddr
                -> String
ppN2NSocketInfo = ppSocketInfo show
