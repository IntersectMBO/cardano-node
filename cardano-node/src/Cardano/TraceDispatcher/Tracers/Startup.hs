{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
module Cardano.TraceDispatcher.Tracers.Startup
  ( getStartupInfo
  , namesStartupInfo
  , docStartupInfo
  , ppStartupInfoTrace
  ) where

import           Prelude

import           Data.Aeson (ToJSON (..), Value (..), (.=))
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Text (Text, pack)
import           Data.Time (getCurrentTime)
import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Version (showVersion)
import           Network.Socket (SockAddr)
import           Paths_cardano_node (version)

import qualified Cardano.Chain.Genesis as Gen
import           Cardano.Slotting.Slot (EpochSize (..))

import           Cardano.Ledger.Shelley.API as SL

import           Ouroboros.Network.NodeToClient (LocalAddress (..),
                     LocalSocket (..))
import           Ouroboros.Network.NodeToNode (DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))

import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as WCT
import           Ouroboros.Consensus.Byron.Ledger.Conversions
                     (fromByronEpochSlots, fromByronSlotLength,
                     genesisSlotLength)
import           Ouroboros.Consensus.Cardano.Block (HardForkLedgerConfig (..))
import           Ouroboros.Consensus.Cardano.CanHardFork
                     (ByronPartialLedgerConfig (..),
                     ShelleyPartialLedgerConfig (..))
import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.Config.SupportsNode
                     (ConfigSupportsNode (..))
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
                     (HardForkLedgerConfig (..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Shelley.Ledger.Ledger (shelleyLedgerGenesis)

import           Cardano.Logging

import           Cardano.Api (NetworkMagic (..), SlotNo (..))
import           Cardano.Api.Protocol.Types (BlockType (..), protocolInfo)

import           Cardano.Config.Git.Rev (gitRev)

import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Configuration.TopologyP2P
import           Cardano.Node.Startup
import           Cardano.Node.Configuration.POM (NodeConfiguration, ncProtocol)
import           Cardano.Node.Protocol (SomeConsensusProtocol (..))
import           Cardano.Node.Protocol.Types (protocolName)


getStartupInfo ::
     NodeConfiguration
  -> SomeConsensusProtocol
  -> FilePath
  -> IO [StartupTrace blk]
getStartupInfo nc (SomeConsensusProtocol whichP pForInfo) fp = do
  nodeStartTime <- getCurrentTime
  let cfg = pInfoConfig $ protocolInfo pForInfo
      basicInfoCommon = BICommon $ BasicInfoCommon {
                biProtocol = pack . protocolName $ ncProtocol nc
              , biVersion  = pack . showVersion $ version
              , biCommit   = gitRev
              , biNodeStartTime = nodeStartTime
              , biConfigPath = fp
              , biNetworkMagic = getNetworkMagic $ Consensus.configBlock cfg
              }
      protocolDependentItems =
        case whichP of
          ByronBlockType ->
            let DegenLedgerConfig cfgByron = Consensus.configLedger cfg
            in [getGenesisValuesByron cfg cfgByron]
          ShelleyBlockType ->
            let DegenLedgerConfig cfgShelley = Consensus.configLedger cfg
            in [getGenesisValues "Shelley" cfgShelley]
          CardanoBlockType ->
            let CardanoLedgerConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo = Consensus.configLedger cfg
            in getGenesisValuesByron cfg cfgByron
               : getGenesisValues "Shelley" cfgShelley
               : getGenesisValues "Allegra" cfgAllegra
               : getGenesisValues "Mary"    cfgMary
               : [getGenesisValues "Alonzo"  cfgAlonzo]
  pure (basicInfoCommon : protocolDependentItems)
    where
      getGenesisValues era config =
        let genesis = shelleyLedgerGenesis $ shelleyLedgerConfig config
        in BIShelley $ BasicInfoShelleyBased {
            bisEra               = era
          , bisSystemStartTime   = SL.sgSystemStart genesis
          , bisSlotLength        = WCT.getSlotLength . WCT.mkSlotLength
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

--------------------------------------------------------------------------------
-- StartupInfo Tracer
--------------------------------------------------------------------------------

namesStartupInfo :: StartupTrace blk -> [Text]
namesStartupInfo = \case
  StartupInfo {}                            -> ["StartupInfo"]
  StartupP2PInfo {}                         -> ["StartupP2PInfo"]
  StartupTime {}                            -> ["StartupTime"]
  StartupNetworkMagic {}                    -> ["StartupNetworkMagic"]
  StartupSocketConfigError {}               -> ["StartupSocketConfigError"]
  StartupDBValidation {}                    -> ["StartupDBValidation"]
  NetworkConfigUpdate {}                    -> ["NetworkConfigUpdate"]
  NetworkConfigUpdateError {}               -> ["NetworkConfigUpdateError"]
  NetworkConfig {}                          -> ["NetworkConfig"]
  P2PWarning {}                             -> ["P2PWarning"]
  P2PWarningDevelopementNetworkProtocols {} -> ["P2PWarningDevelopementNetworkProtocols"]
  WarningDevelopmentNetworkProtocols {}     -> ["WarningDevelopmentNetworkProtocols"]
  BICommon {}                               -> ["Common"]
  BIShelley {}                              -> ["ShelleyBased"]
  BIByron {}                                -> ["Byron"]
  BINetwork {}                              -> ["Network"]

instance ( Show (BlockNodeToNodeVersion blk)
         , Show (BlockNodeToClientVersion blk)
         )
        => LogFormatting (StartupTrace blk) where
  forHuman = ppStartupInfoTrace

  forMachine dtal (StartupInfo addresses
                                 localSocket
                                 supportedNodeToNodeVersions
                                 supportedNodeToClientVersions)
      = mkObject (
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
                toJSON (map show . Map.assocs $ supportedNodeToNodeVersions)
            , "nodeToClientVersions" .=
                toJSON (map show . Map.assocs $ supportedNodeToClientVersions)
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
      mkObject [ "kind" .= String "StartupP2PInfo"
               , "diffusionMode" .= String (showT diffusionMode) ]
  forMachine _dtal (StartupTime time) =
      mkObject [ "kind" .= String "StartupTime"
               , "startupTime" .= String ( showT
                                         . (ceiling :: POSIXTime -> Int)
                                         . utcTimeToPOSIXSeconds
                                         $ time
                                         )
               ]
  forMachine _dtal (StartupNetworkMagic networkMagic) =
      mkObject [ "kind" .= String "StartupNetworkMagic"
               , "networkMagic" .= String (showT . unNetworkMagic
                                          $ networkMagic) ]
  forMachine _dtal (StartupSocketConfigError err) =
      mkObject [ "kind" .= String "StartupSocketConfigError"
               , "error" .= String (showT err) ]
  forMachine _dtal StartupDBValidation =
      mkObject [ "kind" .= String "StartupDBValidation"
               , "message" .= String "start db validation" ]
  forMachine _dtal NetworkConfigUpdate =
      mkObject [ "kind" .= String "NetworkConfigUpdate"
               , "message" .= String "ntework configuration update" ]
  forMachine _dtal (NetworkConfigUpdateError err) =
      mkObject [ "kind" .= String "NetworkConfigUpdateError"
               , "error" .= String err ]
  forMachine _dtal (NetworkConfig localRoots publicRoots useLedgerAfter) =
      mkObject [ "kind" .= String "NetworkConfig"
               , "localRoots" .= toJSON localRoots
               , "publicRoots" .= toJSON publicRoots
               , "useLedgerAfter" .= UseLedger useLedgerAfter
               ]
  forMachine _dtal P2PWarning =
      mkObject [ "kind" .= String "P2PWarning"
               , "message" .= String p2pWarningMessage ]
  forMachine _dtal P2PWarningDevelopementNetworkProtocols =
      mkObject [ "kind" .= String "P2PWarningDevelopementNetworkProtocols"
               , "message" .= String p2pWarningDevelopmentNetworkProtocolsMessage ]
  forMachine _ver (WarningDevelopmentNetworkProtocols ntnVersions ntcVersions) =
      mkObject [ "kind" .= String "WarningDevelopmentNetworkProtocols"
               , "message" .= String "enabled development network protocols"
               , "nodeToNodeDevelopmentVersions" .= String (showT ntnVersions)
               , "nodeToClientDevelopmentVersions" .= String (showT ntcVersions)
               ]
  forMachine _dtal (BINetwork BasicInfoNetwork {..}) =
      mkObject [ "kind" .= String "BasicInfoNetwork"
               , "addresses" .= String (showT niAddresses)
               , "diffusionMode"  .= String (showT niDiffusionMode)
               , "dnsProducers" .= String (showT niDnsProducers)
               , "ipProducers" .= String (showT niIpProducers)
               ]
  forMachine _dtal (BIByron BasicInfoByron {..}) =
      mkObject [ "kind" .= String "BasicInfoByron"
               , "systemStartTime" .= String (showT bibSystemStartTime)
               , "slotLength"  .= String (showT bibSlotLength)
               , "epochLength" .= String (showT bibEpochLength)
               ]
  forMachine _dtal (BIShelley BasicInfoShelleyBased {..}) =
      mkObject [ "kind" .= String "BasicInfoShelleyBased"
               , "era"  .= String bisEra
               , "systemStartTime" .= String (showT bisSystemStartTime)
               , "slotLength"  .= String (showT bisSlotLength)
               , "epochLength" .= String (showT bisEpochLength)
               , "slotsPerKESPeriod" .= String (showT bisSlotsPerKESPeriod)
               ]
  forMachine _dtal (BICommon BasicInfoCommon {..}) =
      mkObject [ "kind" .= String "BasicInfoCommon"
               , "configPath" .= String (pack biConfigPath)
               , "networkMagic"  .= String (showT biNetworkMagic)
               , "protocol" .= String biProtocol
               , "version" .= String biVersion
               , "commit" .= String biCommit
               , "nodeStartTime" .= biNodeStartTime
               ]

-- | Pretty print 'StartupInfoTrace'
--
ppStartupInfoTrace :: ( Show (BlockNodeToNodeVersion blk)
                      , Show (BlockNodeToClientVersion blk)
                      )
                   => StartupTrace blk
                   -> Text
ppStartupInfoTrace (StartupInfo addresses
                                localSocket
                                supportedNodeToNodeVersions
                                supportedNodeToClientVersions)
  = pack
  $ "\n" ++ intercalate "\n"
    [ "node addresses:          " ++ intercalate ", " (map ppN2NSocketInfo addresses)
    , "local socket:            " ++ maybe "NONE" ppN2CSocketInfo localSocket
    , "node-to-node versions:\n"
       ++ intercalate "\n"
          (map (\(v, bv) -> show v ++ "\t" ++ show bv)
        . Map.assocs
        $ supportedNodeToNodeVersions)
    , "node-to-client versions:\n"
       ++ intercalate "\n"
          (map (\(v, bv) -> show v ++ "\t" ++ show bv)
        . Map.assocs
        $ supportedNodeToClientVersions)
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

ppStartupInfoTrace NetworkConfigUpdate = "Performing topology configuration update"
ppStartupInfoTrace (NetworkConfigUpdateError err) = err
ppStartupInfoTrace (NetworkConfig localRoots publicRoots useLedgerAfter) =
    pack
  $ intercalate "\n"
  [ "\nLocal Root Groups:"
  , "  " ++ intercalate "\n  " (map (\(x,y) -> show (x, Map.assocs y))
                                    localRoots)
  , "Public Roots:"
  , "  " ++ intercalate "\n  " (map show publicRoots)
  , case useLedgerAfter of
      UseLedgerAfter slotNo -> "Get root peers from the ledger after slot "
                            ++ show (unSlotNo slotNo)
      DontUseLedger         -> "Don't use ledger to get root peers."
  ]

ppStartupInfoTrace P2PWarning = p2pWarningMessage

ppStartupInfoTrace P2PWarningDevelopementNetworkProtocols =
    p2pWarningDevelopmentNetworkProtocolsMessage

ppStartupInfoTrace (WarningDevelopmentNetworkProtocols ntnVersions ntcVersions) =
     "enabled development network protocols: "
  <> showT ntnVersions
  <> " "
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
      "unsupported and unverified version of "
   <> "`cardano-node` with peer-to-peer networking capabilities"

p2pWarningDevelopmentNetworkProtocolsMessage :: Text
p2pWarningDevelopmentNetworkProtocolsMessage =
    "peer-to-peer requires TestEnableDevelopmentNetworkProtocols to be set to True"


docStartupInfo :: Documented (StartupTrace blk)
docStartupInfo = Documented [
    DocMsg
      (BICommon anyProto)
      []
      "_biConfigPath_: is the path to the config in use. \
      \\n_biProtocol_: is the name of the protocol, e.g. \"Byron\", \"Shelley\" \
      \or \"Byron; Shelley\". \
      \\n_biVersion_: is the version of the node software running. \
      \\n_biCommit_: is the commit revision of the software running. \
      \\n_biNodeStartTime_: gives the time this node was started."
  , DocMsg
      (BIShelley anyProto)
      []
      "bisEra is the current era, e.g. \"Shelley\", \"Allegra\", \"Mary\" \
      \or \"Alonzo\". \
      \\n_bisSystemStartTime_: TODO JNF \
      \\n_bisSlotLength_: gives the length of a slot as time interval. \
      \\n_bisEpochLength_: gives the number of slots which forms an epoch. \
      \\n_bisSlotsPerKESPeriod_: gives the slots per KES period."
  , DocMsg
      (BIByron anyProto)
      []
      "_bibSystemStartTime_: TODO JNF \
      \\n_bibSlotLength_: gives the length of a slot as time interval. \
      \\n_bibEpochLength_: gives the number of slots which forms an epoch."
  , DocMsg
      (BINetwork anyProto)
      []
      "_niAddresses_: IPv4 or IPv6 socket ready to accept connections\
      \or diffusion addresses. \
      \\n_niDiffusionMode_: shows if the node runs only initiator or both\
      \initiator or responder node. \
      \\n_niDnsProducers_: shows the list of domain names to subscribe to. \
      \\n_niIpProducers_: shows the list of ip subscription addresses."
  ]

--
-- Utils
--

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
