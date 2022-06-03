{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Node.POM
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Time.Clock (secondsToDiffTime)

import           Cardano.Node.Configuration.POM
import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Types
import           Cardano.Tracing.Config (PartialTraceOptions (..), defaultPartialTraceConfiguration,
                   partialTraceSelectionToEither)
import qualified Ouroboros.Consensus.Node as Consensus (NetworkP2PMode (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy (SnapshotInterval (..))
import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..),
                   DiffusionMode (InitiatorAndResponderDiffusionMode))

import           Hedgehog (Property, discover, withTests, (===))
import qualified Hedgehog
import           Hedgehog.Internal.Property (evalEither, failWith)


-- This is a simple test to check that the POM technique is working as intended.
-- What is entered on the command line via the cli takes precedence and this is
-- tested in the property below.
-- See: https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67

prop_sanityCheck_POM :: Property
prop_sanityCheck_POM =
   withTests 1 . Hedgehog.property $ do
    let combinedPartials = defaultPartialNodeConfiguration
                             <> testPartialYamlConfig
                             <> testPartialCliConfig
        nc = makeNodeConfiguration combinedPartials
    expectedConfig <- evalEither eExpectedConfig
    case nc of
      Left err -> failWith Nothing $ "Partial Options Monoid sanity check failure: " <> err
      Right config -> config === expectedConfig

-- | Example partial configuration theoretically created from a
-- config yaml file.
testPartialYamlConfig :: PartialNodeConfiguration
testPartialYamlConfig =
  PartialNodeConfiguration
    { pncProtocolConfig = Last . Just
                        . NodeProtocolConfigurationShelley
                        $ NodeShelleyProtocolConfiguration
                            (GenesisFile "dummmy-genesis-file") Nothing
    , pncSocketConfig = Last . Just $ SocketConfig (Last Nothing) mempty mempty mempty
    , pncShutdownConfig = Last Nothing
    , pncDiffusionMode = Last Nothing
    , pncSnapshotInterval = mempty
    , pncTestEnableDevelopmentNetworkProtocols = Last Nothing
    , pncMaxConcurrencyBulkSync = Last Nothing
    , pncMaxConcurrencyDeadline = Last Nothing
    , pncLoggingSwitch = Last $ Just True
    , pncLogMetrics = Last $ Just True
    , pncTraceConfig = Last (Just $ PartialTracingOnLegacy defaultPartialTraceConfiguration)
    , pncConfigFile = mempty
    , pncTopologyFile = mempty
    , pncDatabaseFile = mempty
    , pncProtocolFiles = mempty
    , pncValidateDB = mempty
    , pncMaybeMempoolCapacityOverride = mempty
    , pncProtocolIdleTimeout = mempty
    , pncTimeWaitTimeout = mempty
    , pncAcceptedConnectionsLimit = mempty
    , pncTargetNumberOfRootPeers = mempty
    , pncTargetNumberOfKnownPeers = mempty
    , pncTargetNumberOfEstablishedPeers = mempty
    , pncTargetNumberOfActivePeers = mempty
    , pncEnableP2P = Last (Just DisabledP2PMode)
    }

-- | Example partial configuration theoretically created
-- from what was parsed on the command line.
testPartialCliConfig :: PartialNodeConfiguration
testPartialCliConfig =
  PartialNodeConfiguration
    { pncSocketConfig = Last . Just $ SocketConfig mempty mempty mempty mempty
    , pncShutdownConfig = Last . Just $ ShutdownConfig Nothing (Just . ASlot $ SlotNo 42)
    , pncConfigFile   = mempty
    , pncTopologyFile = mempty
    , pncDatabaseFile = mempty
    , pncDiffusionMode = mempty
    , pncSnapshotInterval = Last . Just . RequestedSnapshotInterval $ secondsToDiffTime 100
    , pncTestEnableDevelopmentNetworkProtocols = Last $ Just True
    , pncProtocolFiles = Last . Just $ ProtocolFilepaths Nothing Nothing Nothing Nothing Nothing Nothing
    , pncValidateDB = Last $ Just True
    , pncProtocolConfig = mempty
    , pncMaxConcurrencyBulkSync = mempty
    , pncMaxConcurrencyDeadline = mempty
    , pncLoggingSwitch = mempty
    , pncLogMetrics = mempty
    , pncTraceConfig = Last (Just $ PartialTracingOnLegacy defaultPartialTraceConfiguration)
    , pncMaybeMempoolCapacityOverride = mempty
    , pncProtocolIdleTimeout = mempty
    , pncTimeWaitTimeout = mempty
    , pncAcceptedConnectionsLimit = mempty
    , pncTargetNumberOfRootPeers = mempty
    , pncTargetNumberOfKnownPeers = mempty
    , pncTargetNumberOfEstablishedPeers = mempty
    , pncTargetNumberOfActivePeers = mempty
    , pncEnableP2P = Last (Just DisabledP2PMode)
    }

-- | Expected final NodeConfiguration
eExpectedConfig :: Either Text NodeConfiguration
eExpectedConfig = do
  traceOptions <- partialTraceSelectionToEither
                    (return $ PartialTracingOnLegacy defaultPartialTraceConfiguration)
  return $ NodeConfiguration
    { ncSocketConfig = SocketConfig mempty mempty mempty mempty
    , ncShutdownConfig = ShutdownConfig Nothing (Just . ASlot $ SlotNo 42)
    , ncConfigFile = ConfigYamlFilePath "configuration/cardano/mainnet-config.json"
    , ncTopologyFile = TopologyFile "configuration/cardano/mainnet-topology.json"
    , ncDatabaseFile = DbFile "mainnet/db/"
    , ncProtocolFiles = ProtocolFilepaths Nothing Nothing Nothing Nothing Nothing Nothing
    , ncValidateDB = True
    , ncProtocolConfig = NodeProtocolConfigurationShelley
                           $ NodeShelleyProtocolConfiguration
                             (GenesisFile "dummmy-genesis-file") Nothing
    , ncDiffusionMode = InitiatorAndResponderDiffusionMode
    , ncSnapshotInterval = RequestedSnapshotInterval $ secondsToDiffTime 100
    , ncTestEnableDevelopmentNetworkProtocols = True
    , ncMaxConcurrencyBulkSync = Nothing
    , ncMaxConcurrencyDeadline = Nothing
    , ncLoggingSwitch = True
    , ncLogMetrics = True
    , ncTraceConfig = traceOptions
    , ncMaybeMempoolCapacityOverride = Nothing
    , ncProtocolIdleTimeout = 5
    , ncTimeWaitTimeout = 60
    , ncAcceptedConnectionsLimit =
        AcceptedConnectionsLimit
          { acceptedConnectionsHardLimit = 512
          , acceptedConnectionsSoftLimit = 384
          , acceptedConnectionsDelay     = 5
          }
    , ncTargetNumberOfRootPeers = 100
    , ncTargetNumberOfKnownPeers = 100
    , ncTargetNumberOfEstablishedPeers = 50
    , ncTargetNumberOfActivePeers = 20
    , ncEnableP2P = SomeNetworkP2PMode Consensus.DisabledP2PMode
    }

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
