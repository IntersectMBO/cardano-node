{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Node.Types
  ( -- * Configuration
    AdjustFilePaths(..)
  , ConfigError(..)
  , ConfigYamlFilePath(..)
  , DbFile(..)
  , GenesisFile(..)
  , ProtocolFilepaths (..)
  , GenesisHash(..)
  , MaxConcurrencyBulkSync(..)
  , MaxConcurrencyDeadline(..)
    -- * Networking
  , TopologyFile(..)
  , NodeDiffusionMode (..)
    -- * Consensus protocol configuration
  , NodeByronProtocolConfiguration(..)
  , NodeHardForkProtocolConfiguration(..)
  , NodeProtocolConfiguration(..)
  , NodeShelleyProtocolConfiguration(..)
  , NodeAlonzoProtocolConfiguration(..)
  , VRFPrivateKeyFilePermissionError(..)
  , renderVRFPrivateKeyFilePermissionError
  ) where

import           Cardano.Prelude
import           Prelude (fail)

import           Data.Aeson
import qualified Data.Text as Text

import           Cardano.Api
import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Node.Configuration.Socket (SocketConfig (..), PartialSocketConfig (..))
import           Cardano.Node.NodeAddress

--TODO: things will probably be clearer if we don't use these newtype wrappers and instead
-- use records with named fields in the CLI code.
import           Ouroboros.Network.NodeToNode (DiffusionMode (..))

-- | Errors for the cardano-config module.
newtype ConfigError = ConfigErrorFileNotFound FilePath
    deriving Show

-- | Filepath of the configuration yaml file. This file determines
-- all the configuration settings required for the cardano node
-- (logging, tracing, protocol, slot length etc)
newtype ConfigYamlFilePath = ConfigYamlFilePath
  { unConfigPath :: FilePath }
  deriving newtype (Eq, Show)

newtype DbFile = DbFile
  { unDB :: FilePath }
  deriving newtype (Eq, Show)

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

instance FromJSON GenesisFile where
  parseJSON (String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid = fail $ "Parsing of GenesisFile failed due to type mismatch. "
                          <> "Encountered: " <> show invalid

newtype MaxConcurrencyBulkSync = MaxConcurrencyBulkSync
  { unMaxConcurrencyBulkSync :: Word }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)

newtype MaxConcurrencyDeadline = MaxConcurrencyDeadline
  { unMaxConcurrencyDeadline :: Word }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)


-- | Newtype wrapper which provides 'FromJSON' instance for 'DiffusionMode'.
--
newtype NodeDiffusionMode
  = NodeDiffusionMode { getDiffusionMode :: DiffusionMode }
  deriving newtype Show

instance FromJSON NodeDiffusionMode where
    parseJSON (String str) =
      case str of
        "InitiatorOnly"
          -> pure $ NodeDiffusionMode InitiatorOnlyDiffusionMode
        "InitiatorAndResponder"
          -> pure $ NodeDiffusionMode InitiatorAndResponderDiffusionMode
        _ -> fail "Parsing NodeDiffusionMode failed: can be either 'InitiatorOnly' or 'InitiatorAndResponder'"
    parseJSON _ = fail "Parsing NodeDiffusionMode failed"

class AdjustFilePaths a where
  adjustFilePaths :: (FilePath -> FilePath) -> a -> a


data ProtocolFilepaths =
     ProtocolFilepaths {
       byronCertFile        :: !(Maybe FilePath)
     , byronKeyFile         :: !(Maybe FilePath)
     , shelleyKESFile       :: !(Maybe FilePath)
     , shelleyVRFFile       :: !(Maybe FilePath)
     , shelleyCertFile      :: !(Maybe FilePath)
     , shelleyBulkCredsFile :: !(Maybe FilePath)
     } deriving (Eq, Show)

newtype GenesisHash = GenesisHash (Crypto.Hash Crypto.Blake2b_256 ByteString)
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data NodeProtocolConfiguration =
       NodeProtocolConfigurationByron   NodeByronProtocolConfiguration
     | NodeProtocolConfigurationShelley NodeShelleyProtocolConfiguration
     | NodeProtocolConfigurationCardano NodeByronProtocolConfiguration
                                        NodeShelleyProtocolConfiguration
                                        NodeAlonzoProtocolConfiguration
                                        NodeHardForkProtocolConfiguration
  deriving (Eq, Show)

data NodeShelleyProtocolConfiguration =
     NodeShelleyProtocolConfiguration {
       npcShelleyGenesisFile     :: !GenesisFile
     , npcShelleyGenesisFileHash :: !(Maybe GenesisHash)
     }
  deriving (Eq, Show)

data NodeAlonzoProtocolConfiguration =
     NodeAlonzoProtocolConfiguration {
       npcAlonzoGenesisFile     :: !GenesisFile
     , npcAlonzoGenesisFileHash :: !(Maybe GenesisHash)
     }
  deriving (Eq, Show)

data NodeByronProtocolConfiguration =
     NodeByronProtocolConfiguration {
       npcByronGenesisFile                   :: !GenesisFile
     , npcByronGenesisFileHash               :: !(Maybe GenesisHash)
     , npcByronReqNetworkMagic               :: !RequiresNetworkMagic
     , npcByronPbftSignatureThresh           :: !(Maybe Double)

       --TODO: eliminate these two: it can be hard-coded
       -- | Update application name.
     , npcByronApplicationName               :: !Byron.ApplicationName

       -- | Application (ie software) version.
     , npcByronApplicationVersion            :: !Byron.NumSoftwareVersion

       --TODO: eliminate these: it can be done automatically in consensus
       -- | These declare the version of the protocol that the node is prepared
       -- to run. This is usually the version of the protocol in use on the
       -- chain now, but during protocol updates this version will be the one
       -- that we declare that we are ready to move to. This is the endorsement
       -- mechanism for determining when enough block producers are ready to
       -- move to the next version.
       --
     , npcByronSupportedProtocolVersionMajor :: !Word16
     , npcByronSupportedProtocolVersionMinor :: !Word16
     , npcByronSupportedProtocolVersionAlt   :: !Word8
     }
  deriving (Eq, Show)

-- | Configuration relating to a hard forks themselves, not the specific eras.
--
data NodeHardForkProtocolConfiguration =
     NodeHardForkProtocolConfiguration {

       -- | During the development and integration of new eras we wish to be
       -- able to test the hard fork transition into the new era, but we do not
       -- wish to generally have the node advertise that it understands the new
       -- era. Avoiding advertising new development eras until they are ready
       -- makes it practical to include new not-yet-ready eras into the main
       -- release version of the node without the danger that operators on the
       -- mainnet will prematurely advertise that their nodes are capable of
       -- crossing the next hard fork.
       --
       -- It should /always/ remain at the default of false for nodes running
       -- on the mainnet.
       --
       -- This flag should be set to true for nodes taking part in testnets for
       -- testing the new era.
       --
       npcTestEnableDevelopmentHardForkEras :: Bool

       -- | For testing purposes we support specifying that the hard fork
       -- happens at an exact epoch number (ie the first epoch of the new era).
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestShelleyHardForkAtEpoch        :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at a given major protocol version. For example this can be
       -- used to cause the Shelley hard fork to occur at the transition from
       -- protocol version 0 to version 1 (rather than the default of from 1 to
       -- 2) which can make the test setup simpler.
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestShelleyHardForkAtVersion      :: Maybe Word

       -- | For testing purposes we support specifying that the hard fork
       -- happens at an exact epoch number (ie the first epoch of the new era).
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestAllegraHardForkAtEpoch        :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at a given major protocol version.
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestAllegraHardForkAtVersion      :: Maybe Word

       -- | For testing purposes we support specifying that the hard fork
       -- happens at an exact epoch number (ie the first epoch of the new era).
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestMaryHardForkAtEpoch           :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at a given major protocol version.
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
       --
     , npcTestMaryHardForkAtVersion         :: Maybe Word

       -- | For testing purposes we support specifying that the hard fork
       -- happens at an exact epoch number (ie the first epoch of the new era).
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestAlonzoHardForkAtEpoch         :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at a given major protocol version.
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestAlonzoHardForkAtVersion       :: Maybe Word
     }
  deriving (Eq, Show)

newtype TopologyFile = TopologyFile
  { unTopology :: FilePath }
  deriving newtype (Show, Eq)

instance AdjustFilePaths NodeProtocolConfiguration where

  adjustFilePaths f (NodeProtocolConfigurationByron pc) =
    NodeProtocolConfigurationByron (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationShelley pc) =
    NodeProtocolConfigurationShelley (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationCardano pcb pcs pca pch) =
    NodeProtocolConfigurationCardano (adjustFilePaths f pcb)
                                     (adjustFilePaths f pcs)
                                     (adjustFilePaths f pca)
                                     pch

instance AdjustFilePaths NodeByronProtocolConfiguration where
  adjustFilePaths f x@NodeByronProtocolConfiguration {
                        npcByronGenesisFile
                      } =
    x { npcByronGenesisFile = adjustFilePaths f npcByronGenesisFile }

instance AdjustFilePaths NodeShelleyProtocolConfiguration where
  adjustFilePaths f x@NodeShelleyProtocolConfiguration {
                        npcShelleyGenesisFile
                      } =
    x { npcShelleyGenesisFile = adjustFilePaths f npcShelleyGenesisFile }

instance AdjustFilePaths NodeAlonzoProtocolConfiguration where
  adjustFilePaths f x@NodeAlonzoProtocolConfiguration {
                        npcAlonzoGenesisFile
                      } =
    x { npcAlonzoGenesisFile = adjustFilePaths f npcAlonzoGenesisFile }

instance AdjustFilePaths SocketConfig where
  adjustFilePaths f x@SocketConfig{ncSocketPath} =
    x { ncSocketPath = adjustFilePaths f ncSocketPath }

instance AdjustFilePaths PartialSocketConfig where
  adjustFilePaths f x@PartialSocketConfig{pncSocketPath} =
    x { pncSocketPath = adjustFilePaths f pncSocketPath }

instance AdjustFilePaths SocketPath where
  adjustFilePaths f (SocketPath p) = SocketPath (f p)

instance AdjustFilePaths GenesisFile where
  adjustFilePaths f (GenesisFile p) = GenesisFile (f p)

instance AdjustFilePaths a => AdjustFilePaths (Maybe a) where
  adjustFilePaths f = fmap (adjustFilePaths f)

instance AdjustFilePaths a => AdjustFilePaths (Last a) where
  adjustFilePaths f = fmap (adjustFilePaths f)

data VRFPrivateKeyFilePermissionError
  = OtherPermissionsExist FilePath
  | GroupPermissionsExist FilePath
  | GenericPermissionsExist FilePath
  deriving Show

renderVRFPrivateKeyFilePermissionError :: VRFPrivateKeyFilePermissionError -> Text
renderVRFPrivateKeyFilePermissionError err =
  case err of
    OtherPermissionsExist fp ->
      "VRF private key file at: " <> Text.pack fp
      <> " has \"other\" file permissions. Please remove all \"other\" file permissions."

    GroupPermissionsExist fp ->
      "VRF private key file at: " <> Text.pack fp
      <> "has \"group\" file permissions. Please remove all \"group\" file permissions."
    GenericPermissionsExist fp ->
      "VRF private key file at: " <> Text.pack fp
      <> "has \"generic\" file permissions. Please remove all \"generic\" file permissions."

-- | Basic information about the node.
