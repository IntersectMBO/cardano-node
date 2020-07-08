
-- | Shelley CLI command types
module Cardano.CLI.Shelley.Commands
  ( -- * CLI command types
    ShelleyCommand (..)
  , AddressCmd (..)
  , StakeAddressCmd (..)
  , TransactionCmd (..)
  , NodeCmd (..)
  , PoolCmd (..)
  , QueryCmd (..)
  , BlockCmd (..)
  , SystemCmd (..)
  , GovernanceCmd (..)
  , GenesisCmd (..)
  , TextViewCmd (..)

    -- * CLI flag types
  , AddressKeyType (..)
  , GenesisDir (..)
  , TxInCount (..)
  , TxOutCount (..)
  , TxShelleyWinessCount (..)
  , TxByronWinessCount (..)
  , ITNKeyFile (..)
  , OpCertCounterFile (..)
  , OutputFile (..)
  , ProtocolParamsFile (..)
  , SigningKeyFile (..)
  , TxBodyFile (..)
  , TxFile (..)
  , VerificationKeyFile (..)
  , GenesisKeyFile (..)
  , MetaDataFile (..)
  , PoolId (..)
  , PoolMetaDataFile (..)
  , GenesisFile (..)
  , PrivKeyFile (..)
  , BlockId (..)
  , QueryFilter (..)
  ) where

import           Prelude
import           Data.Set (Set)
import           Data.Text (Text)

import qualified Cardano.Api as OldApi
import           Cardano.Api.Protocol (ProtocolData)
import           Cardano.Api.Typed hiding (PoolId, Hash)

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))

import           Cardano.Config.Types
                  (CertificateFile (..), NodeAddress, SigningKeyFile(..),
                   UpdateProposalFile(..))
import           Shelley.Spec.Ledger.TxData (MIRPot)

--
-- Shelley CLI command data types
--

-- | All the CLI subcommands under \"shelley\".
--
data ShelleyCommand
  = AddressCmd      AddressCmd
  | StakeAddressCmd StakeAddressCmd
  | TransactionCmd  TransactionCmd
  | NodeCmd         NodeCmd
  | PoolCmd         PoolCmd
  | QueryCmd        QueryCmd
  | BlockCmd        BlockCmd
  | SystemCmd       SystemCmd
  | GovernanceCmd   GovernanceCmd
  | GenesisCmd      GenesisCmd
  | TextViewCmd     TextViewCmd
  deriving (Eq, Show)


data AddressCmd
  = AddressKeyGen AddressKeyType VerificationKeyFile SigningKeyFile
  | AddressKeyHash VerificationKeyFile (Maybe OutputFile)
  | AddressBuild VerificationKeyFile (Maybe VerificationKeyFile) NetworkId (Maybe OutputFile)
  | AddressBuildMultiSig  --TODO
  | AddressInfo Text (Maybe OutputFile)
  | AddressConvertKey SigningKeyFile SigningKeyFile
  deriving (Eq, Show)

data StakeAddressCmd
  = StakeAddressKeyGen VerificationKeyFile SigningKeyFile
  | StakeAddressKeyHash VerificationKeyFile (Maybe OutputFile)
  | StakeAddressBuild VerificationKeyFile NetworkId (Maybe OutputFile)
  | StakeKeyRegister PrivKeyFile NodeAddress
  | StakeKeyDelegate PrivKeyFile PoolId Lovelace NodeAddress
  | StakeKeyDeRegister PrivKeyFile NodeAddress
  | StakeKeyRegistrationCert VerificationKeyFile OutputFile
  | StakeKeyDelegationCert VerificationKeyFile VerificationKeyFile OutputFile
  | StakeKeyDeRegistrationCert VerificationKeyFile OutputFile
  | StakeKeyITNConversion ITNKeyFile (Maybe OutputFile)
  deriving (Eq, Show)


data TransactionCmd
  = TxBuildRaw
      [TxIn]
      [TxOut Shelley]
      SlotNo
      Lovelace
      [CertificateFile]
      [(StakeAddress, Lovelace)]
      [MetaDataFile]
      (Maybe UpdateProposalFile)
      TxBodyFile
  | TxSign TxBodyFile [SigningKeyFile] (Maybe NetworkId) TxFile
  | TxWitness       -- { transaction :: Transaction, key :: PrivKeyFile, nodeAddr :: NodeAddress }
  | TxSignWitness   -- { transaction :: Transaction, witnesses :: [Witness], nodeAddr :: NodeAddress }
  | TxCheck         -- { transaction :: Transaction, nodeAddr :: NodeAddress }
  | TxSubmit FilePath OldApi.Network ProtocolData
  | TxCalculateMinFee
      TxBodyFile
      (Maybe NetworkId)
      ProtocolParamsFile
      TxInCount
      TxOutCount
      TxShelleyWinessCount
      TxByronWinessCount
  | TxGetTxId TxBodyFile
  deriving (Eq, Show)


data NodeCmd
  = NodeKeyGenCold VerificationKeyFile SigningKeyFile OpCertCounterFile
  | NodeKeyGenKES  VerificationKeyFile SigningKeyFile
  | NodeKeyGenVRF  VerificationKeyFile SigningKeyFile
  | NodeKeyHashVRF  VerificationKeyFile (Maybe OutputFile)
  | NodeIssueOpCert VerificationKeyFile SigningKeyFile OpCertCounterFile
                    KESPeriod OutputFile
  deriving (Eq, Show)

data PoolCmd
  = PoolRegister PoolId   -- { operator :: PubKey, owner :: [PubKey], kes :: PubKey, vrf :: PubKey, rewards :: PubKey, cost :: Lovelace, margin :: Margin, nodeAddr :: NodeAddress }
  | PoolReRegister PoolId -- { operator :: PubKey, owner :: [PubKey], kes :: PubKey, vrf :: PubKey, rewards :: PubKey, cost :: Lovelace, margin :: Margin, nodeAddr :: NodeAddress }
  | PoolRetire PoolId EpochNo NodeAddress
  | PoolRegistrationCert
      VerificationKeyFile
      -- ^ Stake pool verification key.
      VerificationKeyFile
      -- ^ VRF Verification key.
      Lovelace
      -- ^ Pool pledge.
      Lovelace
      -- ^ Pool cost.
      Rational
      -- ^ Pool margin.
      VerificationKeyFile
      -- ^ Reward account verification staking key.
      [VerificationKeyFile]
      -- ^ Pool owner verification staking key(s).
      [StakePoolRelay]
      -- ^ Stake pool relays.
      (Maybe StakePoolMetadataReference)
      -- ^ Stake pool metadata.
      NetworkId
      OutputFile
  | PoolRetirementCert
      VerificationKeyFile
      -- ^ Stake pool verification key.
      EpochNo
      -- ^ Epoch in which to retire the stake pool. --TODO: Double check this
      OutputFile
  | PoolGetId VerificationKeyFile
  | PoolMetaDataHash PoolMetaDataFile (Maybe OutputFile)
  deriving (Eq, Show)


data QueryCmd
  = QueryPoolId NodeAddress
  | QueryProtocolParameters ProtocolData NetworkId (Maybe OutputFile)
  | QueryTip ProtocolData NetworkId (Maybe OutputFile)
  | QueryStakeDistribution ProtocolData NetworkId (Maybe OutputFile)
  | QueryStakeAddressInfo StakeAddress ProtocolData NetworkId (Maybe OutputFile)
  | QueryUTxO QueryFilter ProtocolData NetworkId (Maybe OutputFile)
  | QueryVersion NodeAddress
  | QueryLedgerState ProtocolData NetworkId (Maybe OutputFile)
  | QueryStatus NodeAddress
  deriving (Eq, Show)


data BlockCmd
  = BlockInfo BlockId NodeAddress
  deriving (Eq, Show)


data GovernanceCmd
  = GovernanceMIRCertificate MIRPot [VerificationKeyFile] [Lovelace] OutputFile
  | GovernanceProtocolUpdate SigningKeyFile -- { parameters :: ProtocolParams, nodeAddr :: NodeAddress }
  | GovernanceUpdateProposal OutputFile EpochNo
                             [VerificationKeyFile]
                             ProtocolParametersUpdate
  | GovernanceColdKeys SigningKeyFile     -- { genesis :: GenesisKeyFile, keys :: [PubKey], nodeAddr :: NodeAddress }
  deriving (Eq, Show)


data TextViewCmd
  = TextViewInfo !FilePath (Maybe OutputFile)
  deriving (Eq, Show)

data SystemCmd
  = SysStart GenesisFile NodeAddress
  | SysStop NodeAddress
  deriving (Eq, Show)


data GenesisCmd
  = GenesisCreate GenesisDir Word Word (Maybe SystemStart) (Maybe Lovelace) NetworkId
  | GenesisKeyGenGenesis VerificationKeyFile SigningKeyFile
  | GenesisKeyGenDelegate VerificationKeyFile SigningKeyFile OpCertCounterFile
  | GenesisKeyGenUTxO VerificationKeyFile SigningKeyFile
  | GenesisCmdKeyHash VerificationKeyFile
  | GenesisVerKey VerificationKeyFile SigningKeyFile
  | GenesisTxIn VerificationKeyFile NetworkId (Maybe OutputFile)
  | GenesisAddr VerificationKeyFile NetworkId (Maybe OutputFile)
  deriving (Eq, Show)

--
-- Shelley CLI flag/option data types
--

newtype ProtocolParamsFile
  = ProtocolParamsFile FilePath
  deriving (Show, Eq)

newtype TxInCount
  = TxInCount Int
  deriving (Eq, Show)

newtype TxOutCount
  = TxOutCount Int
  deriving (Eq, Show)

newtype TxShelleyWinessCount
  = TxShelleyWinessCount Int
  deriving (Eq, Show)

newtype TxByronWinessCount
  = TxByronWinessCount Int
  deriving (Eq, Show)

newtype BlockId
  = BlockId String -- Probably not a String
  deriving (Eq, Show)

newtype GenesisFile
  = GenesisFile FilePath
  deriving (Eq, Show)

newtype GenesisKeyFile
  = GenesisKeyFile FilePath
  deriving (Eq, Show)

data MetaDataFile = MetaDataFileJSON FilePath
                  | MetaDataFileCBOR FilePath

  deriving (Eq, Show)

newtype OutputFile
  = OutputFile FilePath
  deriving (Eq, Show)

newtype PoolId
  = PoolId String -- Probably not a String
  deriving (Eq, Show)

newtype PoolMetaDataFile = PoolMetaDataFile
  { unPoolMetaDataFile :: FilePath }
  deriving (Eq, Show)

newtype GenesisDir
  = GenesisDir FilePath
  deriving (Eq, Show)

data ITNKeyFile
  = ITNVerificationKeyFile VerificationKeyFile
  | ITNSigningKeyFile SigningKeyFile
  deriving (Eq, Show)

data AddressKeyType
  = AddressKeyShelley
  | AddressKeyShelleyExtended
  | AddressKeyByron
  deriving (Eq, Show)

newtype OpCertCounterFile
  = OpCertCounterFile FilePath
  deriving (Eq, Show)

newtype PrivKeyFile
  = PrivKeyFile FilePath
  deriving (Eq, Show)

newtype TxBodyFile
  = TxBodyFile FilePath
  deriving (Eq, Show)

newtype TxFile
  = TxFile FilePath
  deriving (Eq, Show)

newtype VerificationKeyFile
  = VerificationKeyFile FilePath
  deriving (Eq, Show)

-- | UTxO query filtering options.
data QueryFilter
  = FilterByAddress !(Set (Address Shelley))
  | NoFilter
  deriving (Eq, Show)
