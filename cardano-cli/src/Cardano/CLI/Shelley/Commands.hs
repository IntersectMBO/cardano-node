
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
  , GenesisDir (..)
  , TxInCount (..)
  , TxOutCount (..)
  , OpCertCounterFile (..)
  , OutputFile (..)
  , ProtocolParamsFile (..)
  , SigningKeyFile (..)
  , TxBodyFile (..)
  , TxFile (..)
  , VerificationKeyFile (..)
  , GenesisKeyFile (..)
  , PoolId (..)
  , GenesisFile (..)
  , PrivKeyFile (..)
  , BlockId (..)
  ) where

import           Prelude
import           Data.Text (Text)

import           Cardano.Api
import           Cardano.Slotting.Slot (EpochNo (..))
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))

import           Cardano.Config.Types
                  (NodeAddress, SigningKeyFile(..), CertificateFile (..), UpdateProposalFile(..))
import           Cardano.Config.Shelley.OCert (KESPeriod(..))


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
  = AddressKeyGen VerificationKeyFile SigningKeyFile
  | AddressKeyHash VerificationKeyFile (Maybe OutputFile)
  | AddressBuild VerificationKeyFile (Maybe VerificationKeyFile) (Maybe OutputFile)
  | AddressBuildMultiSig  --TODO
  | AddressInfo Text
  deriving (Eq, Show)

data StakeAddressCmd
  = StakeAddressKeyGen VerificationKeyFile SigningKeyFile
  | StakeAddressBuild VerificationKeyFile (Maybe OutputFile)
  | StakeKeyRegister PrivKeyFile NodeAddress
  | StakeKeyDelegate PrivKeyFile PoolId Lovelace NodeAddress
  | StakeKeyDeRegister PrivKeyFile NodeAddress
  | StakeKeyRegistrationCert VerificationKeyFile OutputFile
  | StakeKeyDelegationCert VerificationKeyFile VerificationKeyFile OutputFile
  | StakeKeyDeRegistrationCert VerificationKeyFile OutputFile
  deriving (Eq, Show)


data TransactionCmd
  = TxBuildRaw [TxIn] [TxOut] SlotNo Lovelace [CertificateFile] (Maybe UpdateProposalFile) TxBodyFile
  | TxSign TxBodyFile [SigningKeyFile] Network TxFile
  | TxWitness       -- { transaction :: Transaction, key :: PrivKeyFile, nodeAddr :: NodeAddress }
  | TxSignWitness   -- { transaction :: Transaction, witnesses :: [Witness], nodeAddr :: NodeAddress }
  | TxCheck         -- { transaction :: Transaction, nodeAddr :: NodeAddress }
  | TxSubmit FilePath Network
  | TxCalculateMinFee
      TxInCount
      TxOutCount
      SlotNo
      Network
      [SigningKeyFile]
      [CertificateFile]
      ProtocolParamsFile
  | TxInfo          -- { transaction :: Transaction, nodeAddr :: NodeAddress }
  deriving (Eq, Show)


data NodeCmd
  = NodeKeyGenCold VerificationKeyFile SigningKeyFile OpCertCounterFile
  | NodeKeyGenKES  VerificationKeyFile SigningKeyFile
  | NodeKeyGenVRF  VerificationKeyFile SigningKeyFile
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
      ShelleyCoin
      -- ^ Pool pledge.
      ShelleyCoin
      -- ^ Pool cost.
      ShelleyStakePoolMargin
      -- ^ Pool margin.
      VerificationKeyFile
      -- ^ Reward account verification staking key.
      [VerificationKeyFile]
      -- ^ Pool owner verification staking key(s).
      [ShelleyStakePoolRelay]
      -- ^ Stake pool relays.
      OutputFile
  | PoolRetirementCert
      VerificationKeyFile
      -- ^ Stake pool verification key.
      EpochNo
      -- ^ Epoch in which to retire the stake pool. --TODO: Double check this
      OutputFile
  | PoolGetId VerificationKeyFile

  deriving (Eq, Show)


data QueryCmd
  = QueryPoolId NodeAddress
  | QueryProtocolParameters Network (Maybe OutputFile)
  | QueryTip Network (Maybe OutputFile)
  | QueryStakeDistribution Network (Maybe OutputFile)
  | QueryUTxO QueryFilter Network (Maybe OutputFile)
  | QueryVersion NodeAddress
  | QueryLedgerState Network (Maybe OutputFile)
  | QueryStatus NodeAddress
  deriving (Eq, Show)


data BlockCmd
  = BlockInfo BlockId NodeAddress
  deriving (Eq, Show)


data GovernanceCmd
  = GovernanceMIRCertificate [VerificationKeyFile] [ShelleyCoin] OutputFile
  | GovernanceProtocolUpdate SigningKeyFile -- { parameters :: ProtocolParams, nodeAddr :: NodeAddress }
  | GovernanceUpdateProposal OutputFile EpochNo [VerificationKeyFile] ShelleyPParamsUpdate
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
  = GenesisCreate GenesisDir Word Word (Maybe SystemStart) (Maybe Lovelace)
  | GenesisKeyGenGenesis VerificationKeyFile SigningKeyFile
  | GenesisKeyGenDelegate VerificationKeyFile SigningKeyFile OpCertCounterFile
  | GenesisKeyGenUTxO VerificationKeyFile SigningKeyFile
  | GenesisKeyHash VerificationKeyFile
  | GenesisVerKey VerificationKeyFile SigningKeyFile
  | GenesisTxIn VerificationKeyFile (Maybe OutputFile)
  | GenesisAddr VerificationKeyFile (Maybe OutputFile)
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

newtype BlockId
  = BlockId String -- Probably not a String
  deriving (Eq, Show)

newtype GenesisFile
  = GenesisFile FilePath
  deriving (Eq, Show)

newtype GenesisKeyFile
  = GenesisKeyFile FilePath
  deriving (Eq, Show)

newtype OutputFile
  = OutputFile FilePath
  deriving (Eq, Show)

newtype PoolId
  = PoolId String -- Probably not a String
  deriving (Eq, Show)

newtype GenesisDir
  = GenesisDir FilePath
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
