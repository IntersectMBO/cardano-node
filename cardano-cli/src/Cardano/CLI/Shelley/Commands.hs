
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
  , PoolId (..)
  , GenesisFile (..)
  , PrivKeyFile (..)
  , BlockId (..)
  ) where

import           Prelude
import           Data.Text (Text)

import           Cardano.Api
import           Cardano.Api.Typed
                   (StakePoolMetadataReference, StakePoolRelay,
                    KESPeriod(..), NetworkId)
import qualified Cardano.Api.Typed as Typed
import           Cardano.Slotting.Slot (EpochNo (..))
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))

import           Cardano.Config.Types
                  (CertificateFile (..), MetaDataFile, NodeAddress,
                   PoolMetaDataFile (..), SigningKeyFile(..),
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
  = AddressKeyGen VerificationKeyFile SigningKeyFile
  | AddressKeyHash VerificationKeyFile (Maybe OutputFile)
  | AddressBuild VerificationKeyFile (Maybe VerificationKeyFile) Network (Maybe OutputFile)
  | AddressBuildMultiSig  --TODO
  | AddressInfo Text
  deriving (Eq, Show)

data StakeAddressCmd
  = StakeAddressKeyGen VerificationKeyFile SigningKeyFile
  | StakeAddressBuild VerificationKeyFile Network (Maybe OutputFile)
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
      [Typed.TxIn]
      [Typed.TxOut Typed.Shelley]
      SlotNo
      Typed.Lovelace
      [CertificateFile]
      [(Typed.StakeAddress, Typed.Lovelace)]
      (Maybe MetaDataFile)
      (Maybe UpdateProposalFile)
      TxBodyFile
  | TxSign TxBodyFile [SigningKeyFile] (Maybe Typed.NetworkId) TxFile
  | TxWitness       -- { transaction :: Transaction, key :: PrivKeyFile, nodeAddr :: NodeAddress }
  | TxSignWitness   -- { transaction :: Transaction, witnesses :: [Witness], nodeAddr :: NodeAddress }
  | TxCheck         -- { transaction :: Transaction, nodeAddr :: NodeAddress }
  | TxSubmit FilePath Network
  | TxCalculateMinFee
      TxBodyFile
      (Maybe Typed.NetworkId)
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
      Typed.Lovelace
      -- ^ Pool pledge.
      Typed.Lovelace
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
      Network
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
  | QueryProtocolParameters Network (Maybe OutputFile)
  | QueryTip Network (Maybe OutputFile)
  | QueryStakeDistribution Network (Maybe OutputFile)
  | QueryStakeAddressInfo Address Network (Maybe OutputFile)
  | QueryUTxO QueryFilter Network (Maybe OutputFile)
  | QueryVersion NodeAddress
  | QueryLedgerState Network (Maybe OutputFile)
  | QueryStatus NodeAddress
  deriving (Eq, Show)


data BlockCmd
  = BlockInfo BlockId NodeAddress
  deriving (Eq, Show)


data GovernanceCmd
  = GovernanceMIRCertificate MIRPot [VerificationKeyFile] [Typed.Lovelace] OutputFile
  | GovernanceProtocolUpdate SigningKeyFile -- { parameters :: ProtocolParams, nodeAddr :: NodeAddress }
  | GovernanceUpdateProposal OutputFile EpochNo
                             [VerificationKeyFile]
                             Typed.ProtocolParametersUpdate
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
  = GenesisCreate GenesisDir Word Word (Maybe SystemStart) (Maybe Typed.Lovelace) NetworkId
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

newtype OutputFile
  = OutputFile FilePath
  deriving (Eq, Show)

newtype PoolId
  = PoolId String -- Probably not a String
  deriving (Eq, Show)

newtype GenesisDir
  = GenesisDir FilePath
  deriving (Eq, Show)

data ITNKeyFile
  = ITNVerificationKeyFile VerificationKeyFile
  | ITNSigningKeyFile SigningKeyFile
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
