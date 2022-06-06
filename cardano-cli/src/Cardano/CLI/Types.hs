{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Types
  ( BalanceTxExecUnits (..)
  , CBORObject (..)
  , CddlTx (..)
  , CertificateFile (..)
  , CurrentKesPeriod (..)
  , EpochLeadershipSchedule (..)
  , GenesisFile (..)
  , OpCertEndingKesPeriod (..)
  , OpCertIntervalInformation (..)
  , OpCertOnDiskCounter (..)
  , OpCertNodeAndOnDiskCounterInformation (..)
  , OpCertNodeStateCounter (..)
  , OpCertStartingKesPeriod (..)
  , OutputFormat (..)
  , OutputSerialisation (..)
  , TxBuildOutputOptions(..)
  , ReferenceScriptAnyEra (..)
  , SigningKeyFile (..)
  , SocketPath (..)
  , ScriptFile (..)
  , ScriptDataOrFile (..)
  , ScriptRedeemerOrFile
  , ScriptWitnessFiles (..)
  , ScriptDatumOrFile (..)
  , SlotsTillKesKeyExpiry (..)
  , TransferDirection(..)
  , TxBodyFile (..)
  , TxOutAnyEra (..)
  , TxOutChangeAddress (..)
  , TxOutDatumAnyEra (..)
  , TxFile (..)
  , UpdateProposalFile (..)
  , VerificationKeyFile (..)
  , Stakes (..)
  , Params (..)
  , RequiredSigner (..)
  ) where

import           Cardano.Prelude hiding (Word64)

import           Data.Aeson (FromJSON (..), ToJSON (..), object, pairs, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import           Data.Word (Word64)

import qualified Cardano.Chain.Slotting as Byron

import           Cardano.Api (AddressAny, AnyScriptLanguage, EpochNo, ExecutionUnits, Hash,
                   InAnyCardanoEra, PaymentKey, ScriptData, SlotNo (SlotNo), Tx, TxIn, Value,
                   WitCtxMint, WitCtxStake, WitCtxTxIn)

import qualified Cardano.Ledger.Crypto as Crypto

import           Cardano.Ledger.Shelley.TxBody (PoolParams (..))

-- | Specify whether to render the script cost as JSON
-- in the cli's build command.
data TxBuildOutputOptions = OutputScriptCostOnly FilePath
                          | OutputTxBodyOnly TxBodyFile
                          deriving Show


-- | Specify what the CBOR file is
-- i.e a block, a tx, etc
data CBORObject = CBORBlockByron Byron.EpochSlots
                | CBORDelegationCertificateByron
                | CBORTxByron
                | CBORUpdateProposalByron
                | CBORVoteByron
                deriving Show

newtype CddlTx = CddlTx {unCddlTx :: InAnyCardanoEra Tx}

-- Encompasses stake certificates, stake pool certificates,
-- genesis delegate certificates and MIR certificates.
newtype CertificateFile = CertificateFile { unCertificateFile :: FilePath }
                          deriving newtype (Eq, Show)

newtype CurrentKesPeriod = CurrentKesPeriod { unCurrentKesPeriod :: Word64 } deriving (Eq, Show)

instance ToJSON CurrentKesPeriod where
  toJSON (CurrentKesPeriod k) = toJSON k

instance FromJSON CurrentKesPeriod where
  parseJSON v = CurrentKesPeriod <$> parseJSON v

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

data OpCertNodeAndOnDiskCounterInformation
  -- | The on disk operational certificate has a counter
  -- that is larger than or equal to its corresponding
  -- counter in the node state. The on disk operational
  -- certificate therefore has a valid counter.
  = OpCertOnDiskCounterMoreThanOrEqualToNodeState
      OpCertOnDiskCounter
      OpCertNodeStateCounter
  -- | The on disk operational certificate has a counter
  -- that is less than the counter in the node state. The
  -- on disk operational certificate is invalid in this case.
  | OpCertOnDiskCounterBehindNodeState
      OpCertOnDiskCounter
      OpCertNodeStateCounter
  -- | The corresponding counter for operational certificate
  -- was not found in the node state. This means the relevant
  -- stake pool has not minted a block yet. When the stake pool
  -- has minted a block the corresponding operational certificate's
  -- counter will be present in the node state.
  | OpCertNoBlocksMintedYet
      OpCertOnDiskCounter
  deriving (Eq, Show)

newtype OpCertOnDiskCounter = OpCertOnDiskCounter { unOpCertOnDiskCounter :: Word64 }
                              deriving (Eq, Show)

instance ToJSON OpCertOnDiskCounter where
  toJSON (OpCertOnDiskCounter k) = toJSON k

instance FromJSON OpCertOnDiskCounter where
  parseJSON v = OpCertOnDiskCounter <$> parseJSON v

newtype OpCertNodeStateCounter = OpCertNodeStateCounter { unOpCertNodeStateCounter :: Word64 }
                                 deriving (Eq, Show)

instance ToJSON OpCertNodeStateCounter where
  toJSON (OpCertNodeStateCounter k) = toJSON k

instance FromJSON OpCertNodeStateCounter where
  parseJSON v = OpCertNodeStateCounter <$> parseJSON v

newtype OpCertStartingKesPeriod = OpCertStartingKesPeriod { unOpCertStartingKesPeriod :: Word64 }
                                  deriving (Eq, Show)

instance ToJSON OpCertStartingKesPeriod where
  toJSON (OpCertStartingKesPeriod k) = toJSON k

instance FromJSON OpCertStartingKesPeriod where
  parseJSON v = OpCertStartingKesPeriod <$> parseJSON v

newtype OpCertEndingKesPeriod = OpCertEndingKesPeriod { unOpCertEndingKesPeriod :: Word64 }
                                deriving (Eq, Show)

instance ToJSON OpCertEndingKesPeriod where
  toJSON (OpCertEndingKesPeriod k) = toJSON k

instance FromJSON OpCertEndingKesPeriod where
  parseJSON v = OpCertEndingKesPeriod <$> parseJSON v

data OpCertIntervalInformation
  = OpCertWithinInterval
      OpCertStartingKesPeriod
      OpCertEndingKesPeriod
      CurrentKesPeriod
      SlotsTillKesKeyExpiry
  | OpCertStartingKesPeriodIsInTheFuture
      OpCertStartingKesPeriod
      OpCertEndingKesPeriod
      CurrentKesPeriod
  | OpCertExpired
      OpCertStartingKesPeriod
      OpCertEndingKesPeriod
      CurrentKesPeriod
  | OpCertSomeOtherError -- ^ Shouldn't be possible
      OpCertStartingKesPeriod
      OpCertEndingKesPeriod
      CurrentKesPeriod
  deriving (Eq, Show)

instance FromJSON GenesisFile where
  parseJSON (Aeson.String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid = panic $ "Parsing of GenesisFile failed due to type mismatch. "
                           <> "Encountered: " <> Text.pack (show invalid)

-- | The desired output format.
data OutputFormat
  = OutputFormatHex
  | OutputFormatBech32
  deriving (Eq, Show)

-- | Specify whether to serialise a value according to the ledger's CDDL spec
-- or the cli's intermediate format. Note the intermediate format is defined
-- within SerialiseAsCBOR instances. The plan is to merge TextEnvelope with
-- SerialiseAsCBOR.
data OutputSerialisation
  = OutputLedgerCDDLSerialisation
  | OutputCliSerialisation
  deriving Show

-- | This data structure is used to allow nicely formatted output within the query stake-snapshot command.
--
-- "markPool", "setPool", "goPool" are the three ledger state stake snapshots (from most recent to least recent)
-- go is the snapshot that is used for the current epoch, set will be used in the next epoch,
-- mark for the epoch after that.  "markTotal", "setTotal", "goTotal" record the total active stake for each snapshot.
--
-- This information can be used by community tools to calculate upcoming leader schedules.
data Stakes =  Stakes
  { markPool :: Integer
  , setPool :: Integer
  , goPool :: Integer
  , markTotal :: Integer
  , setTotal :: Integer
  , goTotal :: Integer
  } deriving Show

-- | Pretty printing for stake information
instance ToJSON Stakes where
  toJSON (Stakes m s g mt st gt) = object
    [ "poolStakeMark" .= m
    , "poolStakeSet" .= s
    , "poolStakeGo" .= g
    , "activeStakeMark" .= mt
    , "activeStakeSet" .= st
    , "activeStakeGo" .= gt
    ]

  toEncoding  (Stakes m s g mt st gt) = pairs $ mconcat
    [ "poolStakeMark" .= m
    , "poolStakeSet" .= s
    , "poolStakeGo" .= g
    , "activeStakeMark" .= mt
    , "activeStakeSet" .= st
    , "activeStakeGo" .= gt
    ]

-- | This data structure is used to allow nicely formatted output in the query pool-params command.
-- params are the current pool parameter settings, futureparams are new parameters, retiringEpoch is the
-- epoch that has been set for pool retirement.  Any of these may be Nothing.
data Params crypto = Params
  { poolParameters :: Maybe (PoolParams crypto)
  , futurePoolParameters :: Maybe (PoolParams crypto)
  , retiringEpoch :: Maybe EpochNo
  } deriving Show

-- | Pretty printing for pool parameters
instance Crypto.Crypto crypto =>  ToJSON (Params crypto) where
  toJSON (Params p fp r) = object
    [ "poolParams" .= p
    , "futurePoolParams" .= fp
    , "retiring" .= r
    ]

  toEncoding (Params p fp r) = pairs $ mconcat
    [ "poolParams" .= p
    , "futurePoolParams" .= fp
    , "retiring" .= r
    ]

newtype SigningKeyFile = SigningKeyFile
  { unSigningKeyFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

newtype SocketPath = SocketPath { unSocketPath :: FilePath }

newtype UpdateProposalFile = UpdateProposalFile { unUpdateProposalFile :: FilePath }
                             deriving newtype (Eq, Show)

newtype VerificationKeyFile
  = VerificationKeyFile { unVerificationKeyFile :: FilePath }
  deriving (Eq, Show)

newtype ScriptFile = ScriptFile { unScriptFile :: FilePath }
                     deriving (Eq, Show)

data ScriptDataOrFile = ScriptDataCborFile  FilePath   -- ^ By reference to a CBOR file
                      | ScriptDataJsonFile  FilePath   -- ^ By reference to a JSON file
                      | ScriptDataValue     ScriptData -- ^ By value
  deriving (Eq, Show)

type ScriptRedeemerOrFile = ScriptDataOrFile

-- | This type is like 'ScriptWitness', but the file paths from which to load
-- the script witness data representation.
--
-- It is era-independent, but witness context-dependent.
--
data ScriptWitnessFiles witctx where
     SimpleScriptWitnessFile  :: ScriptFile
                              -> ScriptWitnessFiles witctx

     PlutusScriptWitnessFiles :: ScriptFile
                              -> ScriptDatumOrFile witctx -- TODO: Babbage Modify to allow specification of inline datums
                              -> ScriptRedeemerOrFile
                              -> ExecutionUnits
                              -> ScriptWitnessFiles witctx

     -- TODO: SimpleReferenceScriptWitnessFiles :: ScriptWitnessFiles witctx

     PlutusReferenceScriptWitnessFiles
       :: TxIn
       -> AnyScriptLanguage
       -> ScriptDatumOrFile witctx -- TODO: Babbage Modify to allow specification of inline datums
       -> ScriptRedeemerOrFile
       -> ExecutionUnits
       -> ScriptWitnessFiles witctx


deriving instance Show (ScriptWitnessFiles witctx)

data ScriptDatumOrFile witctx where
     ScriptDatumOrFileForTxIn    :: ScriptDataOrFile
                                 -> ScriptDatumOrFile WitCtxTxIn

     NoScriptDatumOrFileForMint  :: ScriptDatumOrFile WitCtxMint
     NoScriptDatumOrFileForStake :: ScriptDatumOrFile WitCtxStake

deriving instance Show (ScriptDatumOrFile witctx)

newtype SlotsTillKesKeyExpiry = SlotsTillKesKeyExpiry { unSlotsTillKesKeyExpiry :: SlotNo }
                                deriving (Eq, Show)

instance ToJSON SlotsTillKesKeyExpiry where
  toJSON (SlotsTillKesKeyExpiry k) = toJSON k

instance FromJSON SlotsTillKesKeyExpiry where
  parseJSON v = SlotsTillKesKeyExpiry <$> parseJSON v

-- | Determines the direction in which the MIR certificate will transfer ADA.
data TransferDirection = TransferToReserves | TransferToTreasury
                         deriving Show

-- | A TxOut value that is the superset of possibilities for any era: any
-- address type and allowing multi-asset values. This is used as the type for
-- values passed on the command line. It can be converted into the
-- era-dependent 'TxOutValue' type.
--
data TxOutAnyEra = TxOutAnyEra
                     AddressAny
                     Value
                     TxOutDatumAnyEra
                     ReferenceScriptAnyEra
  deriving (Eq, Show)

data TxOutDatumAnyEra = TxOutDatumByHashOnly (Hash ScriptData)
                      | TxOutDatumByHashOf    ScriptDataOrFile
                      | TxOutDatumByValue     ScriptDataOrFile
                      | TxOutInlineDatumByValue ScriptDataOrFile
                      | TxOutDatumByNone
  deriving (Eq, Show)

data ReferenceScriptAnyEra
  = ReferenceScriptAnyEraNone
  | ReferenceScriptAnyEra FilePath
  deriving (Eq, Show)

-- | A partially-specified transaction output indented to use as a change
-- output.
--
-- It does not specify a value, since this will be worked out automatically.
--
-- It does not use any script data hash, since that's generally not used for
-- change outputs.
--
newtype TxOutChangeAddress = TxOutChangeAddress AddressAny
  deriving (Eq, Show)

-- | A flag that differentiates between automatically
-- and manually balancing a tx.
data BalanceTxExecUnits = AutoBalance | ManualBalance

-- | Plutus script required signers
data RequiredSigner
 = RequiredSignerSkeyFile SigningKeyFile
 | RequiredSignerHash (Hash PaymentKey)
 deriving Show

-- | Which leadership schedule we are interested in.
-- TODO: Implement Previous and Next epochs
data EpochLeadershipSchedule
  = CurrentEpoch
  | NextEpoch
  deriving Show

newtype TxBodyFile
  = TxBodyFile FilePath
  deriving Show

newtype TxFile
  = TxFile FilePath
  deriving Show


