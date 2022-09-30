{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module  Cardano.TxGenerator.Types
        (module Cardano.TxGenerator.Types)
        where

import           GHC.Natural

import           Cardano.Api

import           Cardano.Api.Shelley (ProtocolParameters)
import qualified Cardano.Ledger.Shelley.API as Ledger (ShelleyGenesis)
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
-- import           Cardano.Node.Protocol.Types (SomeConsensusProtocol)

import           Cardano.TxGenerator.Fund (Fund)

-- convenience alias for use trhougout the API
type ShelleyGenesis       = Ledger.ShelleyGenesis StandardShelley

-- some type aliases to keep compatibility with code in Cardano.Benchmarking
type NumberOfInputsPerTx  = Int
type NumberOfOutputsPerTx = Int
type NumberOfTxs          = Int
type TxAdditionalSize     = Int
type TPSRate              = Double


type TxGenerator era = [Fund] -> [TxOut CtxTx era] -> Either TxGenError (Tx era, TxId)

type FundSource m       = m (Either TxGenError [Fund])
type FundToStore m      = Fund -> m ()
type FundToStoreList m  = [Fund] -> m ()

data PayWithChange
  = PayExact [Lovelace]
  | PayWithChange Lovelace [Lovelace]


data TxGenTxParams = TxGenTxParams
  { txParamFee        :: !Lovelace              -- ^ Transaction fee, in Lovelace
  , txParamAddTxSize  :: !Int                   -- ^ Extra transaction payload, in bytes -- Note [Tx additional size]
  , txParamTTL        :: !SlotNo                -- ^ Time-to-live
  }
  deriving (Show, Eq)

-- defaults taken from: cardano-node/nix/nixos/tx-generator-service.nix
defaultTxGenTxParams :: TxGenTxParams
defaultTxGenTxParams = TxGenTxParams
  { txParamFee        = 10_000_000
  , txParamAddTxSize  = 100
  , txParamTTL        = 1_000_000
  }


data TxEnvironment era = TxEnvironment
  { txEnvNetworkId        :: !NetworkId
  -- , txEnvGenesis          :: !ShelleyGenesis
  -- , txEnvProtocolInfo     :: !SomeConsensusProtocol
  , txEnvProtocolParams   :: !ProtocolParameters
  , txEnvFee              :: TxFee era
  , txEnvMetadata         :: TxMetadataInEra era
  }


data TxGenConfig = TxGenConfig
  { confMinUtxoValue  :: !Lovelace              -- ^ Minimum value required per UTxO entry
  , confTxsPerSecond  :: !Double                -- ^ Strength of generated workload, in transactions per second
  , confInitCooldown  :: !Double                -- ^ Delay between init and main submissions in seconds
  , confTxsInputs     :: !NumberOfInputsPerTx   -- ^ Inputs per transaction
  , confTxsOutputs    :: !NumberOfOutputsPerTx  -- ^ Outputs per transaction
  }
  deriving (Show, Eq)


data TxGenPlutusParams =
    PlutusOn                            -- ^ Generate Txs for a Plutus script with explicit settings
      { plutusScript      :: !FilePath  -- ^ Path to the Plutus script
      , plutusData        :: !Integer   -- ^ Data passed to the Plutus script (for now only an int)
      , plutusRedeemer    :: !Integer   -- ^ Redeemer data passed to the Plutus script (for now only an int)
      , plutusExecMemory  :: !Natural   -- ^ Max. memory available for the Plutus script
      , plutusExecSteps   :: !Natural   -- ^ Max. execution steps available for the Plutus script
      }
  | PlutusAuto                          -- ^ Generate Txs for a Plutus script, choosing settings to max out per Tx script budget
      { plutusAutoScript  :: !FilePath  -- ^ Path to the Plutus script
      }
  | PlutusOff                           -- ^ Do not generate Plutus Txs
  deriving (Show, Eq)


data TxGenError where
  ApiError        :: Cardano.Api.Error e => !e -> TxGenError
  ProtocolError   :: Cardano.Api.Error e => !e -> TxGenError
  TxGenError      :: !String -> TxGenError

deriving instance Show TxGenError

instance Error TxGenError where
  displayError = \case
    ApiError e        -> displayError e
    ProtocolError e   -> displayError e
    TxGenError _      -> ""

{-
Note [Tx additional size]
~~~~~~~~~~~~~~~~~~~~~~~~~
This parameter specifies the additional size (in bytes) of a transaction.
Since one transaction is ([input] + [output] + attributes), its size
is defined by its inputs and outputs. We want to have an ability to
increase a transaction's size without increasing the number of inputs or
outputs. Such a big transaction will give us more real-world results
of benchmarking.
Technically, this parameter specifies the size of the attribute we'll
add to the transaction (by default attributes are empty, so if this
parameter is skipped, attributes will remain empty).
-}
