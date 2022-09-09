{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module  Cardano.TxGenerator.Types
        (module Cardano.TxGenerator.Types)
        where

import           Cardano.Api
import           Cardano.Prelude (Text)

import           Cardano.TxGenerator.Fund (Fund)


type TxGenerator era = [Fund] -> [TxOut CtxTx era] -> Either String (Tx era, TxId)

type FundSource m       = m (Either String [Fund])
type FundToStore m      = Fund -> m ()
type FundToStoreList m  = [Fund] -> m ()

data PayWithChange
  = PayExact [Lovelace]
  | PayWithChange Lovelace [Lovelace]

{- TODO:
data NixServiceOptions = NixServiceOptions {
    _nix_debugMode        :: Bool
  , _nix_tx_count         :: NumberOfTxs
  , _nix_init_cooldown    :: Double
  , _nix_era              :: AnyCardanoEra
  , _nix_nodeConfigFile       :: Maybe FilePath
  , _nix_sigKey               :: SigningKeyFile
  , _nix_localNodeSocketPath  :: String
  , _nix_targetNodes          :: NonEmpty NodeIPv4Address
  } deriving (Show, Eq)
-}

data TxGenTxParams = TxGenTxParams
  { txParamFee        :: !Lovelace  -- ^ Transaction fee, in Lovelace
  , txParamAddTxSize  :: !Int       -- ^ Extra transaction payload, in bytes
  , txParamInputs     :: !Int       -- ^ Inputs per transaction
  , txParamOutputs    :: !Int       -- ^ Outputs per transaction
  }
  deriving Show

-- defaults taken from: cardano-node/nix/nixos/tx-generator-service.nix
defaultTxGenTxParams :: TxGenTxParams
defaultTxGenTxParams = TxGenTxParams
  { txParamFee        = 10_000_000
  , txParamAddTxSize  = 100
  , txParamInputs     = 4
  , txParamOutputs    = 4
  }


data TxGenConfig = TxGenConfig
  { confMinUtxoValue  :: !Lovelace  -- ^ Minimum value required per UTxO entry
  , confTxsPerSecond  :: !Double    -- ^ Strength of generated workload, in transactions per second
  , confInitCooldown  :: !Double    -- ^ Delay between init and main submissions in seconds
  }
  deriving Show


data TxGenPlutusParams =
    PlutusOn                            -- ^ Generate Txs for a Plutus script with explicit settings
      { plutusScript      :: !FilePath  -- ^ Path to the Plutus script
      , plutusData        :: !Int       -- ^ Data passed to the Plutus script (for now only an int)
      , plutusRedeemer    :: !Int       -- ^ Redeemer data passed to the Plutus script (for now only an int)
      , plutusExecMemory  :: !Int       -- ^ Max. memory available for the Plutus script
      , plutusExecSteps   :: !Int       -- ^ Max. execution steps available for the Plutus script
      }
  | PlutusAuto                          -- ^ Generate Txs for a Plutus script, choosing settings to max out per Tx script budget
      { plutusScript      :: !FilePath  -- ^ Path to the Plutus script
      }
  | PlutusOff                           -- ^ Do not generate Plutus Txs
  deriving Show


data TxGenError =
    InsufficientFundsForRecipientTx !Lovelace !Lovelace
  -- ^ The calculated expenditure (second value) was not available as a single
  --   UTxO entry.  The first value is the largest single UTxO available.
  | TxFileError !(FileError TextEnvelopeError)
  | SplittingSubmissionError !Text
  | SuppliedUtxoTooSmall !Int !Int
  -- ^ The supplied UTxO size (second value) was less than the requested
  --   number of transactions to send (first value).
  | BadPayloadSize !Text
  deriving Show
