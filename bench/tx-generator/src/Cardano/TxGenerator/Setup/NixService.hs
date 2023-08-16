{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.TxGenerator.Setup.NixService
       ( NixServiceOptions(..)
       , getNodeConfigFile
       , setNodeConfigFile
       , txGenTxParams
       , txGenConfig
       , txGenPlutusParams
       )
       where

import           Data.Aeson
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)

import           Cardano.CLI.Types.Common (FileDirection (..), SigningKeyFile)
import           Cardano.Node.Configuration.NodeAddress (NodeIPv4Address)
import           Cardano.Node.Types (AdjustFilePaths (..))

import           Cardano.Api (AnyCardanoEra, Lovelace, mapFile)
import           Cardano.TxGenerator.Internal.Orphans ()
import           Cardano.TxGenerator.Types


data NixServiceOptions = NixServiceOptions {
    _nix_debugMode        :: Bool
  , _nix_tx_count         :: NumberOfTxs
  , _nix_tps              :: TPSRate
  , _nix_inputs_per_tx    :: NumberOfInputsPerTx
  , _nix_outputs_per_tx   :: NumberOfOutputsPerTx
  , _nix_tx_fee           :: Lovelace
  , _nix_min_utxo_value   :: Lovelace
  , _nix_add_tx_size      :: TxAdditionalSize
  , _nix_init_cooldown    :: Double
  , _nix_era              :: AnyCardanoEra
  , _nix_plutus           :: Maybe TxGenPlutusParams
  , _nix_nodeConfigFile       :: Maybe FilePath
  , _nix_cardanoTracerSocket  :: Maybe FilePath
  , _nix_sigKey               :: SigningKeyFile In
  , _nix_localNodeSocketPath  :: String
  , _nix_targetNodes          :: NonEmpty NodeIPv4Address
  } deriving (Show, Eq)

deriving instance Generic NixServiceOptions

getNodeConfigFile :: NixServiceOptions -> Maybe FilePath
getNodeConfigFile = _nix_nodeConfigFile

setNodeConfigFile :: NixServiceOptions -> FilePath -> NixServiceOptions
setNodeConfigFile opts filePath = opts {_nix_nodeConfigFile = Just filePath }

-- dropping the '_nix_ prefix of above Haskell ADT field labels is assumed
-- to match JSON attribute names as provided by the Nix service definition
jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = stripPrefix }
  where
    stripPrefix :: String -> String
    stripPrefix = drop 5

instance FromJSON NixServiceOptions where
  parseJSON = genericParseJSON jsonOptions

instance AdjustFilePaths NixServiceOptions where
  adjustFilePaths f opts
    = opts {
      _nix_nodeConfigFile = f <$> _nix_nodeConfigFile opts
    , _nix_sigKey = mapFile f $ _nix_sigKey opts
    }


-- | This deserialization is not a general one for that type, but custom-tailored
--   to the service definition in: nix/nixos/tx-generator-service.nix
instance FromJSON TxGenPlutusParams where
  parseJSON = withObject "TxGenPlutusParams" $ \o ->
    PlutusOn
      <$> o .: "type"
      <*> o .: "script"
      <*> o .:? "datum"
      <*> o .:? "redeemer"
      <*> o .:? "limitExecutionMem"
      <*> o .:? "limitExecutionSteps"


---- mapping of Nix service options to API types

txGenTxParams :: NixServiceOptions -> TxGenTxParams
txGenTxParams NixServiceOptions{..}
  = TxGenTxParams {
    txParamFee = _nix_tx_fee
  , txParamAddTxSize = _nix_add_tx_size
  , txParamTTL = txParamTTL defaultTxGenTxParams
  }

txGenConfig :: NixServiceOptions -> TxGenConfig
txGenConfig NixServiceOptions{..}
  = TxGenConfig {
    confMinUtxoValue = _nix_min_utxo_value
  , confTxsPerSecond = _nix_tps
  , confInitCooldown = _nix_init_cooldown
  , confTxsInputs = _nix_inputs_per_tx
  , confTxsOutputs = _nix_outputs_per_tx
  }

txGenPlutusParams :: NixServiceOptions -> TxGenPlutusParams
txGenPlutusParams
  = fromMaybe PlutusOff . _nix_plutus
