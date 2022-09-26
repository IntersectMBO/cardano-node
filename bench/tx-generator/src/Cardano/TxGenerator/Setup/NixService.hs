{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Cardano.TxGenerator.Setup.NixService
       ( NixServiceOptions(..)
       , getNodeConfigFile
       , setNodeConfigFile
       )
       where

import           Data.Aeson
import           Data.List.NonEmpty (NonEmpty)
import           GHC.Generics (Generic)
import           GHC.Natural

import           Cardano.CLI.Types (SigningKeyFile (..))
import           Cardano.Node.Configuration.NodeAddress (NodeIPv4Address)

import           Cardano.Api (AnyCardanoEra, Lovelace)
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
  , _nix_plutusAutoMode   :: Bool
  , _nix_plutusLoopScript :: FilePath
  , _nix_era              :: AnyCardanoEra
  , _nix_plutusMode       :: Bool
  , _nix_plutusScript     :: String
  , _nix_plutusData       :: Integer
  , _nix_plutusRedeemer   :: Integer
  , _nix_executionMemory  :: Natural
  , _nix_executionSteps   :: Natural
  , _nix_nodeConfigFile       :: Maybe FilePath
  , _nix_cardanoTracerSocket  :: Maybe FilePath
  , _nix_sigKey               :: SigningKeyFile
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
