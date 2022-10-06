{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Cardano.Benchmarking.NixOptions
where

import           Data.Aeson
import           Data.List.NonEmpty
import           GHC.Generics
import           GHC.Natural
import           Prelude

import           Cardano.CLI.Types (SigningKeyFile (..))
import           Cardano.Node.Configuration.NodeAddress (NodeIPv4Address)

import           Cardano.Api (AnyCardanoEra, Lovelace)
import           Cardano.Benchmarking.Script.Aeson (parseJSONFile)
import           Cardano.TxGenerator.Types

parseNixServiceOptions :: FilePath -> IO NixServiceOptions
parseNixServiceOptions = parseJSONFile fromJSON

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

getNodeConfigFile :: NixServiceOptions -> FilePath
getNodeConfigFile opts = case _nix_nodeConfigFile opts of
  Nothing -> error "getNodeConfigFile: This should not happend: nodeConfigFile not set"
  Just fp -> fp

setNodeConfigFile :: NixServiceOptions -> FilePath -> NixServiceOptions
setNodeConfigFile opts filePath = opts {_nix_nodeConfigFile = Just filePath }

jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = stripPrefix }
  where
    stripPrefix :: String -> String
    stripPrefix ('_':'n':'i':'x':'_':baseName) = baseName
    stripPrefix bad = error $ "bad fieldname: " ++ bad

instance ToJSON NixServiceOptions where
  toJSON     = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON NixServiceOptions where
  parseJSON = genericParseJSON jsonOptions
