{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.TxGenerator.Setup.NixService
       ( NixServiceOptions (..)
       , NodeDescription (..)
       , defaultKeepaliveTimeout
       , getKeepaliveTimeout
       , getKeepaliveTimeout'
       , getNodeAlias
       , getNodeConfigFile
       , setNodeConfigFile
       , txGenTxParams
       , txGenConfig
       , txGenPlutusParams
       )
       where

import           Cardano.Api (AnyCardanoEra, mapFile)

import           Cardano.CLI.Types.Common (FileDirection (..), SigningKeyFile)
import qualified Cardano.Ledger.Coin as L
import           Cardano.Node.Configuration.NodeAddress (NodeAddress' (..),
                   NodeHostIPv4Address (..), NodeIPv4Address)
import           Cardano.Node.Types (AdjustFilePaths (..))
import           Cardano.TxGenerator.Internal.Orphans ()
import           Cardano.TxGenerator.Types

import           Data.Aeson.Types as Aeson
import           Data.Foldable (find)
import           Data.Function (on)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe (fromMaybe)
import qualified Data.Time.Clock as Clock (DiffTime, secondsToDiffTime)
import           GHC.Generics (Generic)


data NixServiceOptions = NixServiceOptions {
    _nix_debugMode        :: Bool
  , _nix_tx_count         :: NumberOfTxs
  , _nix_tps              :: TPSRate
  , _nix_inputs_per_tx    :: NumberOfInputsPerTx
  , _nix_outputs_per_tx   :: NumberOfOutputsPerTx
  , _nix_tx_fee           :: L.Coin
  , _nix_min_utxo_value   :: L.Coin
  , _nix_add_tx_size      :: TxAdditionalSize
  , _nix_init_cooldown    :: Double
  , _nix_era              :: AnyCardanoEra
  , _nix_plutus           :: Maybe TxGenPlutusParams
  , _nix_keepalive        :: Maybe Integer
  , _nix_nodeConfigFile       :: Maybe FilePath
  , _nix_cardanoTracerSocket  :: Maybe FilePath
  , _nix_sigKey               :: SigningKeyFile In
  , _nix_localNodeSocketPath  :: String
  , _nix_targetNodes          :: NonEmpty NodeDescription
  } deriving (Show, Eq)

deriving instance Generic NixServiceOptions

-- only works on JSON Object types
data NodeDescription =
  NodeDescription {
      -- NodeIPAddress would be agnostic to IPv4 vs. IPv6 and likely
      -- a small investment here.
      ndAddr    :: NodeIPv4Address
    , ndName    :: String
    } deriving (Eq, Show, Generic)

-- { "alias": "foo", "addr": ..., "port": ... }
instance FromJSON NodeDescription where
  parseJSON = withObject "NodeDescription" \v -> do
    unNodeHostIPv4Address
            <- v .:  "addr"    <?> Key "addr"
    naPort  <- fmap toEnum $
                 v .:  "port"  <?> Key "port"
    let naHostAddress = NodeHostIPv4Address {..}
        ndAddr        = NodeAddress {..}
    ndName  <- v .:? "name"    <?> Key "name" .!= show ndAddr
    pure $ NodeDescription {..}

instance ToJSON NodeDescription where
  toJSON NodeDescription {..} = object
       [ "name" .= ndName
       , "addr" .= unNodeHostIPv4Address
       , "port" .= fromEnum naPort ] where
    _addr@NodeAddress {..} = ndAddr
    _hostAddr@NodeHostIPv4Address {..} = naHostAddress


-- Long GC pauses on target nodes can trigger spurious MVar deadlock
-- detection. Increasing this timeout can help mitigate those errors.
-- 10s turned out to be a problem, so it's 30s now.
defaultKeepaliveTimeout :: Clock.DiffTime
defaultKeepaliveTimeout = 30

getKeepaliveTimeout :: NixServiceOptions -> Clock.DiffTime
getKeepaliveTimeout = maybe defaultKeepaliveTimeout Clock.secondsToDiffTime . _nix_keepalive

getKeepaliveTimeout' :: Maybe NixServiceOptions -> Clock.DiffTime
getKeepaliveTimeout' = maybe defaultKeepaliveTimeout getKeepaliveTimeout

getNodeAlias :: NixServiceOptions -> NodeIPv4Address -> Maybe String
getNodeAlias NixServiceOptions {..} ip = ndName <$>
  find ((=:=:= ip) . ndAddr) _nix_targetNodes where
    (=:=:=) = (==) `on` naHostAddress

getNodeConfigFile :: NixServiceOptions -> Maybe FilePath
getNodeConfigFile = _nix_nodeConfigFile

setNodeConfigFile :: NixServiceOptions -> FilePath -> NixServiceOptions
setNodeConfigFile opts filePath = opts { _nix_nodeConfigFile = Just filePath }

-- dropping the '_nix_ prefix of above Haskell ADT field labels is assumed
-- to match JSON attribute names as provided by the Nix service definition
jsonOptions :: Aeson.Options
jsonOptions = Aeson.defaultOptions { fieldLabelModifier = drop 5 }

instance FromJSON NixServiceOptions where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance AdjustFilePaths NixServiceOptions where
  adjustFilePaths f opts
    = opts {
      _nix_nodeConfigFile = f <$> _nix_nodeConfigFile opts
    , _nix_sigKey = mapFile f $ _nix_sigKey opts
    }


-- | This deserialization is not a general one for that type, but custom-tailored
--   to the service definition in: nix/nixos/tx-generator-service.nix
instance FromJSON TxGenPlutusParams where
  parseJSON = Aeson.withObject "TxGenPlutusParams" $ \o ->
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
