{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Benchmarking.TxFirehose.Config
  ( Config (..)
  , loadConfig
  ) where

import Control.Monad (when)
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Numeric.Natural (Natural)
import System.Exit (die)

data Config = Config
  { cfgSocketPath        :: !FilePath
  , cfgNetworkMagic      :: !Natural
  , cfgSigningKeyFile    :: !FilePath
  , cfgStakingKeyFile    :: !(Maybe FilePath)
    -- ^ Optional stake signing key. If provided, the derived address is a
    -- base address (payment credential + stake credential); otherwise it
    -- is an enterprise address. Set this when spending from a genesis
    -- entry whose address includes a stake credential (e.g. Shelley
    -- initialFunds with a delegator setup).
  , cfgTps               :: !Double
  , cfgInputsPerTx       :: !Natural
  , cfgOutputsPerTx      :: !Natural
  , cfgFee               :: !Integer
  , cfgReconcileEvery    :: !Double
    -- ^ Requery UTxO by address every N seconds. 0 disables periodic
    -- reconciliation (still requeries at startup and on sync errors).
  }
  deriving Show

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \o -> do
    socketPath     <- o .:  "socketPath"
    networkMagic   <- o .:  "networkMagic"
    signingKeyFile <- o .:  "signingKeyFile"
    stakingKeyFile <- o .:? "stakingKeyFile"
    tps            <- o .:  "tps"
    inputsPerTx    <- o .:? "inputsPerTx"    Aeson..!= 1
    outputsPerTx   <- o .:? "outputsPerTx"   Aeson..!= 1
    fee            <- o .:? "fee"            Aeson..!= 200_000
    reconcileEvery <- o .:? "reconcileEvery" Aeson..!= 60
    pure Config
      { cfgSocketPath      = socketPath
      , cfgNetworkMagic    = networkMagic
      , cfgSigningKeyFile  = signingKeyFile
      , cfgStakingKeyFile  = stakingKeyFile
      , cfgTps             = tps
      , cfgInputsPerTx     = inputsPerTx
      , cfgOutputsPerTx    = outputsPerTx
      , cfgFee             = fee
      , cfgReconcileEvery  = reconcileEvery
      }

loadConfig :: FilePath -> IO Config
loadConfig path = do
  bytes <- Aeson.eitherDecodeFileStrict path
  case bytes of
    Left err -> die $ "Config parse error in " ++ path ++ ": " ++ err
    Right cfg -> do
      when (cfgTps cfg <= 0) $ die "tps must be > 0"
      when (cfgInputsPerTx cfg == 0) $ die "inputsPerTx must be >= 1"
      when (cfgOutputsPerTx cfg == 0) $ die "outputsPerTx must be >= 1"
      when (cfgFee cfg < 0) $ die "fee must be >= 0"
      pure cfg
