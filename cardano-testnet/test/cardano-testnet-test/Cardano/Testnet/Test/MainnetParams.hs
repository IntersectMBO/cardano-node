{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.MainnetParams
  ( hprop_mainnet_params
  ) where

import           Cardano.Testnet

import qualified Data.Aeson as A
import qualified Data.Aeson.Lens as A
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default.Class (def)
import           Lens.Micro ((^?))

import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Cardano (liftToIntegration)
import           Testnet.Start.Types (GenesisOptions (..), TestnetOnChainParams (..))

import           Hedgehog ((/==))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Can get on-chain parameters from blockfrost files/"'@
hprop_mainnet_params :: H.Property
hprop_mainnet_params = integrationRetryWorkspace 2 "mainnet-params" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do

  let creationOptions = def
        { creationGenesisOptions = def { genesisEpochLength = 200 }
        , creationOnChainParams = OnChainParamsFile
            "test/cardano-testnet-test/files/input/blockfrost-params.json"
        }

  -- Generate the sandbox
  conf <- mkConf tmpDir
  liftToIntegration $ createTestnetEnv creationOptions conf

  -- Run testnet with mainnet on-chain params
  TestnetRuntime
    { testnetNodes
    , testnetMagic
    } <- liftToIntegration $ cardanoTestnet (creationNodes creationOptions) def conf

  -- Get a running node
  TestnetNode{nodeSprocket} <- H.headM testnetNodes

  -- Get on-chain parameters from running node
  execConfig <- mkExecConfig tmpDir nodeSprocket testnetMagic
  output <- H.noteM $ execCli' execConfig
      [ "query", "protocol-parameters"
      , "--output-json"
      ]
  params :: A.Value <- H.leftFail $ A.eitherDecode $ B.pack output
  dRepActivity <- H.nothingFail $ params ^? A.key "dRepActivity" . A._Number
  dRepDeposit <- H.nothingFail $ params ^? A.key "dRepDeposit" . A._Number

  -- Those values have been modified on mainnet since their introductions
  dRepActivity /== 100
  dRepDeposit /== 1_000_000
