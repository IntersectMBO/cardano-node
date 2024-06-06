{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Gov.GovActionTimeout
  ( hprop_check_gov_action_timeout
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (EpochInterval (EpochInterval, unEpochInterval))

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Data.String (fromString)
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Process.Cli.DRep (makeActivityChangeProposal)
import           Testnet.Process.Run (mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Types

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Test that SPOs cannot vote on a Protocol Parameter change
-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Gov Action Timeout/"'@
hprop_check_gov_action_timeout :: Property
hprop_check_gov_action_timeout = integrationWorkspace "gov-action-timeout" $ \tempAbsBasePath' ->
                                 H.runWithDefaultWatchdog_ $ do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- Create default testnet
  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 200
        , cardanoNodeEra = cEra
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  PoolNode{poolRuntime} <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolRuntime
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath poolRuntime

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  baseDir <- H.createDirectoryIfMissing $ gov </> "output"

  -- Figure out expiration time for proposals

  govActionLifetime <- getGovActionLifetime epochStateView ceo
  H.note_ $ "govActionLifetime: " <> show govActionLifetime

  -- Create a proposal
  (governanceActionTxId, _governanceActionIndex) <-
    makeActivityChangeProposal execConfig epochStateView ceo baseDir "proposal"
                               Nothing (EpochInterval 3) wallet0 (EpochInterval 2)

  -- Wait for proposal to expire
  void $ waitForEpochs epochStateView (EpochInterval $ unEpochInterval govActionLifetime + 1)

  -- Check proposal expired
  mGovernanceActionTxIx <- watchEpochStateUpdate epochStateView (EpochInterval 2) $ \(anyNewEpochState, _, _) ->
      return $ maybeExtractGovernanceActionIndex (fromString governanceActionTxId) anyNewEpochState

  mGovernanceActionTxIx H.=== Nothing

