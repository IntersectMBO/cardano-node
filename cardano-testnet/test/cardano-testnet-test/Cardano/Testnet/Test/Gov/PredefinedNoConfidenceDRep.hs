{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Gov.PredefinedNoConfidenceDRep
  ( hprop_check_predefined_no_confidence_drep
  ) where

import           Cardano.Api as Api

import           Cardano.Testnet

import           Prelude

import           System.FilePath ((</>))

import           Testnet.Components.Query (getEpochStateView)
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Util as H
import           Testnet.Types (PoolNode (..), TestnetRuntime (..), nodeSocketPath)

import           Hedgehog
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Predefined No Confidence DRep/"'@
hprop_check_predefined_no_confidence_drep :: Property
hprop_check_predefined_no_confidence_drep = H.integrationWorkspace "test-activity" $ \tempAbsBasePath' -> do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- Create default testnet with 3 DReps and 3 stake holders delegated, one to each DRep.
  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoNodeEra = cEra
        , cardanoNumDReps = 3
        }

  _testnetRuntime@TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=_wallet0:_wallet1:_wallet2:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  PoolNode{poolRuntime} <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolRuntime
  _execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath poolRuntime

  _epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  _gov <- H.createDirectoryIfMissing $ work </> "governance"

  -- ToDo: Do some proposal and vote yes with all the DReps.
  -- ToDo: ASSERT: that proposal passes.
  -- ToDo: Take the last two stake delegators and delegate them to "No Confidence".
  -- ToDo: This can be done using cardano-cli conway stake-address vote-delegation-certificate --always-no-confidence
  -- ToDo: Do some other proposal and vote yes with all the DReps.
  -- ToDo: ASSERT: the new proposal does NOT pass.
  -- ToDo: Create a no confidence proposal.
  -- ToDo: This can be done using cardano-cli conway governance action create-no-confidence
  -- ToDo: Vote no to the no confidence proposal with all DReps.
  -- ToDo: ASSERT: the no confidence proposal passes.

  success
