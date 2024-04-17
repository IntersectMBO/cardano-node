{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Gov.PredefinedAbstainDRep
  ( hprop_check_predefined_abstain_drep
  ) where

import           Cardano.Api as Api

import           Cardano.Testnet

import           Prelude

import           System.FilePath ((</>))

import           Testnet.Components.Query (getEpochStateView)
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Util as H
import           Testnet.Runtime
import           Testnet.Types (PoolNode (..), TestnetRuntime (..), nodeSocketPath)

import           Hedgehog
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Predefined Abstain DRep/"'@
hprop_check_predefined_abstain_drep :: Property
hprop_check_predefined_abstain_drep = H.integrationWorkspace "test-activity" $ \tempAbsBasePath' -> do
    -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoNodeEra = cEra
        , cardanoNumDReps = 1
        }

  testnetRuntime@TestnetRuntime
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

  startLedgerNewEpochStateLogging testnetRuntime tempAbsPath'

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  _gov <- H.createDirectoryIfMissing $ work </> "governance"

  -- ToDo: Do some proposal and vote yes with the first DRep only.
  -- ToDo: ASSERT: Check that proposal does NOT pass.
  -- ToDo: Take the last two stake delegators and delegate them to "Abstain".
  -- ToDo: This can be done using cardano-cli conway stake-address vote-delegation-certificate --always-abstain
  -- ToDo: Do some other proposal and vote yes with first DRep only.
  -- ToDo: ASSERT: Check the new proposal passes now.

  success
