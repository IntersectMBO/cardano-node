{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.GovActionTimeout
  ( hprop_check_gov_action_timeout
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (EpochInterval (..))

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Data.Default.Class
import qualified Data.Text as Text
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Process.Cli.DRep (makeActivityChangeProposal)
import           Testnet.Process.Cli.Keys (cliStakeAddressKeyGen)
import           Testnet.Process.Cli.SPO (createStakeKeyRegistrationCertificate)
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog.Extras as H

-- | Test that SPOs cannot vote on a Protocol Parameter change
-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Gov Action Timeout/"'@
hprop_check_gov_action_timeout :: Property
hprop_check_gov_action_timeout = integrationRetryWorkspace 2 "gov-action-timeout" $ \tempAbsBasePath' ->
                                 H.runWithDefaultWatchdog_ $ do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- Create default testnet
  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      eraName = eraToString sbe
      asbe = AnyShelleyBasedEra sbe
      creationOptions = def
        { creationEra = asbe
        , creationGenesisOptions = def { genesisEpochLength = 200 }
        }

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets=wallet0:wallet1:_
    , configurationFile
    }
    <- createAndRunTestnet creationOptions def conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  baseDir <- H.createDirectoryIfMissing $ gov </> "output"

  -- Register stake address
  let stakeCertFp = gov </> "stake.regcert"
      stakeKeys =  KeyPair { verificationKey = File $ gov </> "stake.vkey"
                           , signingKey = File $ gov </> "stake.skey"
                           }
  cliStakeAddressKeyGen stakeKeys
  keyDeposit <- getKeyDeposit epochStateView ceo
  createStakeKeyRegistrationCertificate
    tempAbsPath (AnyShelleyBasedEra sbe) (verificationKey stakeKeys) keyDeposit stakeCertFp


  stakeCertTxBodyFp <- H.note $ work </> "stake.registration.txbody"
  stakeCertTxSignedFp <- H.note $ work </> "stake.registration.tx"

  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  void $ execCli' execConfig
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 10_000_000
    , "--certificate-file", stakeCertFp
    , "--witness-override", show @Int 2
    , "--out-file", stakeCertTxBodyFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "sign"
    , "--tx-body-file", stakeCertTxBodyFp
    , "--signing-key-file", signingKeyFp $ paymentKeyInfoPair wallet1
    , "--signing-key-file", signingKeyFp stakeKeys
    , "--out-file", stakeCertTxSignedFp
    ]

  void $ execCli' execConfig
    [ eraName, "transaction", "submit"
    , "--tx-file", stakeCertTxSignedFp
    ]

  -- make sure that stake registration cert gets into a block
  _ <- waitForBlocks epochStateView 1

  -- Create a proposal
  (governanceActionTxId, _governanceActionIndex) <-
    makeActivityChangeProposal execConfig epochStateView ceo (baseDir </> "proposal")
                               Nothing (EpochInterval 3) stakeKeys wallet0 (EpochInterval 2)

  -- Read the proposal's expiry epoch directly from the gov state.
  -- The RATIFY rule removes expired proposals at the start of epoch
  -- @expiresAfter + 1@, so once @currentEpoch > expiresAfter@ the proposal
  -- must be gone.
  expiresAfter@(EpochNo expiryE) <- H.nothingFailM $
    maybeExtractGovernanceActionExpiry governanceActionTxId <$> getEpochState epochStateView
  H.note_ $ "Proposal expires after epoch: " <> show expiresAfter

  -- Wait until we are at least two epochs past @expiresAfter@, i.e. in
  -- epoch @expiresAfter + 2@ or later. RATIFY removes the proposal at the
  -- first block of epoch @expiresAfter + 1@, but the testnet security
  -- parameter is @k = 5@ blocks while epochs average only ~10 blocks, so a
  -- rollback within the @k@-window can cross the removal boundary. Waiting
  -- a full extra epoch past the boundary makes the removal @k@-deep stable
  -- and eliminates the rollback race.
  EpochNo currentE <- getCurrentEpochNo epochStateView
  let epochsToWait
        | expiryE + 2 > currentE = fromIntegral $ expiryE + 2 - currentE
        | otherwise = 0
  void $ waitForEpochs epochStateView (EpochInterval epochsToWait)

  -- At this point the proposal must be absent from the gov state.
  mGovernanceActionTxIx <-
    maybeExtractGovernanceActionIndex governanceActionTxId <$> getEpochState epochStateView
  mGovernanceActionTxIx === Nothing

