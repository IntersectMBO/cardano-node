{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.PParamChangeFailsSPO
  ( hprop_check_pparam_fails_spo
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Experimental (Some (..))
import           Cardano.Api.Ledger (EpochInterval (..))

import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Data.Default.Class
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           Data.Word (Word16)
import           System.FilePath ((</>))

import           Testnet.Components.Query
import           Testnet.Defaults (defaultSpoColdKeyPair, defaultSpoKeys)
import           Testnet.Process.Cli.DRep
import           Testnet.Process.Cli.Keys (cliStakeAddressKeyGen)
import qualified Testnet.Process.Cli.SPO as SPO
import           Testnet.Process.Cli.SPO (createStakeKeyRegistrationCertificate)
import           Testnet.Process.Cli.Transaction (failToSubmitTx, signTx)
import           Testnet.Process.Run (execCli', mkExecConfig)
import           Testnet.Property.Util (integrationWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog (Property, annotateShow)
import qualified Hedgehog.Extras as H
import           Hedgehog.Internal.Property (MonadTest)
import           Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)

-- | Test that SPOs cannot vote on a Protocol Parameter change
-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/PParam change fails for SPO/"'@
hprop_check_pparam_fails_spo :: Property
hprop_check_pparam_fails_spo = integrationWorkspace "test-pparam-spo" $ \tempAbsBasePath' ->
                                 H.runWithDefaultWatchdog_ $ do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- Create default testnet
  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      asbe = AnyShelleyBasedEra sbe
      eraName = eraToString sbe
      fastTestnetOptions = def { cardanoNodeEra = asbe }
      shelleyOptions = def { genesisEpochLength = 200 }

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets=wallet0:wallet1:_wallet2:_
    , configurationFile
    }
    <- createAndRunTestnet fastTestnetOptions shelleyOptions conf

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
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 10_000_000
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

  let propVotes :: [(String, Int)]
      propVotes = mkVotes [(1, "yes")]
      -- replicate votes requested number of times
      mkVotes :: [(Int, String)] -- ^ [(count, vote)]
              -> [(String, Int)] -- ^ [(vote, ordering number)]
      mkVotes votes = zip (concatMap (uncurry replicate) votes) [1..]
  annotateShow propVotes

  (governanceActionTxId, governanceActionIndex) <-
    makeActivityChangeProposal execConfig epochStateView ceo (baseDir </> "proposal")
                               Nothing (EpochInterval 3) stakeKeys wallet0 (EpochInterval 2)

  failToVoteChangeProposalWithSPOs ceo execConfig epochStateView baseDir "vote"
                                   governanceActionTxId governanceActionIndex propVotes wallet1

-- | Cast votes for a governance action with SPO keys.
failToVoteChangeProposalWithSPOs
  :: (HasCallStack, MonadTest m, MonadIO m, MonadCatch m, H.MonadAssertion m, Typeable era)
  => ConwayEraOnwards era -- ^ The conway era onwards witness for the era in which the
                          -- transaction will be constructed.
  -> H.ExecConfig -- ^ Specifies the CLI execution configuration.v
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
                    -- using the 'getEpochStateView' function.
  -> FilePath -- ^ Base directory path where generated files will be stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' folder.
  -> String -- ^ The transaction id of the governance action to vote.
  -> Word16 -- ^ The index of the governance action to vote.
  -> [([Char], Int)] -- ^ Votes to be casted for the proposal. Each tuple contains the index
                     -- of the default SPO that will make the vote and the type of the vote
                     -- (i.e: "yes", "no", "abstain").
  -> PaymentKeyInfo -- ^ Wallet that will pay for the transaction.
  -> m ()
failToVoteChangeProposalWithSPOs ceo execConfig epochStateView work prefix
                                 governanceActionTxId governanceActionIndex votes wallet = withFrozenCallStack $ do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let sbe = convert ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  voteFiles <- SPO.generateVoteFiles ceo execConfig baseDir "vote-files"
                                     governanceActionTxId governanceActionIndex
                                     [(defaultSpoKeys idx, vote) | (vote, idx) <- votes]

  voteTxBodyFp <- createVotingTxBody execConfig epochStateView sbe baseDir "vote-tx-body"
                                     voteFiles wallet

  let signingKeys = Some (paymentKeyInfoPair wallet):(Some . defaultSpoColdKeyPair . snd <$> votes)
  voteTxFp <- signTx execConfig cEra baseDir "signed-vote-tx" voteTxBodyFp signingKeys

  failToSubmitTx execConfig cEra voteTxFp "DisallowedVoters"
