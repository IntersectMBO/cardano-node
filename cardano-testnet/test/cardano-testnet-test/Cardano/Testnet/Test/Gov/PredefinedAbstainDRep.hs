{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.PredefinedAbstainDRep
  ( hprop_check_predefined_abstain_drep
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyLedgerEra)
import           Cardano.Api.Ledger (EpochInterval (EpochInterval))

import           Cardano.Ledger.Conway.Core (ppNOptL)
import           Cardano.Ledger.Conway.Governance (ConwayGovState, cgsCurPParamsL)
import           Cardano.Ledger.Core (EraPParams)
import           Cardano.Ledger.Shelley.LedgerState (epochStateGovStateL, nesEpochStateL)
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Data.Data (Typeable)
import           Data.String (fromString)
import qualified Data.Text as Text
import           Data.Word (Word16)
import           GHC.Stack (HasCallStack)
import           Lens.Micro ((^.))
import           System.FilePath ((</>))

import           Testnet.Components.Configuration (anyEraToString)
import           Testnet.Components.Query
import           Testnet.Defaults (defaultDRepKeyPair, defaultDelegatorStakeKeyPair)
import           Testnet.Process.Cli.DRep (createCertificatePublicationTxBody, createVotingTxBody,
                   generateVoteFiles)
import qualified Testnet.Process.Cli.Keys as P
import           Testnet.Process.Cli.Transaction (retrieveTransactionId, signTx, submitTx)
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Util as H
import           Testnet.Types (KeyPair (..),
                   PaymentKeyInfo (paymentKeyInfoAddr, paymentKeyInfoPair), PoolNode (..),
                   SomeKeyPair (SomeKeyPair), StakingKey, TestnetRuntime (..), nodeSocketPath)

import           Hedgehog
import qualified Hedgehog.Extras as H

-- | This test creates a default testnet with three DReps delegated to by three
-- separate stake holders (one per DRep). We then do a proposal for an arbitrary
-- parameter change (in this case to the @desiredNumberOfPools@ parameter) to check
-- that it fails, when the first DRep votes "yes" and the last two vote "no". Later
-- we chack that if we change the stake holders under the DReps that vote "no" to
-- delegate to the automate "always abstain" DRep, the same kind of proposal passes.
-- If the proposal passes, it means that the stake was counted as abstaining,
-- because the threshold of minimum participation is 50%, if the stake was not counted as
-- abstaining, the "yes" votes would not have been enough, since they only account
-- for the 33% of the total active stake.
--
-- This test is meant to ensure that delegating to "always abstain" has the desired
-- effect of counting as abstaining for the stake delegated.
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Predefined Abstain DRep/"'@
hprop_check_predefined_abstain_drep :: Property
hprop_check_predefined_abstain_drep = H.integrationWorkspace "test-activity" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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
        { cardanoEpochLength = 200
        , cardanoNodeEra = cEra
        , cardanoNumDReps = 3
        }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:wallet1:wallet2:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  PoolNode{poolRuntime} <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolRuntime
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath poolRuntime

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  initialDesiredNumberOfPools <- getDesiredPoolNumberValue epochStateView ceo

  let newNumberOfDesiredPools = initialDesiredNumberOfPools + 1

  -- Do some proposal and vote yes with the first DRep only
  -- and assert that proposal does NOT pass.
  void $ desiredPoolNumberProposalTest execConfig epochStateView ceo gov "firstProposal"
                                       wallet0 Nothing [(1, "yes")] newNumberOfDesiredPools 3 (Just initialDesiredNumberOfPools) 10

  -- Take the last two stake delegators and delegate them to "Abstain".
  delegateToAlwaysAbstain execConfig epochStateView sbe gov "delegateToAbstain1"
                          wallet1 (defaultDelegatorStakeKeyPair 2)
  delegateToAlwaysAbstain execConfig epochStateView sbe gov "delegateToAbstain2"
                          wallet2 (defaultDelegatorStakeKeyPair 3)

  -- Do some other proposal and vote yes with first DRep only
  -- and assert the new proposal passes now.
  let newNumberOfDesiredPools2 = newNumberOfDesiredPools + 1
  void $ desiredPoolNumberProposalTest execConfig epochStateView ceo gov "secondProposal"
                                       wallet0 Nothing [(1, "yes")] newNumberOfDesiredPools2 0 (Just newNumberOfDesiredPools2) 10

delegateToAlwaysAbstain
  :: (HasCallStack, MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Typeable era)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
                    -- using the 'getEpochStateView' function.
  -> ShelleyBasedEra era -- ^ The Shelley-based era (e.g., 'ConwayEra') in which the transaction will be constructed.
  -> FilePath -- ^ Base directory path where generated files will be stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' folder.
  -> PaymentKeyInfo -- ^ Wallet that will pay for the transaction.
  -> KeyPair StakingKey -- ^ Staking key pair used for delegation.
  -> m ()
delegateToAlwaysAbstain execConfig epochStateView sbe work prefix
                        payingWallet skeyPair@(KeyPair vKeyFile _sKeyFile) = do

  let era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  -- Create vote delegation certificate
  let voteDelegationCertificatePath = baseDir </> "delegation-certificate.delegcert"
  void $ H.execCli' execConfig
    [ anyEraToString cEra, "stake-address", "vote-delegation-certificate"
    , "--always-abstain"
    , "--stake-verification-key-file", unFile vKeyFile
    , "--out-file", voteDelegationCertificatePath
    ]

  -- Compose transaction to publish delegation certificate
  repRegTxBody1 <- createCertificatePublicationTxBody execConfig epochStateView sbe baseDir "del-cert-txbody"
                                                      (File voteDelegationCertificatePath) payingWallet

  -- Sign transaction
  repRegSignedRegTx1 <- signTx execConfig cEra baseDir "signed-reg-tx"
                               repRegTxBody1 [ SomeKeyPair (paymentKeyInfoPair payingWallet)
                                             , SomeKeyPair skeyPair]

  -- Submit transaction
  submitTx execConfig cEra repRegSignedRegTx1

  -- Wait two epochs
  void $ waitForEpochs epochStateView (EpochInterval 1)

desiredPoolNumberProposalTest
  :: (HasCallStack, MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Foldable t)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
  -> ConwayEraOnwards ConwayEra -- ^ The ConwaysEraOnwards witness for the Conway era
  -> FilePath -- ^ Base directory path where generated files will be stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' folder.
  -> PaymentKeyInfo -- ^ Wallet that will pay for the transaction.
  -> Maybe (String, Word16) -- ^ The transaction identifier and index of the previous passed
                            -- governance action if any.
  -> t (Int, String) -- ^ Model of votes to issue as a list of pairs of amount of each vote
                     -- together with the vote (i.e: "yes", "no", "abstain")
  -> Integer -- ^ What to change the @desiredPoolNumber@ to
  -> Integer -- ^ Minimum number of epochs to wait before checking the result
  -> Maybe Integer -- ^ What the expected result is of the change (if anything)
  -> Integer -- ^ Maximum number of epochs to wait while waiting for the result
  -> m (String, Word16)
desiredPoolNumberProposalTest execConfig epochStateView ceo work prefix wallet
                              previousProposalInfo votes change minWait mExpected maxWait = do
  let sbe = conwayEraOnwardsToShelleyBasedEra ceo

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let propVotes :: [DefaultDRepVote]
      propVotes = zip (concatMap (uncurry replicate) votes) [1..]
  annotateShow propVotes

  thisProposal@(governanceActionTxId, governanceActionIndex) <-
    makeDesiredPoolNumberChangeProposal execConfig epochStateView ceo baseDir "proposal"
                                        previousProposalInfo (fromIntegral change) wallet

  voteChangeProposal execConfig epochStateView sbe baseDir "vote"
                     governanceActionTxId governanceActionIndex propVotes wallet

  (EpochNo epochAfterProp) <- getCurrentEpochNo epochStateView
  H.note_ $ "Epoch after \"" <> prefix <> "\" prop: " <> show epochAfterProp

  void $ waitForEpochs epochStateView (EpochInterval $ fromIntegral minWait)
  forM_ mExpected $
    assertNewEpochState epochStateView
                        sbe
                        (EpochInterval $ fromIntegral maxWait)
                        (nesEpochStateL . epochStateGovStateL . cgsCurPParamsL . ppNOptL)
      . fromIntegral

  return thisProposal

makeDesiredPoolNumberChangeProposal
  :: (HasCallStack, H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
  -> ConwayEraOnwards ConwayEra -- ^ The conway era onwards witness for the era in which the transaction will be constructed.
  -> FilePath -- ^ Base directory path where generated files will be stored.
  -> String -- ^ Name for the subfolder that will be created under 'work' folder.
  -> Maybe (String, Word16) -- ^ The transaction identifier and index of the previous passed
                            -- governance action if any.
  -> Word16 -- ^ What to change the @desiredPoolNumber@ to
  -> PaymentKeyInfo -- ^ Wallet that will pay for the transaction.
  -> m (String, Word16)
makeDesiredPoolNumberChangeProposal execConfig epochStateView ceo work prefix
                                    prevGovActionInfo desiredPoolNumber wallet = do

  let sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let stakeVkeyFp = baseDir </> "stake.vkey"
      stakeSKeyFp = baseDir </> "stake.skey"

  P.cliStakeAddressKeyGen
    $ KeyPair { verificationKey = File stakeVkeyFp
              , signingKey = File stakeSKeyFp
              }

  proposalAnchorFile <- H.note $ baseDir </> "sample-proposal-anchor"
  H.writeFile proposalAnchorFile "dummy anchor data"

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  minDRepDeposit <- getMinDRepDeposit epochStateView ceo

  proposalFile <- H.note $ baseDir </> "sample-proposal-file"

  void $ H.execCli' execConfig $
    [ "conway", "governance", "action", "create-protocol-parameters-update"
    , "--testnet"
    , "--governance-action-deposit", show @Integer minDRepDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    ] ++ concatMap (\(prevGovernanceActionTxId, prevGovernanceActionIndex) ->
                      [ "--prev-governance-action-tx-id", prevGovernanceActionTxId
                      , "--prev-governance-action-index", show prevGovernanceActionIndex
                      ]) prevGovActionInfo ++
    [ "--number-of-pools", show desiredPoolNumber
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--out-file", proposalFile
    ]

  proposalBody <- H.note $ baseDir </> "tx.body"
  txIn <- findLargestUtxoForPaymentKey epochStateView sbe wallet

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn txIn
    , "--proposal-file", proposalFile
    , "--out-file", proposalBody
    ]

  signedProposalTx <- signTx execConfig cEra baseDir "signed-proposal"
                             (File proposalBody) [SomeKeyPair $ paymentKeyInfoPair wallet]

  submitTx execConfig cEra signedProposalTx

  governanceActionTxId <- retrieveTransactionId execConfig signedProposalTx

  governanceActionIndex <-
    H.nothingFailM $ watchEpochStateUpdate epochStateView (EpochInterval 1) $ \(anyNewEpochState, _, _) ->
      pure $ maybeExtractGovernanceActionIndex (fromString governanceActionTxId) anyNewEpochState

  pure (governanceActionTxId, governanceActionIndex)

-- A pair of a vote string (i.e: "yes", "no", or "abstain") and the number of
-- a default DRep (from the ones created by 'cardanoTestnetDefault')
type DefaultDRepVote = (String, Int)

-- | Create and issue votes for (or against) a government proposal with default
-- Delegate Representative (DReps created by 'cardanoTestnetDefault') using @cardano-cli@.
voteChangeProposal :: (MonadTest m, MonadIO m, MonadCatch m, H.MonadAssertion m)
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
                    -- using the 'getEpochStateView' function.
  -> ShelleyBasedEra ConwayEra -- ^ The Shelley-based witness for ConwayEra (i.e: ShelleyBasedEraConway).
  -> FilePath -- ^ Base directory path where the subdirectory with the intermediate files will be created.
  -> String -- ^ Name for the subdirectory that will be created for storing the intermediate files.
  -> String -- ^ Transaction id of the governance action to vote.
  -> Word16 -- ^ Index of the governance action to vote in the transaction.
  -> [DefaultDRepVote] -- ^ List of votes to issue as pairs of the vote and the number of DRep that votes it.
  -> PaymentKeyInfo -- ^ Wallet that will pay for the transactions
  -> m ()
voteChangeProposal execConfig epochStateView sbe work prefix
                   governanceActionTxId governanceActionIndex votes wallet = do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  voteFiles <- generateVoteFiles execConfig baseDir "vote-files"
                                 governanceActionTxId governanceActionIndex
                                 [(defaultDRepKeyPair idx, vote) | (vote, idx) <- votes]

  voteTxBodyFp <- createVotingTxBody execConfig epochStateView sbe baseDir "vote-tx-body"
                                     voteFiles wallet

  voteTxFp <- signTx execConfig cEra baseDir "signed-vote-tx" voteTxBodyFp
                     (SomeKeyPair (paymentKeyInfoPair wallet):[SomeKeyPair $ defaultDRepKeyPair n | (_, n) <- votes])
  submitTx execConfig cEra voteTxFp

-- | Obtains the @desiredPoolNumberValue@ from the protocol parameters.
-- The @desiredPoolNumberValue@ or (@k@ in the spec) is the protocol parameter
-- that defines what is the optimal number of SPOs. It is a tradeoff between
-- decentralization and efficiency and the spec suggest it should be between 100 an 1000.
-- Changing this parameter will indirectly affect how easy it is to saturate a pool in order to
-- incentivize that the number of SPOs stays close to the parameter value.
getDesiredPoolNumberValue :: (EraPParams (ShelleyLedgerEra era), H.MonadAssertion m, MonadTest m, MonadIO m)
  => EpochStateView
  -> ConwayEraOnwards era
  -> m Integer
getDesiredPoolNumberValue epochStateView ceo = do
   govState :: ConwayGovState era <- getGovState epochStateView ceo
   return $ toInteger $ govState ^. cgsCurPParamsL
                                  . ppNOptL
