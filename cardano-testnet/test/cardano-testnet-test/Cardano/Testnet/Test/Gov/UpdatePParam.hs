{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.UpdatePParam
  ( hprop_update_pparam
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Ledger (EpochInterval (..))
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley (createAnchor)

import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Conway.PParams as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.String
import qualified Data.Text as Text
import           Data.Word
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.DRep (createVotingTxBody, generateVoteFiles,
                   retrieveTransactionId, signTx, submitTx)
import           Testnet.Components.Query
import           Testnet.Components.TestWatchdog
import           Testnet.Defaults
import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Util as H
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog.Extras as H

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Update PParams/"'@
hprop_update_pparam :: Property
hprop_update_pparam = H.integrationWorkspace "propose-new-constitution" $ \tempAbsBasePath' -> runWithDefaultWatchdog_ $ do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"
  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      -- Generate model for votes
      allVotes :: [(String, Int)]
      allVotes = zip (concatMap (uncurry replicate) [(4, "yes"), (3, "no"), (2, "abstain")]) [1..]

  annotateShow allVotes

  let numVotes :: Int
      numVotes = length allVotes
  annotateShow numVotes

  guardRailScript <- H.note $ work </> "guard-rail-script.plutusV3"
  H.writeFile guardRailScript $ Text.unpack plutusV3NonSpendingScript
  execConfigOffline <- H.mkExecConfigOffline tempBaseAbsPath

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  proposalAnchorFile <- H.note $ gov </> "sample-proposal-anchor"
  H.writeFile proposalAnchorFile "dummy anchor data"
  proposalAnchorDataBS <- evalIO $ BS.readFile proposalAnchorFile
  proposalAnchorDataHash <- H.execCli' execConfigOffline
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  -- TODO: Update help text for policyid. The script hash is not
  -- only useful for minting scripts
  constitutionScriptHash <- filter (/= '\n') <$>
    H.execCli' execConfigOffline
      [ anyEraToString cEra, "transaction"
      , "policyid"
      , "--script-file", guardRailScript
      ]

  H.note_ $ "Constitution script hash: " <> constitutionScriptHash

  url <- evalMaybe $ L.textToUrl 28 "https://tinyurl.com/3wrwb2as"
  let fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoNodeEra = cEra
        , cardanoNumDReps = numVotes
        }
      anchor = createAnchor url proposalAnchorDataBS
      ScriptHash cScriptHash = fromString constitutionScriptHash
      constitution = L.Constitution anchor $ L.SJust cScriptHash

  alonzoGenesis <- evalEither $ first prettyError defaultAlonzoGenesis
  (startTime, shelleyGenesis') <- getDefaultShelleyGenesis fastTestnetOptions
  let conwayGenesisWithCommittee =
        defaultConwayGenesis { L.cgConstitution = constitution }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:_wallet1:_
    , configurationFile
    } <- cardanoTestnet
           fastTestnetOptions
           conf startTime shelleyGenesis'
           alonzoGenesis conwayGenesisWithCommittee

  PoolNode{poolRuntime} <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket poolRuntime
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath poolRuntime

  epochStateView <- getEpochStateView configurationFile socketPath

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath
  H.note_ $ "Foldblocks config file: " <> unFile configurationFile


  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"

  P.cliStakeAddressKeyGen
    $ KeyPair { verificationKey = File stakeVkeyFp
              , signingKey = File stakeSKeyFp
              }

  -- Attempt a protocol parameters update (witnessed with guard rail script)
  let newCommitteeTermLength = 1000
  pparamsUpdateFp <- H.note $ work </> "protocol-parameters-upate.action"
  void $ H.execCli' execConfig
    [ anyEraToString cEra, "governance", "action", "create-protocol-parameters-update"
    , "--testnet"
    , "--governance-action-deposit", show @Int 2_000_000 -- TODO: retrieve this from conway genesis.
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--constitution-script-hash", constitutionScriptHash
    , "--committee-term-length", show @Word32 newCommitteeTermLength
    , "--out-file", pparamsUpdateFp
    ]

  updateProposalTxBody <- H.note $ work </> "update-proposal.txbody"
  txin4 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  utxo <- findAllUtxos epochStateView sbe

  let utxoJSON1 = encodePretty utxo
  H.lbsWriteFile (work </> "utxo1.json") utxoJSON1
  H.noteShow_ (work </> "utxo1.json")

  let relevantValue = fromMaybe mempty ((\(TxOut _ txVal _ _) -> txOutValueToValue txVal) <$> Map.lookup txin4 utxo)
      adaAtInput = L.unCoin $ selectLovelace relevantValue


  H.noteShow_ adaAtInput
  protocolParametersFile <- H.note $ work </> "protocol-parameters.json"
  void $ H.execCli' execConfig
    [ anyEraToString cEra, "query", "protocol-parameters"
    , "--out-file", protocolParametersFile
    ]

  utxo2 <- findAllUtxos epochStateView sbe
  let utxoJSON2 = encodePretty utxo2
  H.lbsWriteFile (work </> "utxo2.json") utxoJSON2
  H.noteShow_ (work </> "utxo2.json")

  void $ H.execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-estimate"
    , "--shelley-key-witnesses", show @Int 1
    , "--total-utxo-value", show @Integer adaAtInput
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--protocol-params-file", protocolParametersFile
    , "--tx-in", Text.unpack $ renderTxIn txin4
    , "--tx-in-collateral", Text.unpack $ renderTxIn txin4
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 3_000_001
    , "--proposal-file", pparamsUpdateFp
    , "--proposal-script-file", guardRailScript
    , "--proposal-redeemer-value", "0"
    , "--proposal-execution-units", "(2000000,20000000)"
    , "--out-file", updateProposalTxBody
    ]

  updateProposalTx <- H.note $ work </> "update-proposal.tx"

  signedPParamsProposalTx <- signTx execConfig cEra work updateProposalTx
                               (File updateProposalTxBody) [SomeKeyPair $ paymentKeyInfoPair wallet0]


  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", unFile signedPParamsProposalTx
    ]

  -- Need to vote on proposal. Drep threshold must be met
  governanceActionTxIdPParamUpdate <- retrieveTransactionId execConfig signedPParamsProposalTx

  !pparamsPropSubmittedResult
   <- H.leftFailM $ findCondition
        (maybeExtractGovernanceActionIndex (fromString governanceActionTxIdPParamUpdate))
        configurationFile
        socketPath
        (EpochNo 5)

  governanceActionIndexPParams <- H.nothingFail pparamsPropSubmittedResult

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  pparamsVoteFiles <- generateVoteFiles execConfig work "pparams-update-vote-files"
                        governanceActionTxIdPParamUpdate governanceActionIndexPParams
                        [(defaultDRepKeyPair idx, vote) | (vote, idx) <- allVotes]

  -- Submit votes
  pparamsVoteTxBodyFp <- createVotingTxBody execConfig epochStateView sbe work "pparams-vote-tx-body"
                           pparamsVoteFiles wallet0

  let signingKeys = SomeKeyPair <$> (paymentKeyInfoPair wallet0:(defaultDRepKeyPair . snd <$> allVotes))

  pparamsVoteTxFp <- signTx execConfig cEra work "signed-vote-tx" pparamsVoteTxBodyFp signingKeys
  submitTx execConfig cEra pparamsVoteTxFp

  mPParamsUpdate
    <- H.leftFailM $ findCondition
         (checkPParamsUpdated (EpochInterval newCommitteeTermLength))
         configurationFile socketPath (EpochNo 10)

  H.nothingFail mPParamsUpdate

checkPParamsUpdated
  :: EpochInterval -- ^ The epoch interval to check for in the updated protocol parameters
  -> AnyNewEpochState
  -> Maybe ()
checkPParamsUpdated committeeTermLength (AnyNewEpochState sbe nes) =
  let curCommTermLength :: EpochInterval
      curCommTermLength = caseShelleyToBabbageOrConwayEraOnwards
                            (const $ error "Committee max term length only exists in Conway era onwards")
                            (const $ nes ^. L.newEpochStateGovStateL . L.cgsCurPParamsL . L.ppCommitteeMaxTermLengthL)
                            sbe
  in if curCommTermLength == committeeTermLength
     then Just () -- PParams was successfully updated and we terminate the fold.
     else Nothing -- PParams was not updated yet, we continue the fold.
