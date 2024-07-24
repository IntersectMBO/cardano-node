{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{- These are all pretty reasonable warning options.
 - Maybe all but unused-imports should be suppressed in the cabal
 - configuration higher up in the codebase?
 - These disablings of warnings and makings of warnings not errors
 - ("softenings") aren't needed anymore as the code now stands.
 - It could still be useful to keep these ready to reactivate in
 - the event of rapid code restructurings until it comes time to
 - do the final cleanup of the commit sequence.
 -* OPTIONS_GHC -fno-warn-incomplete-uni-patterns  *-
 -* OPTIONS_GHC -Wno-error=partial-type-signatures *-
 -* OPTIONS_GHC -Wno-error=unused-imports          *-
 -* OPTIONS_GHC -Wno-unrecognised-pragmas          *-
 -}

{- These also used to be needed to pass hlint checks, but the warnings
 - have likewise since been silenced, so they're no longer needed to
 - make progress in the interim. In like fashion, it may be useful to
 - let these disabled pragmas linger until the point of final clean-up
 - of the commit sequence for intensive restructurings.
 -* HLINT ignore "Unused LANGUAGE pragma"          *-
 -* HLINT ignore "Avoid lambda using `infix`"      *-
 -}

module  Cardano.TxGenerator.GovExample where

import           Cardano.Api hiding (StakeAddress, StakeCredential)
import qualified Cardano.Api as Api (NetworkId (..))
import qualified Cardano.Api.Byron as Byron (WitnessNetworkIdOrByronAddress (..))
import qualified Cardano.Api.Ledger as Ledger (ConwayEraTxCert (..)
                   , ConwayTxCert (..)
                   , Credential (..)
                   , EraCrypto
                   , KeyRole (..)
                   , PParams (..)
                   , Prices (..)
                   , ShelleyEraTxCert (..)
                   , ShelleyTxCert (..)
                   , TxCert
                   , pattern UnRegDepositTxCert
                   , pattern UnRegTxCert
                   , ppPricesL)
import           Cardano.Api.Shelley (GovernanceAction (..)
                   , Hash (..)
                   , PoolId
                   , PlutusScriptOrReferenceInput (..)
                   , Proposal (..)
                   , ReferenceScript (..)
                   , ShelleyLedgerEra
                   , SimpleScriptOrReferenceInput (..)
                   , StakeAddress (..)
                   , StakeCredential (..)
                   , Vote (..)
                   , VotingProcedures (..)
                   , LedgerProtocolParameters (..))
import qualified Cardano.Api.Shelley as Api (
                     convertToLedgerProtocolParameters
                   , createVotingProcedure
                   , fromShelleyStakeCredential
                   , getTxBodyAndWitnesses)

import qualified Cardano.Binary as CBOR (serialize')
import qualified Cardano.Chain.Common as Byron (AddrAttributes (..)
                   , NetworkMagic (..), mkAttributes)
import qualified Cardano.CLI.EraBased.Commands.Transaction as Cmd
-- Unqualified imports of types need to be re-qualified before a PR.
-- Adjust line break to stylish-haskell/fourmolu/etc.
import           Cardano.CLI.Json.Friendly (FriendlyFormat (..)
                   , friendlyTx
                   , friendlyTxBody)
import           Cardano.CLI.Read (ByronOrShelleyWitness (..)
                   , IncompleteCddlTxBody (..)
                   , ShelleyBootstrapWitnessSigningKeyData (..))
import qualified Cardano.CLI.Read as CLI (categoriseSomeSigningWitness
                   , fileOrPipe
                   , readFileScriptInAnyLang
                   , readFileTx
                   , readFileTxBody
                   , readFileTxKeyWitness
                   , readScriptDataOrFile
                   , readScriptWitness
                   , readScriptWitnessFiles
                   , readScriptWitnessFilesTuple
                   , readRequiredSigner
                   , readTxGovernanceActions
                   , readTxMetadata
                   , readTxUpdateProposal
                   , readVotingProceduresFiles
                   , readWitnessSigningData)
import qualified Cardano.CLI.EraBased.Run.Genesis as CLI (
                     readProtocolParameters)
import           Cardano.CLI.Types.Common (CertificateFile (..)
                   , InputTxBodyOrTxFile (..)
                   {-
                    - QueryOutputFormat is renamed in the latest
                    - cardano-cli. For whatever reason, I'm unable to
                    - successfully get a sufficiently recent cardano-cli
                    - tag to actually get used, so in the meantime,
                    - I'm using the elder name, QueryOutputFormat.
                    - , OutputFormatJsonOrText (..)
                    - 
                    -}
                   {- , QueryOutputFormat (..) -}
                   , ReferenceScriptAnyEra (..)
                   , ReferenceScriptSize (..)
                   , ScriptWitnessFiles (..)
                   , TxBuildOutputOptions (..)
                   , TxByronWitnessCount (..)
                   , TxOutAnyEra (..)
                   , TxOutChangeAddress (..)
                   , TxOutDatumAnyEra (..)
                   , TxOutShelleyBasedEra (..)
                   , TxShelleyWitnessCount (..)
                   , TxTreasuryDonation (..)
                   , ViewOutputFormat (..)
                   , WitnessFile (..))
import           Cardano.CLI.Types.Errors.BootstrapWitnessError (
                     BootstrapWitnessError (..))
import           Cardano.CLI.Types.Errors.NodeEraMismatchError (
                     NodeEraMismatchError (..))
import           Cardano.CLI.Types.Errors.TxCmdError (
                     AnyTxCmdTxExecUnitsErr (..)
                   , AnyTxBodyErrorAutoBalance (..)
                   , TxCmdError (..))
import qualified Cardano.CLI.Types.Errors.TxValidationError as CLI (
                     convertToTxVotingProcedures
                   , convToTxProposalProcedures
                   , validateRequiredSigners
                   , validateTxAuxScripts
                   , validateTxCurrentTreasuryValue
                   , validateTxReturnCollateral
                   , validateTxScriptValidity
                   , validateTxTotalCollateral
                   , validateTxTreasuryDonation
                   , validateTxValidityLowerBound)
import           Cardano.CLI.Types.Governance
                   (AnyVotingStakeVerificationKeyOrHashOrFile (..))
import qualified Cardano.CLI.Types.Output as CLI (renderScriptCosts)
import           Cardano.CLI.Types.TxFeature (TxFeature (..))
import qualified Cardano.Ledger.Api.PParams as Ledger (ppMinFeeAL)
import qualified Cardano.Ledger.Api.Tx.Cert as Ledger (pattern RetirePoolTxCert)
import           Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..), Url)
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Coin as Ledger ()
import           Cardano.Ledger.Crypto -- exports only types and classes
import           Cardano.TxGenerator.FundQueue (Fund (..), FundInEra (..), FundQueue)
import qualified Cardano.TxGenerator.FundQueue as FundQueue (
                     emptyFundQueue, getFundCoin, getFundKey
                   , getFundTxIn, getFundWitness, insertFund, toList)
import           Cardano.TxGenerator.Setup.SigningKey
import           Cardano.TxGenerator.Types (FundSource
                   , FundToStoreList, TxEnvironment (..)
                   , TxGenError (..), TxGenerator)
import           Cardano.TxGenerator.Utils (inputsToOutputsWithFee)
import           Cardano.TxGenerator.UTxO (ToUTxOList, makeToUTxOList, mkUTxOVariant)

import qualified Control.Monad as Monad (foldM, forM)
import           Control.Monad.Trans.State.Strict
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson (eitherDecodeFileStrict', object)
import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import           Data.Bifunctor (bimap, first)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Either (fromRight)
import           Data.Function ((&))
import qualified Data.List as List (foldl', null)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, keysSet)
import qualified Data.Maybe as Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set (elems, fromList, toList, (\\))
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text (pack)
import qualified Data.Text.IO as Text (putStrLn)
import           Data.Type.Equality (TestEquality (..), (:~:) (..))
import           Lens.Micro ((^.))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as
         Consensus (Target (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as
            Net.Tx (SubmitResult (..))
import           System.Exit (die)
import qualified System.IO as IO (print)

import           Paths_tx_generator

-- It may be worth including ConwayEraOnwardsConstraints somehow.


demo :: IO ()
demo = getDataFileName "data/protocol-parameters.json" >>= demo'

demo' :: FilePath -> IO ()
demo' parametersFile = do
  protocolParameters <- either die pure =<< Aeson.eitherDecodeFileStrict' parametersFile
  let
      demoEnv :: TxEnvironment ConwayEra
      demoEnv = TxEnvironment {
          txEnvNetworkId = Api.Mainnet
        , txEnvProtocolParams = protocolParameters
        , txEnvFee = TxFeeExplicit ShelleyBasedEraConway 100000
        , txEnvMetadata = TxMetadataNone
        }

  run1 <- Monad.foldM (worker $ generateTx demoEnv) (FundQueue.emptyFundQueue `FundQueue.insertFund` genesisFund) [1..10]
  run2 <- Monad.foldM (worker $ generateTxM demoEnv) (FundQueue.emptyFundQueue `FundQueue.insertFund` genesisFund) [1..10]
  putStrLn $ "Are run results identical? " ++ show (FundQueue.toList run1 == FundQueue.toList run2)
  where
    worker ::
         Generator (Either TxGenError (Tx ConwayEra))
      -> FundQueue
      -> Int
      -> IO FundQueue
    worker pureGenerator generatorState counter = do
      putStrLn $ "running tx-generator. Iteration : " ++ show counter
      let (res, newState) = runState pureGenerator generatorState
      case res of
        Right tx -> print tx
        Left err -> print err
      return newState

signingKey :: SigningKey PaymentKey
signingKey = fromRight (error "signingKey: parseError") $ parseSigningKeyTE keyData
  where
    keyData = TextEnvelope { teType = TextEnvelopeType "GenesisUTxOSigningKey_ed25519"
              , teDescription = fromString "Genesis Initial UTxO Signing Key"
              , teRawCBOR = "X \vl1~\182\201v(\152\250A\202\157h0\ETX\248h\153\171\SI/m\186\242D\228\NAK\182(&\162"}

drepSigningKey :: SigningKey PaymentKey
drepSigningKey = fromRight (error "drepSigningKey: parseError") $ parseSigningKeyTE keyData
  where
    keyData = TextEnvelope { teType = TextEnvelopeType "DRepSigningKey_ed25519"
                           , teDescription = fromString "Delegate Representative Signing Key"
                           -- This is actually the CBOR as it appeared
                           -- in the JSON file and needs conversion to raw CBOR.
                           , teRawCBOR = "5820ac0757312cf883baa809d8cf6c3c48e86acc70db9c6eb5511666c8b128d9020a" }

genesisTxIn :: TxIn
genesisValue :: TxOutValue ConwayEra

(genesisTxIn, genesisValue) =
  ( TxIn "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162" (TxIx 0)
  , lovelaceToTxOutValue ShelleyBasedEraConway $ Coin 90000000000000
  )

genesisFund :: Fund
genesisFund
  = Fund $ InAnyCardanoEra ConwayEra fundInEra
  where
    fundInEra :: FundInEra ConwayEra
    fundInEra  = FundInEra {
        _fundTxIn = genesisTxIn
      , _fundVal = genesisValue
      , _fundWitness = KeyWitness KeyWitnessForSpending
      , _fundSigningKey = Just signingKey
      }

type Generator = State FundQueue

-- Need to ask Carlos or Aniket what anchors are about.
-- The particular issue is what could be substituted for the URL if it
-- turns out fake ones like I used earlier error out.
-- Cardano.Api.Governance.Actions.createAnchor
--         :: Url -> ByteString -> Anchor StandardCrypto
localGenVote :: forall era . {- ConwayEraOnwardsConstraints era => -} ConwayEraOnwards era -> Vote -> IO ()
localGenVote era vote = do
  let _procedure = Api.createVotingProcedure
                             (era {- eon -} :: ConwayEraOnwards era)
                             (vote {- votingChoice -} :: Vote)
                             (Nothing :: Maybe (Url, Text))
  _ <- shelleyBasedEraConstraints localShelleyBasedEra do
    _ <- pure (undefined :: AnyVotingStakeVerificationKeyOrHashOrFile)
    pure undefined
  pure ()
  where
    localShelleyBasedEra = conwayEraOnwardsToShelleyBasedEra era

-- Call into the CLI.
localCheats :: forall era . {- ConwayEraOnwardsConstraints era => -} [GovernanceAction (ConwayEraOnwards era)]
localCheats = [TreasuryWithdrawal [(undefined :: Network, undefined :: StakeCredential, undefined :: Coin)] SNothing]

-- runTxBuildRaw from CLI
localGenTx :: forall era. ()
  => IsShelleyBasedEra era
  => ShelleyBasedEra era
  -> LedgerProtocolParameters era
  -> (TxInsCollateral era, [Fund])
  -> TxFee era
  -> TxMetadataInEra era
  -> TxGenerator era
localGenTx sbe ledgerParameters (collateral, collFunds) fee metadata inFunds outputs
  = bimap
      ApiError
      (\b -> (signShelleyTransaction (shelleyBasedEra @era) b $ map WitnessPaymentKey allKeys, getTxId b))
      (createAndValidateTransactionBody (shelleyBasedEra @era) txBodyContent)
 where
  allKeys = Maybe.mapMaybe FundQueue.getFundKey $ inFunds ++ collFunds
  txBodyContent = defaultTxBodyContent sbe
    & setTxIns (map (\f -> (FundQueue.getFundTxIn f, BuildTxWith $ FundQueue.getFundWitness f)) inFunds)
    & setTxInsCollateral collateral
    & setTxOuts outputs
    & setTxFee fee
    & setTxValidityLowerBound TxValidityNoLowerBound
    & setTxValidityUpperBound (defaultTxValidityUpperBound sbe)
    & setTxMetadata metadata
    & setTxProtocolParams (BuildTxWith (Just ledgerParameters))


localSourceToStoreTransaction ::
     Monad m
  => TxGenerator era
  -> FundSource m
  -> ([Coin] -> split)
  -> ToUTxOList era split
  -> FundToStoreList m                --inline to ToUTxOList
  -> m (Either TxGenError (Tx era))
localSourceToStoreTransaction txGenerator fundSource inToOut mkTxOut fundToStore =
  fundSource >>= either (return . Left) go
 where
  go inputFunds = do
    let
      -- 'getFundCoin' unwraps the 'TxOutValue' in a fund field
      -- so it's all just 'Lovelace' instead of a coproduct
      -- maintaining distinctions.
      outValues = inToOut $ map FundQueue.getFundCoin inputFunds
      (outputs, toFunds) = mkTxOut outValues
    case txGenerator inputFunds outputs of
        Left err -> return $ Left err
        Right (tx, txId) -> do
          fundToStore $ toFunds txId
          return $ Right tx

generateTx ::
     TxEnvironment ConwayEra
  -> Generator (Either TxGenError (Tx ConwayEra))
generateTx TxEnvironment{..}
  = localSourceToStoreTransaction
        generator
        consumeInputFunds
        computeOutputValues
        (makeToUTxOList $ repeat computeUTxO)
        addNewOutputFunds
  where
    TxFeeExplicit _ fee = txEnvFee

    generator :: TxGenerator ConwayEra
    generator =
        case Api.convertToLedgerProtocolParameters shelleyBasedEra txEnvProtocolParams of
          Right ledgerParameters ->
            localGenTx ShelleyBasedEraConway ledgerParameters collateralFunds txEnvFee txEnvMetadata
          Left err -> \_ _ -> Left (ApiError err)
      where
        -- collateralFunds are needed for Plutus transactions
        collateralFunds :: (TxInsCollateral ConwayEra, [Fund])
        collateralFunds = (TxInsCollateralNone, [])

-- Create a transaction that uses all the available funds.
    consumeInputFunds :: Generator (Either TxGenError [Fund])
    consumeInputFunds = do
      funds <- FundQueue.toList <$> get
      put FundQueue.emptyFundQueue
      return $ Right funds

    addNewOutputFunds :: [Fund] -> Generator ()
    addNewOutputFunds = put . List.foldl' FundQueue.insertFund FundQueue.emptyFundQueue

    computeOutputValues :: [Coin] -> [Coin]
    computeOutputValues = inputsToOutputsWithFee fee numOfOutputs
      where numOfOutputs = 2

    computeUTxO = mkUTxOVariant txEnvNetworkId signingKey


generateTxM ::
      TxEnvironment ConwayEra
  ->  Generator (Either TxGenError (Tx ConwayEra))
generateTxM txEnv
  = do
      inFunds <- get
      case generateTxPure txEnv inFunds of
        Right (tx, outFunds)  -> put outFunds >> pure (Right tx)
        Left err              -> pure (Left err)

generateTxPure ::
     TxEnvironment ConwayEra
  -> FundQueue
  -> Either TxGenError (Tx ConwayEra, FundQueue)
generateTxPure TxEnvironment{..} inQueue
  = do
      (tx, txId) <- generator inputs outputs
      let outQueue = List.foldl' FundQueue.insertFund FundQueue.emptyFundQueue (toFunds txId)
      pure (tx, outQueue)
  where
    inputs = FundQueue.toList inQueue
    TxFeeExplicit _ fee = txEnvFee

    generator :: TxGenerator ConwayEra
    generator =
        case Api.convertToLedgerProtocolParameters shelleyBasedEra txEnvProtocolParams of
          Right ledgerParameters ->
            localGenTx ShelleyBasedEraConway ledgerParameters collateralFunds txEnvFee txEnvMetadata
          Left err -> \_ _ -> Left (ApiError err)
      where
        -- collateralFunds are needed for Plutus transactions
        collateralFunds :: (TxInsCollateral ConwayEra, [Fund])
        collateralFunds = (TxInsCollateralNone, [])

    outValues = computeOutputValues $ map FundQueue.getFundCoin inputs
    (outputs, toFunds) = makeToUTxOList (repeat computeUTxO) outValues

    computeOutputValues :: [Coin] -> [Coin]
    computeOutputValues = inputsToOutputsWithFee fee numOfOutputs
      where numOfOutputs = 2

    computeUTxO = mkUTxOVariant txEnvNetworkId signingKey


runTransactionCmds :: Cmd.TransactionCmds era -> ExceptT TxCmdError IO ()
runTransactionCmds = \case
  Cmd.TransactionBuildCmd args -> runTransactionBuildCmd args
  Cmd.TransactionBuildEstimateCmd args -> runTransactionBuildEstimateCmd args
  Cmd.TransactionBuildRawCmd args -> runTransactionBuildRawCmd args
  Cmd.TransactionSignCmd args -> runTransactionSignCmd args
  Cmd.TransactionSubmitCmd args -> runTransactionSubmitCmd args
  Cmd.TransactionCalculateMinFeeCmd args -> runTransactionCalculateMinFeeCmd args
  Cmd.TransactionCalculateMinValueCmd args -> runTransactionCalculateMinValueCmd args
  Cmd.TransactionHashScriptDataCmd args -> runTransactionHashScriptDataCmd args
  Cmd.TransactionTxIdCmd args -> runTransactionTxIdCmd args
  Cmd.TransactionViewCmd args -> runTransactionViewCmd args
  Cmd.TransactionPolicyIdCmd args -> runTransactionPolicyIdCmd args
  Cmd.TransactionWitnessCmd args -> runTransactionWitnessCmd args
  Cmd.TransactionSignWitnessCmd args -> runTransactionSignWitnessCmd args

-- ----------------------------------------------------------------------------
-- Building transactions
--

runTransactionBuildCmd
  :: ()
  => Cmd.TransactionBuildCmdArgs era
  -> ExceptT TxCmdError IO ()
runTransactionBuildCmd
  Cmd.TransactionBuildCmdArgs
    { eon
    , nodeSocketPath
    , consensusModeParams
    , networkId = networkId
    , mScriptValidity = mScriptValidity
    , mOverrideWitnesses = mOverrideWitnesses
    , txins
    , readOnlyReferenceInputs
    , requiredSigners = reqSigners
    , txinsc
    , mReturnCollateral = mReturnColl
    , mTotalCollateral
    , txouts
    , changeAddresses
    , mValue
    , mValidityLowerBound
    , mValidityUpperBound
    , certificates
    , withdrawals
    , metadataSchema
    , scriptFiles
    , metadataFiles
    , mUpdateProposalFile
    , voteFiles
    , proposalFiles
    , treasuryDonation -- Maybe TxTreasuryDonation
    , buildOutputOptions
    } = shelleyBasedEraConstraints eon $ do
    let era = toCardanoEra eon

    -- The user can specify an era prior to the era that the node is currently in.
    -- We cannot use the user specified era to construct a query against a node because it may differ
    -- from the node's era and this will result in the 'QueryEraMismatch' failure.

    let localNodeConnInfo =
          LocalNodeConnectInfo
            { localConsensusModeParams = consensusModeParams
            , localNodeNetworkId = networkId
            , localNodeSocketPath = nodeSocketPath
            }

    inputsAndMaybeScriptWits <- firstExceptT TxCmdScriptWitnessError $ CLI.readScriptWitnessFiles eon txins
    certFilesAndMaybeScriptWits <-
      firstExceptT TxCmdScriptWitnessError $ CLI.readScriptWitnessFiles eon certificates

    -- TODO: Conway Era - How can we make this more composable?
    certsAndMaybeScriptWits <-
      sequence
        [ fmap
            (,mSwit)
            ( firstExceptT TxCmdReadTextViewFileError . newExceptT $
                readFileTextEnvelope AsCertificate (File certFile)
            )
        | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
        ]
    withdrawalsAndMaybeScriptWits <-
      firstExceptT TxCmdScriptWitnessError $
        CLI.readScriptWitnessFilesTuple eon withdrawals
    txMetadata <-
      firstExceptT TxCmdMetadataError . newExceptT $
        CLI.readTxMetadata eon metadataSchema metadataFiles
    valuesWithScriptWits <- readValueScriptWitnesses eon $ Maybe.fromMaybe mempty mValue
    scripts <-
      firstExceptT TxCmdScriptFileError $
        mapM (CLI.readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      hoistEither $ first TxCmdAuxScriptsValidationError $ CLI.validateTxAuxScripts eon scripts

    mProp <- case mUpdateProposalFile of
      Just (Featured w (Just updateProposalFile)) ->
        CLI.readTxUpdateProposal w updateProposalFile & firstExceptT TxCmdReadTextViewFileError
      _ -> pure TxUpdateProposalNone

    requiredSigners <-
      mapM (firstExceptT TxCmdRequiredSignerError . newExceptT . CLI.readRequiredSigner) reqSigners
    mReturnCollateral <- Monad.forM mReturnColl $ toTxOutInShelleyBasedEra eon

    txOuts <- mapM (toTxOutInAnyEra eon) txouts

    -- Conway related
    votingProceduresAndMaybeScriptWits <-
      inEonForEra
        (pure mempty)
        (\w -> firstExceptT TxCmdVoteError $ ExceptT (CLI.readVotingProceduresFiles w voteFiles))
        era

    proposals <-
      newExceptT $
        first TxCmdProposalError
          <$> CLI.readTxGovernanceActions eon proposalFiles

    -- the same collateral input can be used for several plutus scripts
    let filteredTxinsc = Set.toList $ Set.fromList txinsc

    let allReferenceInputs =
          getAllReferenceInputs
            inputsAndMaybeScriptWits
            (snd valuesWithScriptWits)
            certsAndMaybeScriptWits
            withdrawalsAndMaybeScriptWits
            votingProceduresAndMaybeScriptWits
            proposals
            readOnlyReferenceInputs

    let inputsThatRequireWitnessing = [input | (input, _) <- inputsAndMaybeScriptWits]
        allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ filteredTxinsc

    AnyCardanoEra nodeEra <-
      lift (executeLocalStateQueryExpr localNodeConnInfo Consensus.VolatileTip queryCurrentEra)
        & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
        & onLeft (left . TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

    (txEraUtxo, _, eraHistory, systemStart, _, _, _, featuredCurrentTreasuryValueM) <-
      lift
        ( executeLocalStateQueryExpr
            localNodeConnInfo
            Consensus.VolatileTip
            (queryStateForBalancedTx nodeEra allTxInputs [])
        )
        & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
        & onLeft (left . TxCmdQueryConvenienceError)

    let currentTreasuryValueAndDonation =
          case (treasuryDonation, unFeatured <$>
                   featuredCurrentTreasuryValueM) of
            -- We shouldn't specify the treasury value when no donation
            -- is being done
            (Nothing, _) -> Nothing
            -- TODO: Current treasury value couldn't be obtained but
            -- is required: we should fail suggesting that the node's
            -- version is too old
            (Just _td, Nothing) -> Nothing
            (Just td, Just ctv) -> Just (ctv, td)

    -- We need to construct the txBodycontent outside of runTxBuild
    BalancedTxBody txBodyContent balancedTxBody _ _ <-
      runTxBuild
        eon
        nodeSocketPath
        networkId
        mScriptValidity
        inputsAndMaybeScriptWits
        readOnlyReferenceInputs
        filteredTxinsc
        mReturnCollateral
        mTotalCollateral
        txOuts
        changeAddresses
        valuesWithScriptWits
        mValidityLowerBound
        mValidityUpperBound
        certsAndMaybeScriptWits
        withdrawalsAndMaybeScriptWits
        requiredSigners
        txAuxScripts
        txMetadata
        mProp
        mOverrideWitnesses
        votingProceduresAndMaybeScriptWits
        proposals
        currentTreasuryValueAndDonation

    -- TODO: Calculating the script cost should live as a different command.
    -- Why? Because then we can simply read a txbody and figure out
    -- the script cost vs having to build the tx body each time
    case buildOutputOptions of
      OutputScriptCostOnly fp -> do
        let BuildTxWith mTxProtocolParams = txProtocolParams txBodyContent

        pparams <- pure mTxProtocolParams & onNothing (left TxCmdProtocolParametersNotPresentInTxBody)
        executionUnitPrices <-
          pure (getExecutionUnitPrices era pparams) & onNothing (left TxCmdPParamExecutionUnitsNotAvailable)

        Refl <-
          testEquality era nodeEra
            & hoistMaybe (TxCmdTxNodeEraMismatchError $ NodeEraMismatchError era nodeEra)

        scriptExecUnitsMap <-
          firstExceptT (TxCmdTxExecUnitsErr . AnyTxCmdTxExecUnitsErr) $
            hoistEither $
              evaluateTransactionExecutionUnits
                era
                systemStart
                (toLedgerEpochInfo eraHistory)
                pparams
                txEraUtxo
                balancedTxBody

        let mScriptWits = forEraInEon era [] $ \sbe -> collectTxBodyScriptWitnesses sbe txBodyContent

        scriptCostOutput <-
          firstExceptT TxCmdPlutusScriptCostErr $
            hoistEither $
              CLI.renderScriptCosts
                txEraUtxo
                executionUnitPrices
                mScriptWits
                scriptExecUnitsMap
        liftIO $ LBS8.writeFile (unFile fp) $ Aeson.encodePretty scriptCostOutput
      OutputTxBodyOnly fpath ->
        let noWitTx = makeSignedTransaction [] balancedTxBody
         in lift (cardanoEraConstraints era $ writeTxFileTextEnvelopeCddl eon fpath noWitTx)
              & onLeft (left . TxCmdWriteFileError)

runTransactionBuildEstimateCmd
  :: ()
  => Cmd.TransactionBuildEstimateCmdArgs era
  -> ExceptT TxCmdError IO ()
runTransactionBuildEstimateCmd -- TODO change type
  Cmd.TransactionBuildEstimateCmdArgs
    { eon
    , mScriptValidity
    , shelleyWitnesses
    , mByronWitnesses
    , protocolParamsFile
    , totalUTxOValue
    , txins
    , readOnlyReferenceInputs = readOnlyRefIns
    , requiredSigners = reqSigners
    , txinsc = txInsCollateral
    , mReturnCollateral = mReturnColl
    , txouts
    , changeAddress = TxOutChangeAddress changeAddr
    , mValue
    , mValidityLowerBound
    , mValidityUpperBound
    , certificates
    , withdrawals
    , metadataSchema
    , scriptFiles
    , metadataFiles
    , mUpdateProposalFile
    , voteFiles
    , proposalFiles
    , plutusCollateral
    , totalReferenceScriptSize
    -- The new field name:
    -- , currentTreasuryValueAndDonation
    , currentTreasuryValue
    , treasuryDonation
    , txBodyOutFile
    } = do
    let sbe = maryEraOnwardsToShelleyBasedEra eon
    ledgerPParams <-
      firstExceptT TxCmdProtocolParamsError $ CLI.readProtocolParameters sbe protocolParamsFile
    inputsAndMaybeScriptWits <-
      firstExceptT TxCmdScriptWitnessError $
        CLI.readScriptWitnessFiles sbe txins
    certFilesAndMaybeScriptWits <-
      firstExceptT TxCmdScriptWitnessError $
        CLI.readScriptWitnessFiles sbe certificates

    withdrawalsAndMaybeScriptWits <-
      firstExceptT TxCmdScriptWitnessError $
        CLI.readScriptWitnessFilesTuple sbe withdrawals
    txMetadata <-
      firstExceptT TxCmdMetadataError
        . newExceptT
        $ CLI.readTxMetadata sbe metadataSchema metadataFiles
    valuesWithScriptWits <- readValueScriptWitnesses sbe $ Maybe.fromMaybe mempty mValue
    scripts <-
      firstExceptT TxCmdScriptFileError $
        mapM (CLI.readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      hoistEither $ first TxCmdAuxScriptsValidationError $ CLI.validateTxAuxScripts sbe scripts

    txUpdateProposal <- case mUpdateProposalFile of
      Just (Featured w (Just updateProposalFile)) ->
        CLI.readTxUpdateProposal w updateProposalFile & firstExceptT TxCmdReadTextViewFileError
      _ -> pure TxUpdateProposalNone

    requiredSigners <-
      mapM (firstExceptT TxCmdRequiredSignerError . newExceptT . CLI.readRequiredSigner) reqSigners

    mReturnCollateral <- mapM (toTxOutInShelleyBasedEra sbe) mReturnColl

    txOuts <- mapM (toTxOutInAnyEra sbe) txouts

    -- the same collateral input can be used for several plutus scripts
    let filteredTxinsc = Set.toList $ Set.fromList txInsCollateral

    -- Conway related
    votingProceduresAndMaybeScriptWits <-
      inEonForShelleyBasedEra
        (pure mempty)
        ( \w ->
            firstExceptT TxCmdVoteError . ExceptT $
              conwayEraOnwardsConstraints w $
                CLI.readVotingProceduresFiles w voteFiles
        )
        sbe

    proposals <-
      lift (CLI.readTxGovernanceActions sbe proposalFiles)
        & onLeft (left . TxCmdProposalError)

    certsAndMaybeScriptWits <-
      shelleyBasedEraConstraints sbe $
        sequence
          [ fmap
              (,mSwit)
              ( firstExceptT TxCmdReadTextViewFileError . newExceptT $
                  readFileTextEnvelope AsCertificate (File certFile)
              )
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]

    let currentTreasuryValueAndDonation ::
             Maybe (TxCurrentTreasuryValue, TxTreasuryDonation)
        currentTreasuryValueAndDonation = do
          value    <- currentTreasuryValue
          donation <- treasuryDonation
          pure (value, donation)
    txBodyContent <-
      hoistEither $
        constructTxBodyContent
          sbe
          mScriptValidity
          (Just ledgerPParams)
          inputsAndMaybeScriptWits
          readOnlyRefIns
          filteredTxinsc
          mReturnCollateral
          Nothing -- TODO: Remove total collateral parameter from estimateBalancedTxBody
          txOuts
          mValidityLowerBound
          mValidityUpperBound
          valuesWithScriptWits
          certsAndMaybeScriptWits
          withdrawalsAndMaybeScriptWits
          requiredSigners
          0
          txAuxScripts
          txMetadata
          txUpdateProposal
          votingProceduresAndMaybeScriptWits
          proposals
          currentTreasuryValueAndDonation
    let stakeCredentialsToDeregisterMap = Map.fromList $ Maybe.catMaybes [getStakeDeregistrationInfo cert | (cert, _) <- certsAndMaybeScriptWits]
        drepsToDeregisterMap = Map.fromList $ Maybe.catMaybes [getDRepDeregistrationInfo cert | (cert, _) <- certsAndMaybeScriptWits]
        poolsToDeregister = Set.fromList $ Maybe.catMaybes [getPoolDeregistrationInfo cert | (cert, _) <- certsAndMaybeScriptWits]
        totCol = Maybe.fromMaybe 0 plutusCollateral
        pScriptExecUnits =
          Map.fromList
            [ (sWitIndex, execUnits)
            | (sWitIndex, AnyScriptWitness (PlutusScriptWitness _ _ _ _ _ execUnits)) <-
                collectTxBodyScriptWitnesses sbe txBodyContent
            ]

    BalancedTxBody _ balancedTxBody _ _ <-
      hoistEither $
        first TxCmdFeeEstimationError $
          estimateBalancedTxBody
            eon
            txBodyContent
            ledgerPParams
            poolsToDeregister
            stakeCredentialsToDeregisterMap
            drepsToDeregisterMap
            pScriptExecUnits
            totCol
            shelleyWitnesses
            (Maybe.fromMaybe 0 mByronWitnesses)
            (maybe 0 unReferenceScriptSize totalReferenceScriptSize)
            (anyAddressInShelleyBasedEra sbe changeAddr)
            totalUTxOValue

    let noWitTx = makeSignedTransaction [] balancedTxBody
    lift (writeTxFileTextEnvelopeCddl sbe txBodyOutFile noWitTx)
      & onLeft (left . TxCmdWriteFileError)

getPoolDeregistrationInfo
  :: Certificate era
  -> Maybe PoolId
getPoolDeregistrationInfo (ShelleyRelatedCertificate w cert) =
  shelleyToBabbageEraConstraints w $ getShelleyDeregistrationPoolId cert
getPoolDeregistrationInfo (ConwayCertificate w cert) =
  conwayEraOnwardsConstraints w $ getConwayDeregistrationPoolId cert

getShelleyDeregistrationPoolId
  :: Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Ledger.ShelleyEraTxCert (ShelleyLedgerEra era)
  => Ledger.TxCert (ShelleyLedgerEra era) ~ Ledger.ShelleyTxCert (ShelleyLedgerEra era)
  => Ledger.ShelleyTxCert (ShelleyLedgerEra era)
  -> Maybe PoolId
getShelleyDeregistrationPoolId cert = do
  case cert of
    Ledger.RetirePoolTxCert poolId _ -> Just (StakePoolKeyHash poolId)
    _ -> Nothing

getConwayDeregistrationPoolId
  :: Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Ledger.TxCert (ShelleyLedgerEra era) ~ Ledger.ConwayTxCert (ShelleyLedgerEra era)
  => Ledger.ConwayEraTxCert (ShelleyLedgerEra era)
  => Ledger.ConwayTxCert (ShelleyLedgerEra era)
  -> Maybe PoolId
getConwayDeregistrationPoolId cert = do
  case cert of
    Ledger.RetirePoolTxCert poolId _ -> Just (StakePoolKeyHash poolId)
    _ -> Nothing

getDRepDeregistrationInfo
  :: Certificate era
  -> Maybe (Ledger.Credential Ledger.DRepRole StandardCrypto, Coin)
getDRepDeregistrationInfo ShelleyRelatedCertificate{} = Nothing
getDRepDeregistrationInfo (ConwayCertificate w cert) =
  conwayEraOnwardsConstraints w $ getConwayDRepDeregistrationInfo cert

getConwayDRepDeregistrationInfo
  :: Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Ledger.TxCert (ShelleyLedgerEra era) ~ Ledger.ConwayTxCert (ShelleyLedgerEra era)
  => Ledger.ConwayEraTxCert (ShelleyLedgerEra era)
  => Ledger.ConwayTxCert (ShelleyLedgerEra era)
  -> Maybe (Ledger.Credential Ledger.DRepRole StandardCrypto, Coin)
getConwayDRepDeregistrationInfo = Ledger.getUnRegDRepTxCert

getStakeDeregistrationInfo
  :: Certificate era
  -> Maybe (StakeCredential, Coin)
getStakeDeregistrationInfo (ShelleyRelatedCertificate w cert) =
  shelleyToBabbageEraConstraints w $ getShelleyDeregistrationInfo cert
getStakeDeregistrationInfo (ConwayCertificate w cert) =
  conwayEraOnwardsConstraints w $ getConwayDeregistrationInfo cert

-- There for no deposits required pre-conway for registering stake
-- credentials.
getShelleyDeregistrationInfo
  :: Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Ledger.ShelleyEraTxCert (ShelleyLedgerEra era)
  => Ledger.TxCert (ShelleyLedgerEra era) ~ Ledger.ShelleyTxCert (ShelleyLedgerEra era)
  => Ledger.ShelleyTxCert (ShelleyLedgerEra era)
  -> Maybe (StakeCredential, Coin)
getShelleyDeregistrationInfo cert = do
  case cert of
    Ledger.UnRegTxCert stakeCred -> Just (Api.fromShelleyStakeCredential stakeCred, 0)
    _ -> Nothing

getConwayDeregistrationInfo
  :: Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Ledger.TxCert (ShelleyLedgerEra era) ~ Ledger.ConwayTxCert (ShelleyLedgerEra era)
  => Ledger.ConwayEraTxCert (ShelleyLedgerEra era)
  => Ledger.ConwayTxCert (ShelleyLedgerEra era)
  -> Maybe (StakeCredential, Coin)
getConwayDeregistrationInfo cert = do
  case cert of
    Ledger.UnRegDepositTxCert stakeCred depositRefund -> Just (Api.fromShelleyStakeCredential stakeCred, depositRefund)
    _ -> Nothing

getExecutionUnitPrices :: CardanoEra era -> LedgerProtocolParameters era -> Maybe Ledger.Prices
getExecutionUnitPrices cEra (LedgerProtocolParameters pp) =
  forEraInEonMaybe cEra $ \aeo ->
    alonzoEraOnwardsConstraints aeo $
      pp ^. Ledger.ppPricesL

runTransactionBuildRawCmd
  :: ()
  => Cmd.TransactionBuildRawCmdArgs era
  -> ExceptT TxCmdError IO ()
runTransactionBuildRawCmd
  Cmd.TransactionBuildRawCmdArgs
    { eon
    , mScriptValidity
    , txIns
    , readOnlyRefIns
    , txInsCollateral
    , mReturnCollateral = mReturnColl
    , mTotalCollateral
    , requiredSigners = reqSigners
    , txouts
    , mValue
    , mValidityLowerBound
    , mValidityUpperBound
    , fee
    , certificates
    , withdrawals
    , metadataSchema
    , scriptFiles
    , metadataFiles
    , mProtocolParamsFile
    , mUpdateProprosalFile
    , voteFiles
    , proposalFiles
    -- , currentTreasuryValueAndDonation
    , currentTreasuryValue
    , treasuryDonation
    , txBodyOutFile
    } = do
    inputsAndMaybeScriptWits <-
      firstExceptT TxCmdScriptWitnessError $
        CLI.readScriptWitnessFiles eon txIns
    certFilesAndMaybeScriptWits <-
      firstExceptT TxCmdScriptWitnessError $
        CLI.readScriptWitnessFiles eon certificates

    withdrawalsAndMaybeScriptWits <-
      firstExceptT TxCmdScriptWitnessError $
        CLI.readScriptWitnessFilesTuple eon withdrawals
    txMetadata <-
      firstExceptT TxCmdMetadataError
        . newExceptT
        $ CLI.readTxMetadata eon metadataSchema metadataFiles
    valuesWithScriptWits <- readValueScriptWitnesses eon $ Maybe.fromMaybe mempty mValue
    scripts <-
      firstExceptT TxCmdScriptFileError $
        mapM (CLI.readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      hoistEither $ first TxCmdAuxScriptsValidationError $ CLI.validateTxAuxScripts eon scripts

    pparams <- Monad.forM mProtocolParamsFile $ \ppf ->
      firstExceptT TxCmdProtocolParamsError (CLI.readProtocolParameters eon ppf)

    let mLedgerPParams = LedgerProtocolParameters <$> pparams

    txUpdateProposal <- case mUpdateProprosalFile of
      Just (Featured w (Just updateProposalFile)) ->
        CLI.readTxUpdateProposal w updateProposalFile & firstExceptT TxCmdReadTextViewFileError
      _ -> pure TxUpdateProposalNone

    requiredSigners <-
      mapM (firstExceptT TxCmdRequiredSignerError . newExceptT . CLI.readRequiredSigner) reqSigners

    mReturnCollateral <- mapM (toTxOutInShelleyBasedEra eon) mReturnColl

    txOuts <- mapM (toTxOutInAnyEra eon) txouts

    -- the same collateral input can be used for several plutus scripts
    let filteredTxinsc = Set.toList $ Set.fromList txInsCollateral

    -- Conway related
    votingProceduresAndMaybeScriptWits <-
      inEonForShelleyBasedEra
        (pure mempty)
        ( \w ->
            firstExceptT TxCmdVoteError . ExceptT $
              conwayEraOnwardsConstraints w $
                CLI.readVotingProceduresFiles w voteFiles
        )
        eon

    proposals <-
      lift (CLI.readTxGovernanceActions eon proposalFiles)
        & onLeft (left . TxCmdProposalError)

    certsAndMaybeScriptWits <-
      shelleyBasedEraConstraints eon $
        sequence
          [ fmap
              (,mSwit)
              ( firstExceptT TxCmdReadTextViewFileError . newExceptT $
                  readFileTextEnvelope AsCertificate (File certFile)
              )
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]
    let currentTreasuryValueAndDonation ::
             Maybe (TxCurrentTreasuryValue, TxTreasuryDonation)
        currentTreasuryValueAndDonation = do
          value    <- currentTreasuryValue
          donation <- treasuryDonation
          pure (value, donation)
    txBody <-
      hoistEither $
        runTxBuildRaw
          eon
          mScriptValidity
          inputsAndMaybeScriptWits
          readOnlyRefIns
          filteredTxinsc
          mReturnCollateral
          mTotalCollateral
          txOuts
          mValidityLowerBound
          mValidityUpperBound
          fee
          valuesWithScriptWits
          certsAndMaybeScriptWits
          withdrawalsAndMaybeScriptWits
          requiredSigners
          txAuxScripts
          txMetadata
          mLedgerPParams
          txUpdateProposal
          votingProceduresAndMaybeScriptWits
          proposals
          currentTreasuryValueAndDonation

    let noWitTx = makeSignedTransaction [] txBody
    lift (writeTxFileTextEnvelopeCddl eon txBodyOutFile noWitTx)
      & onLeft (left . TxCmdWriteFileError)

runTxBuildRaw
  :: ()
  => ShelleyBasedEra era
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ Read only reference inputs
  -> [TxIn]
  -- ^ TxIn for collateral
  -> Maybe (TxOut CtxTx era)
  -- ^ Return collateral
  -> Maybe Coin
  -- ^ Total collateral
  -> [TxOut CtxTx era]
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> TxValidityUpperBound era
  -- ^ Tx upper bound
  -> Coin
  -- ^ Tx fee
  -> (Value, [ScriptWitness WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Coin, Maybe (ScriptWitness WitCtxStake era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> Maybe (LedgerProtocolParameters era)
  -> TxUpdateProposal era
  -> [(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))]
  -> [(Proposal era, Maybe (ScriptWitness WitCtxStake era))]
  -> Maybe (TxCurrentTreasuryValue, TxTreasuryDonation)
  -> Either TxCmdError (TxBody era)
runTxBuildRaw
  sbe
  mScriptValidity
  inputsAndMaybeScriptWits
  readOnlyRefIns
  txinsc
  mReturnCollateral
  mTotCollateral
  txouts
  mLowerBound
  mUpperBound
  fee
  valuesWithScriptWits
  certsAndMaybeSriptWits
  withdrawals
  reqSigners
  txAuxScripts
  txMetadata
  mpparams
  txUpdateProposal
  votingProcedures
  proposals
  mCurrentTreasuryValueAndDonation = do
    txBodyContent <-
      constructTxBodyContent
        sbe
        mScriptValidity
        (unLedgerProtocolParameters <$> mpparams)
        inputsAndMaybeScriptWits
        readOnlyRefIns
        txinsc
        mReturnCollateral
        mTotCollateral
        txouts
        mLowerBound
        mUpperBound
        valuesWithScriptWits
        certsAndMaybeSriptWits
        withdrawals
        reqSigners
        fee
        txAuxScripts
        txMetadata
        txUpdateProposal
        votingProcedures
        proposals
        mCurrentTreasuryValueAndDonation

    first TxCmdTxBodyError $ createAndValidateTransactionBody sbe txBodyContent

constructTxBodyContent
  :: ShelleyBasedEra era
  -> Maybe ScriptValidity
  -> Maybe (Ledger.PParams (ShelleyLedgerEra era))
  -> [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ Read only reference inputs
  -> [TxIn]
  -- ^ TxIn for collateral
  -> Maybe (TxOut CtxTx era)
  -- ^ Return collateral
  -> Maybe Coin
  -- ^ Total collateral
  -> [TxOut CtxTx era]
  -- ^ Normal outputs
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> TxValidityUpperBound era
  -- ^ Tx upper bound
  -> (Value, [ScriptWitness WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Coin, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Withdrawals
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> Coin
  -- ^ Tx fee
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> TxUpdateProposal era
  -> [(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))]
  -> [(Proposal era, Maybe (ScriptWitness WitCtxStake era))]
  -> Maybe (TxCurrentTreasuryValue, TxTreasuryDonation)
  -- ^ The current treasury value and the donation. This is a stop gap as the
  -- semantics of the donation and treasury value depend on the script languages
  -- being used.
  -> Either TxCmdError (TxBodyContent BuildTx era)
constructTxBodyContent
  sbe
  mScriptValidity
  mPparams
  inputsAndMaybeScriptWits
  readOnlyRefIns
  txinsc
  mReturnCollateral
  mTotCollateral
  txouts
  mLowerBound
  mUpperBound
  valuesWithScriptWits
  certsAndMaybeScriptWits
  withdrawals
  reqSigners
  fee
  txAuxScripts
  txMetadata
  txUpdateProposal
  votingProcedures
  proposals
  mCurrentTreasuryValueAndDonation =
    do
      let allReferenceInputs =
            getAllReferenceInputs
              inputsAndMaybeScriptWits
              (snd valuesWithScriptWits)
              certsAndMaybeScriptWits
              withdrawals
              votingProcedures
              proposals
              readOnlyRefIns

      validatedCollateralTxIns <- validateTxInsCollateral sbe txinsc
      validatedRefInputs <- validateTxInsReference sbe allReferenceInputs
      validatedTotCollateral <-
        first TxCmdNotSupportedInEraValidationError $ CLI.validateTxTotalCollateral sbe mTotCollateral
      validatedRetCol <-
        first TxCmdNotSupportedInEraValidationError $ CLI.validateTxReturnCollateral sbe mReturnCollateral
      let txFee = TxFeeExplicit sbe fee
      validatedLowerBound <-
        first TxCmdNotSupportedInEraValidationError $ CLI.validateTxValidityLowerBound sbe mLowerBound
      validatedReqSigners <-
        first TxCmdNotSupportedInEraValidationError $ CLI.validateRequiredSigners sbe reqSigners
      validatedMintValue <- createTxMintValue sbe valuesWithScriptWits
      validatedTxScriptValidity <-
        first TxCmdNotSupportedInEraValidationError $ CLI.validateTxScriptValidity sbe mScriptValidity
      validatedVotingProcedures <-
        first TxCmdTxGovDuplicateVotes $ CLI.convertToTxVotingProcedures votingProcedures
      validatedCurrentTreasuryValue <-
        first
          TxCmdNotSupportedInEraValidationError
          (CLI.validateTxCurrentTreasuryValue sbe (fst <$> mCurrentTreasuryValueAndDonation))
      validatedTreasuryDonation <-
        first
          TxCmdNotSupportedInEraValidationError
          (CLI.validateTxTreasuryDonation sbe (snd <$> mCurrentTreasuryValueAndDonation))
      return $
        shelleyBasedEraConstraints sbe $
          ( defaultTxBodyContent sbe
              & setTxIns (validateTxIns inputsAndMaybeScriptWits)
              & setTxInsCollateral validatedCollateralTxIns
              & setTxInsReference validatedRefInputs
              & setTxOuts txouts
              & setTxTotalCollateral validatedTotCollateral
              & setTxReturnCollateral validatedRetCol
              & setTxFee txFee
              & setTxValidityLowerBound validatedLowerBound
              & setTxValidityUpperBound mUpperBound
              & setTxMetadata txMetadata
              & setTxAuxScripts txAuxScripts
              & setTxExtraKeyWits validatedReqSigners
              & setTxProtocolParams (BuildTxWith $ LedgerProtocolParameters <$> mPparams)
              & setTxWithdrawals (TxWithdrawals sbe $ map convertWithdrawals withdrawals)
              & setTxCertificates (convertCertificates sbe certsAndMaybeScriptWits)
              & setTxUpdateProposal txUpdateProposal
              & setTxMintValue validatedMintValue
              & setTxScriptValidity validatedTxScriptValidity
          )
            { -- TODO: Create set* function for proposal procedures and voting procedures
              txProposalProcedures =
                forShelleyBasedEraInEonMaybe sbe (`Featured` CLI.convToTxProposalProcedures proposals)
            , txVotingProcedures = forShelleyBasedEraInEonMaybe sbe (`Featured` validatedVotingProcedures)
            }
            & setTxCurrentTreasuryValue validatedCurrentTreasuryValue
            & setTxTreasuryDonation validatedTreasuryDonation
   where
    convertWithdrawals
      :: (StakeAddress, Coin, Maybe (ScriptWitness WitCtxStake era))
      -> (StakeAddress, Coin, BuildTxWith BuildTx (Witness WitCtxStake era))
    convertWithdrawals (sAddr, ll, mScriptWitnessFiles) =
      case mScriptWitnessFiles of
        Just sWit -> (sAddr, ll, BuildTxWith $ ScriptWitness ScriptWitnessForStakeAddr sWit)
        Nothing -> (sAddr, ll, BuildTxWith $ KeyWitness KeyWitnessForStakeAddr)

runTxBuild
  :: ()
  => ShelleyBasedEra era
  -> SocketPath
  -> NetworkId
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -- ^ Read only reference inputs
  -> [TxIn]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ TxIn for collateral
  -> Maybe (TxOut CtxTx era)
  -- ^ Return collateral
  -> Maybe Coin
  -- ^ Total collateral
  -> [TxOut CtxTx era]
  -- ^ Normal outputs
  -> TxOutChangeAddress
  -- ^ A change output
  -> (Value, [ScriptWitness WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> TxValidityUpperBound era
  -- ^ Tx upper bound
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Coin, Maybe (ScriptWitness WitCtxStake era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> TxUpdateProposal era
  -> Maybe Word
  -> [(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))]
  -> [(Proposal era, Maybe (ScriptWitness WitCtxStake era))]
  -> Maybe (TxCurrentTreasuryValue, TxTreasuryDonation)
  -- ^ The current treasury value and the donation.
  -> ExceptT TxCmdError IO (BalancedTxBody era)
runTxBuild
  sbe
  socketPath
  networkId
  mScriptValidity
  inputsAndMaybeScriptWits
  readOnlyRefIns
  txinsc
  mReturnCollateral
  mTotCollateral
  txouts
  (TxOutChangeAddress changeAddr)
  valuesWithScriptWits
  mLowerBound
  mUpperBound
  certsAndMaybeScriptWits
  withdrawals
  reqSigners
  txAuxScripts
  txMetadata
  txUpdateProposal
  mOverrideWits
  votingProcedures
  proposals
  mCurrentTreasuryValueAndDonation =
    shelleyBasedEraConstraints sbe $ do
      -- TODO: All functions should be parameterized by ShelleyBasedEra
      -- as it's not possible to call this function with ByronEra
      let era = toCardanoEra sbe
          inputsThatRequireWitnessing = [input | (input, _) <- inputsAndMaybeScriptWits]

      let allReferenceInputs =
            getAllReferenceInputs
              inputsAndMaybeScriptWits
              (snd valuesWithScriptWits)
              certsAndMaybeScriptWits
              withdrawals
              votingProcedures
              proposals
              readOnlyRefIns

      let allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ txinsc
          localNodeConnInfo =
            LocalNodeConnectInfo
              { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
              , localNodeNetworkId = networkId
              , localNodeSocketPath = socketPath
              }

      AnyCardanoEra nodeEra <-
        lift (executeLocalStateQueryExpr localNodeConnInfo Consensus.VolatileTip queryCurrentEra)
          & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
          & onLeft (left . TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

      Refl <-
        testEquality era nodeEra
          & hoistMaybe (TxCmdTxNodeEraMismatchError $ NodeEraMismatchError era nodeEra)

      let certs =
            case convertCertificates sbe certsAndMaybeScriptWits of
              TxCertificates _ cs _ -> cs
              _ -> []

-- use readProtocolParametersOrDie from ApiTest.hs
-- also protocolParamMaxBlockExUnits and protocolParamMaxTxExUnits
      (txEraUtxo, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits, drepDelegDeposits, _) <-
        lift
          ( executeLocalStateQueryExpr localNodeConnInfo Consensus.VolatileTip $
              queryStateForBalancedTx nodeEra allTxInputs certs
          )
          & onLeft (left . TxCmdQueryConvenienceError . AcqFailure)
          & onLeft (left . TxCmdQueryConvenienceError)

      txBodyContent <-
        hoistEither $
          constructTxBodyContent
            sbe
            mScriptValidity
            (Just $ unLedgerProtocolParameters pparams)
            inputsAndMaybeScriptWits
            readOnlyRefIns
            txinsc
            mReturnCollateral
            mTotCollateral
            txouts
            mLowerBound
            mUpperBound
            valuesWithScriptWits
            certsAndMaybeScriptWits
            withdrawals
            reqSigners
            0
            txAuxScripts
            txMetadata
            txUpdateProposal
            votingProcedures
            proposals
            mCurrentTreasuryValueAndDonation

      firstExceptT TxCmdTxInsDoNotExist
        . hoistEither
        $ txInsExistInUTxO allTxInputs txEraUtxo
      firstExceptT TxCmdQueryNotScriptLocked
        . hoistEither
        $ notScriptLockedTxIns txinsc txEraUtxo

      cAddr <-
        pure (anyAddressInEra era changeAddr)
          & onLeft (error $ "runTxBuild: Byron address used: " <> show changeAddr) -- should this throw instead?
      balancedTxBody@(BalancedTxBody _ _ _ fee) <-
        firstExceptT (TxCmdBalanceTxBody . AnyTxBodyErrorAutoBalance)
          . hoistEither
          $ makeTransactionBodyAutoBalance
            sbe
            systemStart
            (toLedgerEpochInfo eraHistory)
            pparams
            stakePools
            stakeDelegDeposits
            drepDelegDeposits
            txEraUtxo
            txBodyContent
            cAddr
            mOverrideWits

      liftIO $ putStrLn $ "Estimated transaction fee: " <> (show fee :: String)

      return balancedTxBody

convertCertificates
  :: ()
  => ShelleyBasedEra era
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -> TxCertificates BuildTx era
convertCertificates sbe certsAndScriptWitnesses =
  TxCertificates sbe certs $ BuildTxWith reqWits
 where
  certs = map fst certsAndScriptWitnesses
  reqWits = Map.fromList $ Maybe.mapMaybe convert certsAndScriptWitnesses
  convert
    :: (Certificate era, Maybe (ScriptWitness WitCtxStake era))
    -> Maybe (StakeCredential, Witness WitCtxStake era)
  convert (cert, mScriptWitnessFiles) = do
    sCred <- selectStakeCredentialWitness cert
    Just $ case mScriptWitnessFiles of
      Just sWit -> (sCred, ScriptWitness ScriptWitnessForStakeAddr sWit)
      Nothing -> (sCred, KeyWitness KeyWitnessForStakeAddr)

-- ----------------------------------------------------------------------------
-- Transaction body validation and conversion
--

txFeatureMismatch
  :: ()
  => Monad m
  => CardanoEra era
  -> TxFeature
  -> ExceptT TxCmdError m a
txFeatureMismatch era feature =
  hoistEither . Left $ TxCmdTxFeatureMismatch (anyCardanoEra era) feature

txFeatureMismatchPure
  :: CardanoEra era
  -> TxFeature
  -> Either TxCmdError a
txFeatureMismatchPure era feature =
  Left (TxCmdTxFeatureMismatch (anyCardanoEra era) feature)

validateTxIns
  :: [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -> [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
validateTxIns = map convert
 where
  convert
    :: (TxIn, Maybe (ScriptWitness WitCtxTxIn era))
    -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
  convert (txin, mScriptWitness) =
    case mScriptWitness of
      Just sWit ->
        (txin, BuildTxWith $ ScriptWitness ScriptWitnessForSpending sWit)
      Nothing ->
        (txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)

validateTxInsCollateral
  :: ShelleyBasedEra era
  -> [TxIn]
  -> Either TxCmdError (TxInsCollateral era)
validateTxInsCollateral _ [] = return TxInsCollateralNone
validateTxInsCollateral era txins = do
  forShelleyBasedEraInEonMaybe era (`TxInsCollateral` txins)
    & maybe (txFeatureMismatchPure (toCardanoEra era) TxFeatureCollateral) Right

validateTxInsReference
  :: ShelleyBasedEra era
  -> [TxIn]
  -> Either TxCmdError (TxInsReference BuildTx era)
validateTxInsReference _ [] = return TxInsReferenceNone
validateTxInsReference sbe allRefIns = do
  forShelleyBasedEraInEonMaybe sbe (`TxInsReference` allRefIns)
    & maybe (txFeatureMismatchPure (toCardanoEra sbe) TxFeatureReferenceInputs) Right

getAllReferenceInputs
  :: [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -> [ScriptWitness WitCtxMint era]
  -> [(Certificate era, Maybe (ScriptWitness WitCtxStake era))]
  -> [(StakeAddress, Coin, Maybe (ScriptWitness WitCtxStake era))]
  -> [(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))]
  -> [(Proposal era, Maybe (ScriptWitness WitCtxStake era))]
  -> [TxIn]
  -- ^ Read only reference inputs
  -> [TxIn]
getAllReferenceInputs
  txins
  mintWitnesses
  certFiles
  withdrawals
  votingProceduresAndMaybeScriptWits
  propProceduresAnMaybeScriptWits
  readOnlyRefIns = do
    let txinsWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- txins]
        mintingRefInputs = map getReferenceInput mintWitnesses
        certsWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- certFiles]
        withdrawalsWitByRefInputs = [getReferenceInput sWit | (_, _, Just sWit) <- withdrawals]
        votesWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- votingProceduresAndMaybeScriptWits]
        propsWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- propProceduresAnMaybeScriptWits]

    Maybe.catMaybes $
      concat
        [ txinsWitByRefInputs
        , mintingRefInputs
        , certsWitByRefInputs
        , withdrawalsWitByRefInputs
        , votesWitByRefInputs
        , propsWitByRefInputs
        , map Just readOnlyRefIns
        ]
   where
    getReferenceInput
      :: ScriptWitness witctx era -> Maybe TxIn
    getReferenceInput sWit =
      case sWit of
        PlutusScriptWitness _ _ (PReferenceScript refIn _) _ _ _ -> Just refIn
        PlutusScriptWitness _ _ PScript{} _ _ _ -> Nothing
        SimpleScriptWitness _ (SReferenceScript refIn _) -> Just refIn
        SimpleScriptWitness _ SScript{} -> Nothing

toAddressInAnyEra
  :: CardanoEra era
  -> AddressAny
  -> Either TxCmdError (AddressInEra era)
toAddressInAnyEra era addrAny = runExcept $ do
  case addrAny of
    AddressByron bAddr -> pure (AddressInEra ByronAddressInAnyEra bAddr)
    AddressShelley sAddr -> do
      sbe <-
        requireShelleyBasedEra era
          & onNothing (txFeatureMismatch era TxFeatureShelleyAddresses)

      pure (AddressInEra (ShelleyAddressInEra sbe) sAddr)

toAddressInShelleyBasedEra
  :: ShelleyBasedEra era
  -> Address ShelleyAddr
  -> Either TxCmdError (AddressInEra era)
toAddressInShelleyBasedEra sbe sAddr =
  runExcept $
    pure (AddressInEra (ShelleyAddressInEra sbe) sAddr)

toTxOutValueInAnyEra
  :: ShelleyBasedEra era
  -> Value
  -> Either TxCmdError (TxOutValue era)
toTxOutValueInAnyEra era val =
  caseShelleyToAllegraOrMaryEraOnwards
    ( \_ -> case valueToLovelace val of
        Just l -> return (TxOutValueShelleyBased era l)
        Nothing -> txFeatureMismatchPure (toCardanoEra era) TxFeatureMultiAssetOutputs
    )
    (\w -> return (TxOutValueShelleyBased era (toLedgerValue w val)))
    era

toTxOutValueInShelleyBasedEra
  :: ShelleyBasedEra era
  -> Value
  -> Either TxCmdError (TxOutValue era)
toTxOutValueInShelleyBasedEra sbe val =
  caseShelleyToAllegraOrMaryEraOnwards
    ( \_ -> case valueToLovelace val of
        Just l -> return (TxOutValueShelleyBased sbe l)
        Nothing -> txFeatureMismatchPure (toCardanoEra sbe) TxFeatureMultiAssetOutputs
    )
    (\w -> return (TxOutValueShelleyBased sbe (toLedgerValue w val)))
    sbe

toTxOutInShelleyBasedEra
  :: ShelleyBasedEra era
  -> TxOutShelleyBasedEra
  -> ExceptT TxCmdError IO (TxOut CtxTx era)
toTxOutInShelleyBasedEra era (TxOutShelleyBasedEra addr' val' mDatumHash refScriptFp) = do
  addr <- hoistEither $ toAddressInShelleyBasedEra era addr'
  val <- hoistEither $ toTxOutValueInShelleyBasedEra era val'

  datum <-
    caseShelleyToMaryOrAlonzoEraOnwards
      (const (pure TxOutDatumNone))
      (`toTxAlonzoDatum` mDatumHash)
      era

  refScript <-
    inEonForEra
      (pure ReferenceScriptNone)
      (`getReferenceScript` refScriptFp)
      (toCardanoEra era)

  pure $ TxOut addr val datum refScript

toTxOutByronEra
  :: TxOutAnyEra
  -> ExceptT TxCmdError IO (TxOut CtxTx ByronEra)
toTxOutByronEra (TxOutAnyEra addr' val' _ _) = do
  addr <- hoistEither $ toAddressInAnyEra ByronEra addr'
  let ada = TxOutValueByron $ selectLovelace val'
  pure $ TxOut addr ada TxOutDatumNone ReferenceScriptNone

-- TODO: toTxOutInAnyEra eventually will not be needed because
-- byron related functionality will be treated
-- separately
toTxOutInAnyEra
  :: ShelleyBasedEra era
  -> TxOutAnyEra
  -> ExceptT TxCmdError IO (TxOut CtxTx era)
toTxOutInAnyEra era (TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  let cEra = toCardanoEra era
  addr <- hoistEither $ toAddressInAnyEra cEra addr'
  val <- hoistEither $ toTxOutValueInAnyEra era val'

  datum <-
    caseShelleyToMaryOrAlonzoEraOnwards
      (const (pure TxOutDatumNone))
      (`toTxAlonzoDatum` mDatumHash)
      era

  refScript <-
    caseShelleyToAlonzoOrBabbageEraOnwards
      (const (pure ReferenceScriptNone))
      (`getReferenceScript` refScriptFp)
      era
  pure $ TxOut addr val datum refScript

getReferenceScript
  :: ()
  => BabbageEraOnwards era
  -> ReferenceScriptAnyEra
  -> ExceptT TxCmdError IO (ReferenceScript era)
getReferenceScript w = \case
  ReferenceScriptAnyEraNone -> return ReferenceScriptNone
  ReferenceScriptAnyEra fp -> ReferenceScript w <$> firstExceptT TxCmdScriptFileError (CLI.readFileScriptInAnyLang fp)

toTxAlonzoDatum
  :: ()
  => AlonzoEraOnwards era
  -> TxOutDatumAnyEra
  -> ExceptT TxCmdError IO (TxOutDatum CtxTx era)
toTxAlonzoDatum supp cliDatum =
  case cliDatum of
    TxOutDatumByNone -> pure TxOutDatumNone
    TxOutDatumByHashOnly h -> pure (TxOutDatumHash supp h)
    TxOutDatumByHashOf sDataOrFile -> do
      sData <- firstExceptT TxCmdScriptDataError $ CLI.readScriptDataOrFile sDataOrFile
      pure (TxOutDatumHash supp $ hashScriptDataBytes sData)
    TxOutDatumByValue sDataOrFile -> do
      sData <- firstExceptT TxCmdScriptDataError $ CLI.readScriptDataOrFile sDataOrFile
      pure (TxOutDatumInTx supp sData)
    TxOutInlineDatumByValue sDataOrFile -> do
      let cEra = toCardanoEra supp
      forEraInEon cEra (txFeatureMismatch cEra TxFeatureInlineDatums) $ \babbageOnwards -> do
        sData <- firstExceptT TxCmdScriptDataError $ CLI.readScriptDataOrFile sDataOrFile
        pure $ TxOutDatumInline babbageOnwards sData

-- TODO: Currently we specify the policyId with the '--mint' option on the cli
-- and we added a separate '--policy-id' parser that parses the policy id for the
-- given reference input (since we don't have the script in this case). To avoid asking
-- for the policy id twice (in the build command) we can potentially query the UTxO and
-- access the script (and therefore the policy id).
createTxMintValue
  :: forall era
   . ShelleyBasedEra era
  -> (Value, [ScriptWitness WitCtxMint era])
  -> Either TxCmdError (TxMintValue BuildTx era)
createTxMintValue era (val, scriptWitnesses) =
  if List.null (valueToList val) && List.null scriptWitnesses
    then return TxMintNone
    else do
      caseShelleyToAllegraOrMaryEraOnwards
        (const (txFeatureMismatchPure (toCardanoEra era) TxFeatureMintValue))
        ( \w -> do
            -- The set of policy ids for which we need witnesses:
            let witnessesNeededSet :: Set PolicyId
                witnessesNeededSet =
                  Set.fromList [pid | (AssetId pid _, _) <- valueToList val]

            let witnessesProvidedMap :: Map PolicyId (ScriptWitness WitCtxMint era)
                witnessesProvidedMap = Map.fromList $ gatherMintingWitnesses scriptWitnesses
                witnessesProvidedSet = Map.keysSet witnessesProvidedMap

            -- Check not too many, nor too few:
            validateAllWitnessesProvided witnessesNeededSet witnessesProvidedSet
            validateNoUnnecessaryWitnesses witnessesNeededSet witnessesProvidedSet
            return (TxMintValue w val (BuildTxWith witnessesProvidedMap))
        )
        era
 where
  gatherMintingWitnesses
    :: [ScriptWitness WitCtxMint era]
    -> [(PolicyId, ScriptWitness WitCtxMint era)]
  gatherMintingWitnesses [] = []
  gatherMintingWitnesses (sWit : rest) =
    case scriptWitnessPolicyId sWit of
      Nothing -> gatherMintingWitnesses rest
      Just pid -> (pid, sWit) : gatherMintingWitnesses rest

  validateAllWitnessesProvided witnessesNeeded witnessesProvided
    | null witnessesMissing = return ()
    | otherwise = Left (TxCmdPolicyIdsMissing witnessesMissing (Set.toList witnessesProvided))
   where
    witnessesMissing = Set.elems (witnessesNeeded Set.\\ witnessesProvided)

  validateNoUnnecessaryWitnesses witnessesNeeded witnessesProvided
    | null witnessesExtra = return ()
    | otherwise = Left (TxCmdPolicyIdsExcess witnessesExtra)
   where
    witnessesExtra = Set.elems (witnessesProvided Set.\\ witnessesNeeded)

scriptWitnessPolicyId :: ScriptWitness witctx era -> Maybe PolicyId
scriptWitnessPolicyId (SimpleScriptWitness _ (SScript script)) =
  Just . scriptPolicyId $ SimpleScript script
scriptWitnessPolicyId (SimpleScriptWitness _ (SReferenceScript _ mPid)) =
  PolicyId <$> mPid
scriptWitnessPolicyId (PlutusScriptWitness _ plutusVersion (PScript script) _ _ _) =
  Just . scriptPolicyId $ PlutusScript plutusVersion script
scriptWitnessPolicyId (PlutusScriptWitness _ _ (PReferenceScript _ mPid) _ _ _) =
  PolicyId <$> mPid

readValueScriptWitnesses
  :: ShelleyBasedEra era
  -> (Value, [ScriptWitnessFiles WitCtxMint])
  -> ExceptT TxCmdError IO (Value, [ScriptWitness WitCtxMint era])
readValueScriptWitnesses era (v, sWitFiles) = do
  sWits <- mapM (firstExceptT TxCmdScriptWitnessError . CLI.readScriptWitness era) sWitFiles
  return (v, sWits)

-- ----------------------------------------------------------------------------
-- Transaction signing
--

runTransactionSignCmd
  :: ()
  => Cmd.TransactionSignCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionSignCmd
  Cmd.TransactionSignCmdArgs
    { txOrTxBodyFile = txOrTxBody
    , witnessSigningData = witnessSigningData
    , mNetworkId = mNetworkId
    , outTxFile = outTxFile
    } = do
    sks <- Monad.forM witnessSigningData $ \d ->
      lift (CLI.readWitnessSigningData d)
        & onLeft (left . TxCmdReadWitnessSigningDataError)

    let (sksByron, sksShelley) = partitionSomeWitnesses $ map CLI.categoriseSomeSigningWitness sks

    case txOrTxBody of
      InputTxFile (File inputTxFilePath) -> do
        inputTxFile <- liftIO $ CLI.fileOrPipe inputTxFilePath
        anyTx <- lift (CLI.readFileTx inputTxFile) & onLeft (left . TxCmdTextEnvCddlError)

        InAnyShelleyBasedEra sbe tx <- pure anyTx

        let (txbody, existingTxKeyWits) = Api.getTxBodyAndWitnesses tx

        byronWitnesses <-
          pure (mkShelleyBootstrapWitnesses sbe mNetworkId txbody sksByron)
            & onLeft (left . TxCmdBootstrapWitnessError)

        let newShelleyKeyWits = map (makeShelleyKeyWitness sbe txbody) sksShelley
            allKeyWits = existingTxKeyWits ++ newShelleyKeyWits ++ byronWitnesses
            signedTx = makeSignedTransaction allKeyWits txbody

        lift (writeTxFileTextEnvelopeCddl sbe outTxFile signedTx)
          & onLeft (left . TxCmdWriteFileError)
      InputTxBodyFile (File txbodyFilePath) -> do
        txbodyFile <- liftIO $ CLI.fileOrPipe txbodyFilePath
        IncompleteCddlTxBody { .. } <-
          firstExceptT TxCmdTextEnvCddlError . newExceptT $
            CLI.readFileTxBody txbodyFile

        case unIncompleteCddlTxBody of
          InAnyShelleyBasedEra sbe txbody -> do

            -- Byron witnesses require the network ID. This can either be provided
            -- directly or derived from a provided Byron address.
            byronWitnesses <-
              firstExceptT TxCmdBootstrapWitnessError
                . hoistEither
                $ mkShelleyBootstrapWitnesses sbe mNetworkId txbody sksByron

            let shelleyKeyWitnesses = map (makeShelleyKeyWitness sbe txbody) sksShelley
                tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

            lift (writeTxFileTextEnvelopeCddl sbe outTxFile tx)
              & onLeft (left . TxCmdWriteFileError)

-- ----------------------------------------------------------------------------
-- Transaction submission
--

runTransactionSubmitCmd
  :: ()
  => Cmd.TransactionSubmitCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionSubmitCmd
  Cmd.TransactionSubmitCmdArgs
    { nodeSocketPath
    , consensusModeParams
    , networkId
    , txFile
    } = do
    txFileOrPipe <- liftIO $ CLI.fileOrPipe txFile
    InAnyShelleyBasedEra era tx <-
      lift (CLI.readFileTx txFileOrPipe) & onLeft (left . TxCmdTextEnvCddlError)
    let txInMode = TxInMode era tx
        localNodeConnInfo =
          LocalNodeConnectInfo
            { localConsensusModeParams = consensusModeParams
            , localNodeNetworkId = networkId
            , localNodeSocketPath = nodeSocketPath
            }

    res <- liftIO $ submitTxToNodeLocal localNodeConnInfo txInMode
    case res of
      Net.Tx.SubmitSuccess -> liftIO $ Text.putStrLn "Transaction successfully submitted."
      Net.Tx.SubmitFail reason ->
        case reason of
          TxValidationErrorInCardanoMode err -> left . TxCmdTxSubmitError . Text.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ TxCmdTxSubmitErrorEraMismatch mismatchErr

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTransactionCalculateMinFeeCmd
  :: ()
  => Cmd.TransactionCalculateMinFeeCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionCalculateMinFeeCmd
  Cmd.TransactionCalculateMinFeeCmdArgs
    { txBodyFile = File txbodyFilePath
    , protocolParamsFile = protocolParamsFile
    , txShelleyWitnessCount = TxShelleyWitnessCount nShelleyKeyWitnesses
    , txByronWitnessCount = TxByronWitnessCount nByronKeyWitnesses
    , referenceScriptSize = ReferenceScriptSize sReferenceScript
    -- , outputFormat
    -- , outFile
    } = do
    txbodyFile <- liftIO $ CLI.fileOrPipe txbodyFilePath
    IncompleteCddlTxBody { .. } <-
      firstExceptT TxCmdTextEnvCddlError . newExceptT $
        CLI.readFileTxBody txbodyFile

    let nShelleyKeyWitW32 = fromIntegral nShelleyKeyWitnesses

    InAnyShelleyBasedEra sbe txbody <- pure unIncompleteCddlTxBody

    lpparams <-
      firstExceptT TxCmdProtocolParamsError $
        CLI.readProtocolParameters sbe protocolParamsFile

    let shelleyfee = evaluateTransactionFee sbe lpparams txbody nShelleyKeyWitW32 0 sReferenceScript

    let byronfee =
          shelleyBasedEraConstraints sbe $
            calculateByronWitnessFees (lpparams ^. Ledger.ppMinFeeAL) nByronKeyWitnesses

    let Coin fee = shelleyfee + byronfee
        textToWrite = Text.pack ((show fee :: String) <> " Lovelace")
        _jsonToWrite = Aeson.encodePretty $ Aeson.object ["fee" .= fee]

    -- The Query prefix should go away whenever I can get to actually
    -- link against the new cardano-cli.
    -- This whole code block can't really be used until the update works.
    -- What used to be there before the above was:
    liftIO $ Text.putStrLn textToWrite
    {-
    case (newOutputFormat outputFormat outFile, outFile) of
      (OutputFormatText, Nothing) ->
        liftIO $ Text.putStrLn textToWrite
      (OutputFormatText, Just file) ->
        firstExceptT TxCmdWriteFileError . newExceptT $ writeTextFile file textToWrite
      (OutputFormatJson, Nothing) ->
        liftIO $ LBS8.putStrLn jsonToWrite
      (OutputFormatJson, Just file) ->
        firstExceptT TxCmdWriteFileError . newExceptT $ writeLazyByteStringFile file jsonToWrite
    -}

-- Extra logic to handle byron witnesses.
-- TODO: move this to Cardano.API.Fee.evaluateTransactionFee.
calculateByronWitnessFees
  :: ()
  => Coin
  -- ^ The tx fee per byte (from protocol parameters)
  -> Int
  -- ^ The number of Byron key witnesses
  -> Coin
calculateByronWitnessFees txFeePerByte byronwitcount =
  Coin $
    toInteger txFeePerByte
      * toInteger byronwitcount
      * toInteger sizeByronKeyWitnesses
 where
  sizeByronKeyWitnesses = smallArray + keyObj + sigObj + ccodeObj + attrsObj

  smallArray = 1

  keyObj = 2 + keyLen
  keyLen = 32

  sigObj = 2 + sigLen
  sigLen = 64

  ccodeObj = 2 + ccodeLen
  ccodeLen = 32

  attrsObj = 2 + ByteString.length attributes

  -- We assume testnet network magic here to avoid having
  -- to thread the actual network ID into this function
  -- merely to calculate the fees of byron witnesses more accurately.
  -- This may slightly over-estimate min fees for byron witnesses
  -- in mainnet transaction by one Word32 per witness.
  attributes =
    CBOR.serialize' $
      Byron.mkAttributes
        Byron.AddrAttributes
          { Byron.aaVKDerivationPath = Nothing
          , Byron.aaNetworkMagic = Byron.NetworkTestnet maxBound
          }

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTransactionCalculateMinValueCmd
  :: ()
  => Cmd.TransactionCalculateMinValueCmdArgs era
  -> ExceptT TxCmdError IO ()
runTransactionCalculateMinValueCmd
  Cmd.TransactionCalculateMinValueCmdArgs
    { eon
    , protocolParamsFile
    , txOut
    } = do
    pp <- firstExceptT TxCmdProtocolParamsError (CLI.readProtocolParameters eon protocolParamsFile)
    out <- toTxOutInShelleyBasedEra eon txOut

    let minValue = calculateMinimumUTxO eon out pp
    liftIO . IO.print $ minValue

runTransactionPolicyIdCmd
  :: ()
  => Cmd.TransactionPolicyIdCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionPolicyIdCmd
  Cmd.TransactionPolicyIdCmdArgs
    { scriptFile = File sFile
    } = do
    ScriptInAnyLang _ script <-
      firstExceptT TxCmdScriptFileError $
        CLI.readFileScriptInAnyLang sFile
    liftIO . Text.putStrLn . serialiseToRawBytesHexText $ hashScript script

partitionSomeWitnesses
  :: [ByronOrShelleyWitness]
  -> ( [ShelleyBootstrapWitnessSigningKeyData]
     , [ShelleyWitnessSigningKey]
     )
partitionSomeWitnesses = reversePartitionedWits . List.foldl' go mempty
 where
  reversePartitionedWits (bw, skw) =
    (reverse bw, reverse skw)

  go (byronAcc, shelleyKeyAcc) byronOrShelleyWit =
    case byronOrShelleyWit of
      AByronWitness byronWit ->
        (byronWit : byronAcc, shelleyKeyAcc)
      AShelleyKeyWitness shelleyKeyWit ->
        (byronAcc, shelleyKeyWit : shelleyKeyAcc)

-- | Construct a Shelley bootstrap witness (i.e. a Byron key witness in the
-- Shelley era).
mkShelleyBootstrapWitness
  :: ()
  => ShelleyBasedEra era
  -> Maybe NetworkId
  -> TxBody era
  -> ShelleyBootstrapWitnessSigningKeyData
  -> Either BootstrapWitnessError (KeyWitness era)
mkShelleyBootstrapWitness _ Nothing _ (ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrByronAddressError
mkShelleyBootstrapWitness sbe (Just nw) txBody (ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ makeShelleyBootstrapWitness sbe (Byron.WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness sbe _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ makeShelleyBootstrapWitness sbe (Byron.WitnessByronAddress addr) txBody skey

-- | Attempt to construct Shelley bootstrap witnesses until an error is
-- encountered.
mkShelleyBootstrapWitnesses
  :: ()
  => ShelleyBasedEra era
  -> Maybe NetworkId
  -> TxBody era
  -> [ShelleyBootstrapWitnessSigningKeyData]
  -> Either BootstrapWitnessError [KeyWitness era]
mkShelleyBootstrapWitnesses sbe mnw txBody =
  mapM (mkShelleyBootstrapWitness sbe mnw txBody)

-- ----------------------------------------------------------------------------
-- Other misc small commands
--

runTransactionHashScriptDataCmd
  :: ()
  => Cmd.TransactionHashScriptDataCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionHashScriptDataCmd
  Cmd.TransactionHashScriptDataCmdArgs
    { scriptDataOrFile
    } = do
    d <- firstExceptT TxCmdScriptDataError $ CLI.readScriptDataOrFile scriptDataOrFile
    liftIO $ BS8.putStrLn $ serialiseToRawBytesHex (hashScriptDataBytes d)

runTransactionTxIdCmd
  :: ()
  => Cmd.TransactionTxIdCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionTxIdCmd
  Cmd.TransactionTxIdCmdArgs
    { inputTxBodyOrTxFile
    } = do
    InAnyShelleyBasedEra _era txbody <-
      case inputTxBodyOrTxFile of
        InputTxBodyFile (File txbodyFilePath) -> do
          txbodyFile <- liftIO $ CLI.fileOrPipe txbodyFilePath
          IncompleteCddlTxBody { .. } <-
            firstExceptT TxCmdTextEnvCddlError . newExceptT $
              CLI.readFileTxBody txbodyFile
          pure unIncompleteCddlTxBody
        InputTxFile (File txFilePath) -> do
          txFile <- liftIO $ CLI.fileOrPipe txFilePath
          InAnyShelleyBasedEra era tx <- lift (CLI.readFileTx txFile) & onLeft (left . TxCmdTextEnvCddlError)
          return . InAnyShelleyBasedEra era $ getTxBody tx

    liftIO $ BS8.putStrLn $ serialiseToRawBytesHex (getTxId txbody)

runTransactionViewCmd
  :: ()
  => Cmd.TransactionViewCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionViewCmd
  Cmd.TransactionViewCmdArgs
    { outputFormat
    , mOutFile
    , inputTxBodyOrTxFile
    } =
    case inputTxBodyOrTxFile of
      InputTxBodyFile (File txbodyFilePath) -> do
        txbodyFile <- liftIO $ CLI.fileOrPipe txbodyFilePath
        IncompleteCddlTxBody { unIncompleteCddlTxBody = InAnyShelleyBasedEra era txbody } <-
          firstExceptT TxCmdTextEnvCddlError . newExceptT $
            CLI.readFileTxBody txbodyFile
        -- Why are we differentiating between a transaction body and a transaction?
        -- In the case of a transaction body, we /could/ simply call @makeSignedTransaction []@
        -- to get a transaction which would allow us to reuse friendlyTxBS. However,
        -- this would mean that we'd have an empty list of witnesses mentioned in the output, which
        -- is arguably not part of the transaction body.
        firstExceptT TxCmdWriteFileError . newExceptT $
          case outputFormat of
            ViewOutputFormatYaml -> friendlyTxBody FriendlyYaml mOutFile (toCardanoEra era) txbody
            ViewOutputFormatJson -> friendlyTxBody FriendlyJson mOutFile (toCardanoEra era) txbody
      InputTxFile (File txFilePath) -> do
        txFile <- liftIO $ CLI.fileOrPipe txFilePath
        InAnyShelleyBasedEra era tx <- lift (CLI.readFileTx txFile) & onLeft (left . TxCmdTextEnvCddlError)
        firstExceptT TxCmdWriteFileError . newExceptT $
          case outputFormat of
            ViewOutputFormatYaml -> friendlyTx FriendlyYaml mOutFile (toCardanoEra era) tx
            ViewOutputFormatJson -> friendlyTx FriendlyJson mOutFile (toCardanoEra era) tx

-- ----------------------------------------------------------------------------
-- Witness commands
--

runTransactionWitnessCmd
  :: ()
  => Cmd.TransactionWitnessCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionWitnessCmd
  Cmd.TransactionWitnessCmdArgs
    { txBodyFile = File txbodyFilePath
    , witnessSigningData
    , mNetworkId
    , outFile
    } = do
    txbodyFile <- liftIO $ CLI.fileOrPipe txbodyFilePath
    IncompleteCddlTxBody { unIncompleteCddlTxBody = InAnyShelleyBasedEra sbe txbody } <-
      firstExceptT TxCmdTextEnvCddlError . newExceptT $
        CLI.readFileTxBody txbodyFile
    someWit <-
      firstExceptT TxCmdReadWitnessSigningDataError
        . newExceptT
        $ CLI.readWitnessSigningData witnessSigningData
    witness <-
      case CLI.categoriseSomeSigningWitness someWit :: ByronOrShelleyWitness of
        -- Byron witnesses require the network ID. This can either be provided
        -- directly or derived from a provided Byron address.
        AByronWitness bootstrapWitData ->
          firstExceptT TxCmdBootstrapWitnessError
            . hoistEither
            $ mkShelleyBootstrapWitness sbe mNetworkId txbody bootstrapWitData
        AShelleyKeyWitness skShelley ->
          pure $ makeShelleyKeyWitness sbe txbody skShelley

    firstExceptT TxCmdWriteFileError . newExceptT $
      writeTxWitnessFileTextEnvelopeCddl sbe outFile witness

runTransactionSignWitnessCmd
  :: ()
  => Cmd.TransactionSignWitnessCmdArgs
  -> ExceptT TxCmdError IO ()
runTransactionSignWitnessCmd
  Cmd.TransactionSignWitnessCmdArgs
    { txBodyFile = File txbodyFilePath
    , witnessFiles = witnessFiles
    , outFile = outFile
    } = do
    txbodyFile <- liftIO $ CLI.fileOrPipe txbodyFilePath
    IncompleteCddlTxBody { unIncompleteCddlTxBody = InAnyShelleyBasedEra era txbody } <- lift (CLI.readFileTxBody txbodyFile) & onLeft (left . TxCmdTextEnvCddlError)
    -- TODO: Left off here. Remember we were never reading byron key witnesses anyways!
    witnesses <-
      sequence
        [ do
            InAnyShelleyBasedEra era' witness <-
              lift (CLI.readFileTxKeyWitness file) & onLeft (left . TxCmdCddlWitnessError)

            case testEquality era era' of
              Nothing ->
                left $
                  TxCmdWitnessEraMismatch
                    (AnyCardanoEra $ toCardanoEra era)
                    (AnyCardanoEra $ toCardanoEra era')
                    witnessFile
              Just Refl -> return witness
        | witnessFile@(WitnessFile file) <- witnessFiles
        ]

    let tx = makeSignedTransaction witnesses txbody

    lift (writeTxFileTextEnvelopeCddl era outFile tx) & onLeft (left . TxCmdWriteFileError)
