-- At the risk of duplicating the language extension pragmas and comments
-- thereon from GovExample.hs:
-- The extensions may have been overly liberally used, though they did
-- help speed up development, as far as it got.


-- This section of extensions is mostly short syntax, and they're just
-- very helpful for putting code together in various ways.
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}


-- These two go together, and are extremely useful for constraining the
-- types of local variables or otherwise expressing them in terms of the
-- types appearing in the type signature of the top-level CAF or function
-- even apart from where they aid more advanced type-level programming.
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}


-- The precise implications of this may not be fully understood; however,
-- avoiding relying on it inadvertently seems tricky.
{-# LANGUAGE FlexibleContexts      #-}


-- This is very likely redundant. I believe RankNTypes implies it.
{-# LANGUAGE QuantifiedConstraints #-}


-- These more advanced features are large for interoperability with
-- imported code. While they don't seem so far out as standalone concepts,
-- Other People's Code (TM) that uses them is
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


-- PartialTypeSignatures was extremely useful during the development
-- process, even if it didn't make enough progress.
-- It appears to allow one to ask the type inferencer a question of what
-- type is inferred in the place that an underscore appears, or possibly
-- a type variable whose name begins with an underscore, which may depend
-- on an extension vs. just a single underscore alone.
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-error=partial-type-signatures #-}


-- Which record fields conflicted should have been logged.
-- It's not clear that the conflict was intentional, and it was from code
-- outside the direct influence of the project.
{-# LANGUAGE DuplicateRecordFields #-}


{- These are all pretty reasonable warning options.
 - These disablings of warnings and makings of warnings not errors
 - ("softenings") aren't needed anymore as the code now stands.
 - It could still be useful to keep these ready to reactivate in
 - the event of rapid code restructurings until it comes time to
 - do the final cleanup of the commit sequence.
 -* OPTIONS_GHC -fno-warn-incomplete-uni-patterns  *-
 -* OPTIONS_GHC -Wno-unrecognised-pragmas          *-
 -}

-- Switching these warnings on intermittently seemed to speed development
-- up slightly because compiles are still slow enough even on a
-- decent-capacity laptop that it helps to batch warning fixes.
{-# OPTIONS_GHC -Wno-error=unused-imports          #-}
{-# OPTIONS_GHC -Wno-error=unused-matches          #-}
{-# OPTIONS_GHC -Wno-error=unused-local-binds      #-}
{-# OPTIONS_GHC -Wno-error=redundant-constraints   #-}

{- These also used to be needed to pass hlint checks, but the warnings
 - have likewise since been silenced, so they're no longer needed to
 - make progress in the interim. In like fashion, it may be useful to
 - let these disabled pragmas linger until the point of final clean-up
 - of the commit sequence for intensive restructurings.
 -* HLINT ignore "Unused LANGUAGE pragma"          *-
 -* HLINT ignore "Avoid lambda using `infix`"      *-
 -}


module  Cardano.TxGenerator.GovExample.CLI where

import qualified Cardano.Api as Api
                   ( CardanoEra (..)
                   , Lovelace
                   , NetworkId (..)
                   , PaymentKey
                   , TxMetadataInEra (..)
                   , Value)
import qualified Cardano.Api.Experimental as Api (UnsignedTx (..))
import qualified Cardano.Api.Byron as Byron (WitnessNetworkIdOrByronAddress (..))
import qualified Cardano.Api.Ledger as Ledger
                   ( ConwayEraTxCert (..)
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
import qualified Cardano.Api.Shelley as Api
                   ( BabbageEraOnwards (..)
                   , BuildTx
                   , BuildTxWith (..)
                   , CtxTx
                   , KeyWitness (..)
                   , ReferenceScript (..)
                   , ScriptWitness (..)
                   , ShelleyBasedEra (..)
                   , Tx (..)
                   , TxAuxScripts (..)
                   , TxBody (..)
                   , TxBodyContent (..)
                   , TxCertificates (..)
                   , TxCurrentTreasuryValue (..)
                   , TxFee (..)
                   , TxIn (..)
                   , TxInMode (..)
                   , TxInsReference (..)
                   , TxInsCollateral (..)
                   , TxMintValue (..)
                   , TxOut (..)
                   , TxOutDatum (..)
                   , TxOutValue (..)
                   , TxUpdateProposal (..)
                   , TxValidationErrorInCardanoMode (..)
                   , TxValidityUpperBound (..)
                   , TxWithdrawals (..)
                   , WitCtxMint
                   , WitCtxStake
                   , WitCtxTxIn
                   , Witness (..)
                   , alonzoEraOnwardsConstraints
                   , anyAddressInEra
                   , anyAddressInShelleyBasedEra
                   , anyCardanoEra
                   , calculateMinimumUTxO
                   , cardanoEraConstraints
                   , caseShelleyToAllegraOrMaryEraOnwards
                   , caseShelleyToAlonzoOrBabbageEraOnwards
                   , caseShelleyToMaryOrAlonzoEraOnwards
                   , collectTxBodyScriptWitnesses
                   , conwayEraOnwardsConstraints
                   , createAndValidateTransactionBody
                   , defaultTxBodyContent
                   , estimateBalancedTxBody
                   , evaluateTransactionExecutionUnits
                   , evaluateTransactionFee
                   , executeLocalStateQueryExpr
                   , firstExceptT
                   , forEraInEon
                   , forEraInEonMaybe
                   , forShelleyBasedEraInEonMaybe
                   , fromShelleyStakeCredential
                   , getTxBody
                   , getTxBodyAndWitnesses
                   , getTxId
                   , hashScript
                   , hashScriptDataBytes
                   , hoistEither
                   , hoistMaybe
                   , inEonForEra
                   , inEonForShelleyBasedEra
                   , left
                   , lift
                   , liftIO
                   , makeShelleyBootstrapWitness
                   , makeShelleyKeyWitness
                   , makeSignedTransaction
                   , makeTransactionBodyAutoBalance
                   , maryEraOnwardsToShelleyBasedEra
                   , mkFeatured
                   , mkTxProposalProcedures
                   , mkTxVotingProcedures
                   , newExceptT
                   , notScriptLockedTxIns
                   , onLeft
                   , onNothing
                   , queryCurrentEra
                   , queryStateForBalancedTx
                   , readFileTextEnvelope
                   , requireShelleyBasedEra
                   , runExcept
                   , scriptPolicyId
                   , selectLovelace
                   , selectStakeCredentialWitness
                   , serialiseToRawBytesHex
                   , serialiseToRawBytesHexText
                   , setTxAuxScripts
                   , setTxCertificates
                   , setTxCurrentTreasuryValue
                   , setTxExtraKeyWits
                   , setTxFee
                   , setTxIns
                   , setTxInsCollateral
                   , setTxInsReference
                   , setTxMetadata
                   , setTxMintValue
                   , setTxOuts
                   , setTxProposalProcedures
                   , setTxProtocolParams
                   , setTxScriptValidity
                   , setTxReturnCollateral
                   , setTxTotalCollateral
                   , setTxTreasuryDonation
                   , setTxUpdateProposal
                   , setTxValidityLowerBound
                   , setTxValidityUpperBound
                   , setTxVotingProcedures
                   , setTxWithdrawals
                   , shelleyBasedEraConstraints
                   , shelleyToBabbageEraConstraints
                   , submitTxToNodeLocal
                   , toCardanoEra
                   , toLedgerEpochInfo
                   , toLedgerValue
                   , txInsExistInUTxO
                   , unFeatured
                   , valueToList
                   , valueToLovelace
                   , writeLazyByteStringFile
                   , writeTextFile
                   , writeTxFileTextEnvelopeCddl
                   , writeTxWitnessFileTextEnvelopeCddl
                   , Address (..)
                   , AddressAny (..)
                   , AddressInEra (..)
                   , AddressTypeInEra (..)
                   , AlonzoEraOnwards (..)
                   , AnyCardanoEra (..)
                   , AnyScriptWitness (..)
                   , AssetId (..)
                   , AsType (..)
                   , BalancedTxBody (..)
                   , ByronEra
                   , Certificate (..)
                   , ConsensusModeParams (..)
                   , EpochSlots (..)
                   , ExceptT (..)
                   , Featured (..)
                   , File (..)
                   , Hash (..)
                   , InAnyShelleyBasedEra (..)
                   , KeyWitnessInCtx (..)
                   , LedgerProtocolParameters (..)
                   , LocalNodeConnectInfo (..)
                   , PlutusScriptOrReferenceInput (..)
                   , PolicyId (..)
                   , PoolId
                   , Proposal (..)
                   , QueryConvenienceError (..)
                   , Script (..)
                   , ScriptInAnyLang (..)
                   , ScriptValidity (..)
                   , ScriptWitnessInCtx (..)
                   , ShelleyAddr
                   , ShelleyLedgerEra
                   , ShelleyWitnessSigningKey (..)
                   , SimpleScriptOrReferenceInput (..)
                   , SlotNo (..)
                   , SocketPath
                   , StakeAddress (..)
                   , StakeCredential (..)
                   , TxProposalProcedures (..)
                   , VotingProcedures (..))

import qualified Cardano.Binary as CBOR (serialize')
import qualified Cardano.Chain.Common as Byron (AddrAttributes (..)
                   , NetworkMagic (..), mkAttributes)
import qualified Cardano.CLI.EraBased.Commands.Transaction as Cmd
                   ( TransactionBuildCmdArgs (..)
                   , TransactionBuildEstimateCmdArgs (..)
                   , TransactionBuildRawCmdArgs (..)
                   , TransactionCalculateMinFeeCmdArgs (..)
                   , TransactionCalculateMinValueCmdArgs (..)
                   , TransactionCmds (..)
                   , TransactionHashScriptDataCmdArgs (..)
                   , TransactionPolicyIdCmdArgs (..)
                   , TransactionSignCmdArgs (..)
                   , TransactionSignWitnessCmdArgs (..)
                   , TransactionSubmitCmdArgs (..)
                   , TransactionTxIdCmdArgs (..)
                   , TransactionViewCmdArgs (..)
                   , TransactionWitnessCmdArgs (..))
-- Unqualified imports of types need to be re-qualified before a PR.
-- Adjust line break to stylish-haskell/fourmolu/etc.
-- import qualified Cardano.CLI.Commands.Debug.TransactionView as Cmd
-- Intentionally trigger rebuild to trigger warnings to clean up.
import qualified Cardano.CLI.Read as CLI
                   ( ByronOrShelleyWitness (..)
                   , IncompleteCddlTxBody (..)
                   , ShelleyBootstrapWitnessSigningKeyData (..)
                   , categoriseSomeSigningWitness
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
import qualified Cardano.CLI.EraBased.Run.Genesis.Common as CLI
                    (readProtocolParameters)
import qualified Cardano.CLI.EraBased.Run.Query as CLI (newOutputFormat)
import qualified Cardano.CLI.Types.Common as CLI
                   ( CertificateFile (..)
                   , InputTxBodyOrTxFile (..)
                   , OutputFormatJsonOrText (..)
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
                   , WitnessFile (..))
import           Cardano.CLI.Types.Errors.BootstrapWitnessError
                    (BootstrapWitnessError (..))
import           Cardano.CLI.Types.Errors.NodeEraMismatchError
                    (NodeEraMismatchError (..))
import qualified Cardano.CLI.Types.Errors.TxCmdError as CLI
                    (TxCmdError (..))
import           Cardano.CLI.Types.Errors.TxCmdError
                   ( AnyTxCmdTxExecUnitsErr (..)
                   , AnyTxBodyErrorAutoBalance (..))
import qualified Cardano.CLI.Types.Errors.TxValidationError as CLI
                   ( TxGovDuplicateVotes (..)
                   , validateRequiredSigners
                   , validateTxAuxScripts
                   , validateTxCurrentTreasuryValue
                   , validateTxReturnCollateral
                   , validateTxScriptValidity
                   , validateTxTotalCollateral
                   , validateTxTreasuryDonation
                   , validateTxValidityLowerBound)
import qualified Cardano.CLI.Types.Output as CLI (renderScriptCosts)
import qualified Cardano.CLI.Types.TxFeature as CLI (TxFeature (..))
import qualified Cardano.Ledger.Api.Tx as Ledger (Tx (..))
import qualified Cardano.Ledger.Api as Ledger (Tx)
import qualified Cardano.Ledger.Alonzo.Core as Ledger (Tx)
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Ledger
                   (pattern RetirePoolTxCert , ppMinFeeAL)
import qualified Cardano.Ledger.Crypto as Crypto (StandardCrypto)

import qualified Control.Arrow as Arrow (first, left)
import qualified Control.Monad as Monad (forM)
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson (object)
import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import qualified Data.ByteString as ByteString (length)
import qualified Data.ByteString.Char8 as BS8 (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (putStrLn, writeFile)
import           Data.Function ((&))
import qualified Data.List as List (foldl', null)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, keysSet)
import qualified Data.Maybe as Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set (elems, fromList, toList, (\\))
import qualified Data.Text as Text (pack)
import qualified Data.Text.IO as Text (putStrLn)
import qualified Data.Type.Equality as Equality ((:~:)(..), testEquality)

import           Lens.Micro ((^.))

import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as
         Consensus (Target (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as
            Net.Tx (SubmitResult (..))

import qualified System.Exit as Exit (ExitCode (..), exitWith)
import qualified System.IO as IO (hPutStrLn, print, stderr)


{-
runTransactionCmds :: Cmd.TransactionCmds era -> Api.ExceptT CLI.TxCmdError IO ()
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
-}

-- ----------------------------------------------------------------------------
-- Building transactions
--

{-
runTransactionBuildCmd
  :: ()
  => Cmd.TransactionBuildCmdArgs era
  -> Api.ExceptT CLI.TxCmdError IO ()
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
    } = Api.shelleyBasedEraConstraints eon $ do
    let era = Api.toCardanoEra eon

    -- The user can specify an era prior to the era that the node is currently in.
    -- We cannot use the user specified era to construct a query against a node because it may differ
    -- from the node's era and this will result in the 'QueryEraMismatch' failure.

    let localNodeConnInfo =
          Api.LocalNodeConnectInfo
            { localConsensusModeParams = consensusModeParams
            , localNodeNetworkId = networkId
            , localNodeSocketPath = nodeSocketPath
            }

    inputsAndMaybeScriptWits <- Api.firstExceptT CLI.TxCmdScriptWitnessError $ CLI.readScriptWitnessFiles eon txins
    certFilesAndMaybeScriptWits <-
      Api.firstExceptT CLI.TxCmdScriptWitnessError $ CLI.readScriptWitnessFiles eon certificates

    -- TODO: Conway Era - How can we make this more composable?
    certsAndMaybeScriptWits <-
      sequence
        [ fmap
            (,mSwit)
            ( Api.firstExceptT CLI.TxCmdReadTextViewFileError . Api.newExceptT $
                Api.readFileTextEnvelope Api.AsCertificate (Api.File certFile)
            )
        | (CLI.CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
        ]
    withdrawalsAndMaybeScriptWits <-
      Api.firstExceptT CLI.TxCmdScriptWitnessError $
        CLI.readScriptWitnessFilesTuple eon withdrawals
    txMetadata <-
      Api.firstExceptT CLI.TxCmdMetadataError . Api.newExceptT $
        CLI.readTxMetadata eon metadataSchema metadataFiles
    valuesWithScriptWits <- readValueScriptWitnesses eon $ Maybe.fromMaybe mempty mValue
    scripts <-
      Api.firstExceptT CLI.TxCmdScriptFileError $
        mapM (CLI.readFileScriptInAnyLang . Api.unFile) scriptFiles
    txAuxScripts <-
      Api.hoistEither $ Arrow.left CLI.TxCmdAuxScriptsValidationError $ CLI.validateTxAuxScripts eon scripts

    mProp <- case mUpdateProposalFile of
      Just (Api.Featured w (Just updateProposalFile)) ->
        CLI.readTxUpdateProposal w updateProposalFile & Api.firstExceptT CLI.TxCmdReadTextViewFileError
      _ -> pure Api.TxUpdateProposalNone

    requiredSigners <-
      mapM (Api.firstExceptT CLI.TxCmdRequiredSignerError . Api.newExceptT . CLI.readRequiredSigner) reqSigners
    mReturnCollateral <- Monad.forM mReturnColl $ toTxOutInShelleyBasedEra eon

    txOuts <- mapM (toTxOutInAnyEra eon) txouts

    -- Conway related
    votingProceduresAndMaybeScriptWits <-
      Api.inEonForEra
        (pure mempty)
        (\w -> Api.firstExceptT CLI.TxCmdVoteError $ Api.ExceptT (CLI.readVotingProceduresFiles w voteFiles))
        era

    proposals <-
      Api.newExceptT $
        Arrow.left CLI.TxCmdProposalError
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

    Api.AnyCardanoEra nodeEra <-
      Api.lift (Api.executeLocalStateQueryExpr localNodeConnInfo Consensus.VolatileTip Api.queryCurrentEra)
        & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . Api.AcqFailure)
        & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . Api.QceUnsupportedNtcVersion)

    (txEraUtxo, _, eraHistory, systemStart, _, _, _, featuredCurrentTreasuryValueM) <-
      Api.lift
        ( Api.executeLocalStateQueryExpr
            localNodeConnInfo
            Consensus.VolatileTip
            (Api.queryStateForBalancedTx nodeEra allTxInputs [])
        )
        & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . Api.AcqFailure)
        & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError)

    let currentTreasuryValueAndDonation =
          case (treasuryDonation, Api.unFeatured <$>
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
    Api.BalancedTxBody txBodyContent (Api.UnsignedTx (Api.ShelleyTx _ (Ledger.Tx { body = balancedTxBody }))) _ _ <-
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
      CLI.OutputScriptCostOnly fp -> do
        let Api.BuildTxWith mTxProtocolParams = Api.txProtocolParams txBodyContent

        pparams <- pure mTxProtocolParams & Api.onNothing (Api.left CLI.TxCmdProtocolParametersNotPresentInTxBody)
        executionUnitPrices <-
          pure (getExecutionUnitPrices era pparams) & Api.onNothing (Api.left CLI.TxCmdPParamExecutionUnitsNotAvailable)

        Equality.Refl <-
          Equality.testEquality era nodeEra
            & Api.hoistMaybe (CLI.TxCmdTxNodeEraMismatchError $ NodeEraMismatchError era nodeEra)

        scriptExecUnitsMap <-
          Api.firstExceptT (CLI.TxCmdTxExecUnitsErr . AnyTxCmdTxExecUnitsErr) $
            Api.hoistEither $
              Api.evaluateTransactionExecutionUnits
                era
                systemStart
                (Api.toLedgerEpochInfo eraHistory)
                pparams
                txEraUtxo
                balancedTxBody

        let mScriptWits = Api.forEraInEon era [] $ \sbe -> Api.collectTxBodyScriptWitnesses sbe txBodyContent

        scriptCostOutput <-
          Api.firstExceptT CLI.TxCmdPlutusScriptCostErr $
            Api.hoistEither $
              CLI.renderScriptCosts
                txEraUtxo
                executionUnitPrices
                mScriptWits
                scriptExecUnitsMap
        Api.liftIO $ LBS8.writeFile (Api.unFile fp) $ Aeson.encodePretty scriptCostOutput
      CLI.OutputTxBodyOnly fpath ->
        let noWitTx = Api.makeSignedTransaction [] balancedTxBody
         in Api.lift (Api.cardanoEraConstraints era $ Api.writeTxFileTextEnvelopeCddl eon fpath noWitTx)
              & Api.onLeft (Api.left . CLI.TxCmdWriteFileError)
-}

{-
runTransactionBuildEstimateCmd
  :: ()
  => Cmd.TransactionBuildEstimateCmdArgs era
  -> Api.ExceptT CLI.TxCmdError IO ()
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
    , changeAddress = CLI.TxOutChangeAddress changeAddr
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
    , currentTreasuryValueAndDonation
    , txBodyOutFile
    } = do
    let sbe = Api.maryEraOnwardsToShelleyBasedEra eon
    ledgerPParams <-
      Api.firstExceptT CLI.TxCmdProtocolParamsError $ CLI.readProtocolParameters sbe protocolParamsFile
    inputsAndMaybeScriptWits <-
      Api.firstExceptT CLI.TxCmdScriptWitnessError $
        CLI.readScriptWitnessFiles sbe txins
    certFilesAndMaybeScriptWits <-
      Api.firstExceptT CLI.TxCmdScriptWitnessError $
        CLI.readScriptWitnessFiles sbe certificates

    withdrawalsAndMaybeScriptWits <-
      Api.firstExceptT CLI.TxCmdScriptWitnessError $
        CLI.readScriptWitnessFilesTuple sbe withdrawals
    txMetadata <-
      Api.firstExceptT CLI.TxCmdMetadataError
        . Api.newExceptT
        $ CLI.readTxMetadata sbe metadataSchema metadataFiles
    valuesWithScriptWits <- readValueScriptWitnesses sbe $ Maybe.fromMaybe mempty mValue
    scripts <-
      Api.firstExceptT CLI.TxCmdScriptFileError $
        mapM (CLI.readFileScriptInAnyLang . Api.unFile) scriptFiles
    txAuxScripts <-
      Api.hoistEither $ Arrow.left CLI.TxCmdAuxScriptsValidationError $ CLI.validateTxAuxScripts sbe scripts

    txUpdateProposal <- case mUpdateProposalFile of
      Just (Api.Featured w (Just updateProposalFile)) ->
        CLI.readTxUpdateProposal w updateProposalFile & Api.firstExceptT CLI.TxCmdReadTextViewFileError
      _ -> pure Api.TxUpdateProposalNone

    requiredSigners <-
      mapM (Api.firstExceptT CLI.TxCmdRequiredSignerError . Api.newExceptT . CLI.readRequiredSigner) reqSigners

    mReturnCollateral <- mapM (toTxOutInShelleyBasedEra sbe) mReturnColl

    txOuts <- mapM (toTxOutInAnyEra sbe) txouts

    -- the same collateral input can be used for several plutus scripts
    let filteredTxinsc = Set.toList $ Set.fromList txInsCollateral

    -- Conway related
    votingProceduresAndMaybeScriptWits <-
      Api.inEonForShelleyBasedEra
        (pure mempty)
        ( \w ->
            Api.firstExceptT CLI.TxCmdVoteError . Api.ExceptT $
              Api.conwayEraOnwardsConstraints w $
                CLI.readVotingProceduresFiles w voteFiles
        )
        sbe

    proposals <-
      Api.lift (CLI.readTxGovernanceActions sbe proposalFiles)
        & Api.onLeft (Api.left . CLI.TxCmdProposalError)

    certsAndMaybeScriptWits <-
      Api.shelleyBasedEraConstraints sbe $
        sequence
          [ fmap
              (,mSwit)
              ( Api.firstExceptT CLI.TxCmdReadTextViewFileError . Api.newExceptT $
                  Api.readFileTextEnvelope Api.AsCertificate (Api.File certFile)
              )
          | (CLI.CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]

    txBodyContent <-
      Api.hoistEither $
        constructTxBodyContent
          sbe
          mScriptValidity
          (Just ledgerPParams)
          inputsAndMaybeScriptWits
          readOnlyRefIns
          filteredTxinsc
          mReturnCollateral
          Nothing -- TODO: Remove total collateral parameter from Api.estimateBalancedTxBody
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
            | (sWitIndex, Api.AnyScriptWitness (Api.PlutusScriptWitness _ _ _ _ _ execUnits)) <-
                Api.collectTxBodyScriptWitnesses sbe txBodyContent
            ]

    Api.BalancedTxBody _ balancedTxBody _ _ <-
      Api.hoistEither $
        Arrow.left CLI.TxCmdFeeEstimationError $
          Api.estimateBalancedTxBody
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
            (maybe 0 CLI.unReferenceScriptSize totalReferenceScriptSize)
            (Api.anyAddressInShelleyBasedEra sbe changeAddr)
            totalUTxOValue

    let noWitTx = Api.makeSignedTransaction [] balancedTxBody
    Api.lift (Api.writeTxFileTextEnvelopeCddl sbe txBodyOutFile noWitTx)
      & Api.onLeft (Api.left . CLI.TxCmdWriteFileError)
-}

getPoolDeregistrationInfo
  :: Api.Certificate era
  -> Maybe Api.PoolId
getPoolDeregistrationInfo (Api.ShelleyRelatedCertificate w cert) =
  Api.shelleyToBabbageEraConstraints w $ getShelleyDeregistrationPoolId cert
getPoolDeregistrationInfo (Api.ConwayCertificate w cert) =
  Api.conwayEraOnwardsConstraints w $ getConwayDeregistrationPoolId cert

getShelleyDeregistrationPoolId
  :: Ledger.EraCrypto (Api.ShelleyLedgerEra era) ~ Crypto.StandardCrypto
  => Ledger.ShelleyEraTxCert (Api.ShelleyLedgerEra era)
  => Ledger.TxCert (Api.ShelleyLedgerEra era) ~ Ledger.ShelleyTxCert (Api.ShelleyLedgerEra era)
  => Ledger.ShelleyTxCert (Api.ShelleyLedgerEra era)
  -> Maybe Api.PoolId
getShelleyDeregistrationPoolId cert = do
  case cert of
    Ledger.RetirePoolTxCert poolId _ -> Just (Api.StakePoolKeyHash poolId)
    _ -> Nothing

getConwayDeregistrationPoolId
  :: Ledger.EraCrypto (Api.ShelleyLedgerEra era) ~ Crypto.StandardCrypto
  => Ledger.TxCert (Api.ShelleyLedgerEra era) ~ Ledger.ConwayTxCert (Api.ShelleyLedgerEra era)
  => Ledger.ConwayEraTxCert (Api.ShelleyLedgerEra era)
  => Ledger.ConwayTxCert (Api.ShelleyLedgerEra era)
  -> Maybe Api.PoolId
getConwayDeregistrationPoolId cert = do
  case cert of
    Ledger.RetirePoolTxCert poolId _ -> Just (Api.StakePoolKeyHash poolId)
    _ -> Nothing

getDRepDeregistrationInfo
  :: Api.Certificate era
  -> Maybe (Ledger.Credential Ledger.DRepRole Crypto.StandardCrypto, Coin)
getDRepDeregistrationInfo Api.ShelleyRelatedCertificate{} = Nothing
getDRepDeregistrationInfo (Api.ConwayCertificate w cert) =
  Api.conwayEraOnwardsConstraints w $ getConwayDRepDeregistrationInfo cert

getConwayDRepDeregistrationInfo
  :: Ledger.EraCrypto (Api.ShelleyLedgerEra era) ~ Crypto.StandardCrypto
  => Ledger.TxCert (Api.ShelleyLedgerEra era) ~ Ledger.ConwayTxCert (Api.ShelleyLedgerEra era)
  => Ledger.ConwayEraTxCert (Api.ShelleyLedgerEra era)
  => Ledger.ConwayTxCert (Api.ShelleyLedgerEra era)
  -> Maybe (Ledger.Credential Ledger.DRepRole Crypto.StandardCrypto, Coin)
getConwayDRepDeregistrationInfo = Ledger.getUnRegDRepTxCert

getStakeDeregistrationInfo
  :: Api.Certificate era
  -> Maybe (Api.StakeCredential, Coin)
getStakeDeregistrationInfo (Api.ShelleyRelatedCertificate w cert) =
  Api.shelleyToBabbageEraConstraints w $ getShelleyDeregistrationInfo cert
getStakeDeregistrationInfo (Api.ConwayCertificate w cert) =
  Api.conwayEraOnwardsConstraints w $ getConwayDeregistrationInfo cert

-- There for no deposits required pre-conway for registering stake
-- credentials.
getShelleyDeregistrationInfo
  :: Ledger.EraCrypto (Api.ShelleyLedgerEra era) ~ Crypto.StandardCrypto
  => Ledger.ShelleyEraTxCert (Api.ShelleyLedgerEra era)
  => Ledger.TxCert (Api.ShelleyLedgerEra era) ~ Ledger.ShelleyTxCert (Api.ShelleyLedgerEra era)
  => Ledger.ShelleyTxCert (Api.ShelleyLedgerEra era)
  -> Maybe (Api.StakeCredential, Coin)
getShelleyDeregistrationInfo cert = do
  case cert of
    Ledger.UnRegTxCert stakeCred -> Just (Api.fromShelleyStakeCredential stakeCred, 0)
    _ -> Nothing

getConwayDeregistrationInfo
  :: Ledger.EraCrypto (Api.ShelleyLedgerEra era) ~ Crypto.StandardCrypto
  => Ledger.TxCert (Api.ShelleyLedgerEra era) ~ Ledger.ConwayTxCert (Api.ShelleyLedgerEra era)
  => Ledger.ConwayEraTxCert (Api.ShelleyLedgerEra era)
  => Ledger.ConwayTxCert (Api.ShelleyLedgerEra era)
  -> Maybe (Api.StakeCredential, Coin)
getConwayDeregistrationInfo cert = do
  case cert of
    Ledger.UnRegDepositTxCert stakeCred depositRefund -> Just (Api.fromShelleyStakeCredential stakeCred, depositRefund)
    _ -> Nothing

getExecutionUnitPrices :: Api.CardanoEra era -> Api.LedgerProtocolParameters era -> Maybe Ledger.Prices
getExecutionUnitPrices cEra (Api.LedgerProtocolParameters pp) =
  Api.forEraInEonMaybe cEra $ \aeo ->
    Api.alonzoEraOnwardsConstraints aeo $
      pp ^. Ledger.ppPricesL

runTransactionBuildRawCmd
  :: ()
  => Cmd.TransactionBuildRawCmdArgs era
  -> Api.ExceptT CLI.TxCmdError IO ()
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
    , currentTreasuryValueAndDonation
    , txBodyOutFile
    } = do
    inputsAndMaybeScriptWits <-
      Api.firstExceptT CLI.TxCmdScriptWitnessError $
        CLI.readScriptWitnessFiles eon txIns
    certFilesAndMaybeScriptWits <-
      Api.firstExceptT CLI.TxCmdScriptWitnessError $
        CLI.readScriptWitnessFiles eon certificates

    withdrawalsAndMaybeScriptWits <-
      Api.firstExceptT CLI.TxCmdScriptWitnessError $
        CLI.readScriptWitnessFilesTuple eon withdrawals
    txMetadata <-
      Api.firstExceptT CLI.TxCmdMetadataError
        . Api.newExceptT
        $ CLI.readTxMetadata eon metadataSchema metadataFiles
    valuesWithScriptWits <- readValueScriptWitnesses eon $ Maybe.fromMaybe mempty mValue
    scripts <-
      Api.firstExceptT CLI.TxCmdScriptFileError $
        mapM (CLI.readFileScriptInAnyLang . Api.unFile) scriptFiles
    txAuxScripts <-
      Api.hoistEither $ Arrow.left CLI.TxCmdAuxScriptsValidationError $ CLI.validateTxAuxScripts eon scripts

    pparams <- Monad.forM mProtocolParamsFile $ \ppf ->
      Api.firstExceptT CLI.TxCmdProtocolParamsError (CLI.readProtocolParameters eon ppf)

    let mLedgerPParams = Api.LedgerProtocolParameters <$> pparams

    txUpdateProposal <- case mUpdateProprosalFile of
      Just (Api.Featured w (Just updateProposalFile)) ->
        CLI.readTxUpdateProposal w updateProposalFile & Api.firstExceptT CLI.TxCmdReadTextViewFileError
      _ -> pure Api.TxUpdateProposalNone

    requiredSigners <-
      mapM (Api.firstExceptT CLI.TxCmdRequiredSignerError . Api.newExceptT . CLI.readRequiredSigner) reqSigners

    mReturnCollateral <- mapM (toTxOutInShelleyBasedEra eon) mReturnColl

    txOuts <- mapM (toTxOutInAnyEra eon) txouts

    -- the same collateral input can be used for several plutus scripts
    let filteredTxinsc = Set.toList $ Set.fromList txInsCollateral

    -- Conway related
    votingProceduresAndMaybeScriptWits <-
      Api.inEonForShelleyBasedEra
        (pure mempty)
        ( \w ->
            Api.firstExceptT CLI.TxCmdVoteError . Api.ExceptT $
              Api.conwayEraOnwardsConstraints w $
                CLI.readVotingProceduresFiles w voteFiles
        )
        eon

    proposals <-
      Api.lift (CLI.readTxGovernanceActions eon proposalFiles)
        & Api.onLeft (Api.left . CLI.TxCmdProposalError)

    certsAndMaybeScriptWits <-
      Api.shelleyBasedEraConstraints eon $
        sequence
          [ fmap
              (,mSwit)
              ( Api.firstExceptT CLI.TxCmdReadTextViewFileError . Api.newExceptT $
                  Api.readFileTextEnvelope Api.AsCertificate (Api.File certFile)
              )
          | (CLI.CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]
    txBody <-
      Api.hoistEither $
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

    let noWitTx = Api.makeSignedTransaction [] txBody
    Api.lift (Api.writeTxFileTextEnvelopeCddl eon txBodyOutFile noWitTx)
      & Api.onLeft (Api.left . CLI.TxCmdWriteFileError)

runTxBuildRaw
  :: ()
  => Api.ShelleyBasedEra era
  -> Maybe Api.ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(Api.TxIn, Maybe (Api.ScriptWitness Api.WitCtxTxIn era))]
  -- ^ TxIn with potential script witness
  -> [Api.TxIn]
  -- ^ Read only reference inputs
  -> [Api.TxIn]
  -- ^ Api.TxIn for collateral
  -> Maybe (Api.TxOut Api.CtxTx era)
  -- ^ Return collateral
  -> Maybe Coin
  -- ^ Total collateral
  -> [Api.TxOut Api.CtxTx era]
  -> Maybe Api.SlotNo
  -- ^ Tx lower bound
  -> Api.TxValidityUpperBound era
  -- ^ Tx upper bound
  -> Coin
  -- ^ Tx fee
  -> (Api.Value, [Api.ScriptWitness Api.WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> [(Api.Certificate era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(Api.StakeAddress, Coin, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> [Api.Hash Api.PaymentKey]
  -- ^ Required signers
  -> Api.TxAuxScripts era
  -> Api.TxMetadataInEra era
  -> Maybe (Api.LedgerProtocolParameters era)
  -> Api.TxUpdateProposal era
  -> [(Api.VotingProcedures era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> [(Api.Proposal era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> Maybe (Api.TxCurrentTreasuryValue, CLI.TxTreasuryDonation)
  -> Either CLI.TxCmdError (Api.TxBody era)
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
        (Api.unLedgerProtocolParameters <$> mpparams)
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

    Arrow.left CLI.TxCmdTxBodyError $ Api.createAndValidateTransactionBody sbe txBodyContent


constructTxBodyContent
  :: forall era
   . Api.ShelleyBasedEra era
  -> Maybe Api.ScriptValidity
  -> Maybe (Ledger.PParams (Api.ShelleyLedgerEra era))
  -> [(Api.TxIn, Maybe (Api.ScriptWitness Api.WitCtxTxIn era))]
  -- ^ TxIn with potential script witness
  -> [Api.TxIn]
  -- ^ Read only reference inputs
  -> [Api.TxIn]
  -- ^ TxIn for collateral
  -> Maybe (Api.TxOut Api.CtxTx era)
  -- ^ Return collateral
  -> Maybe Api.Lovelace
  -- ^ Total collateral
  -> [Api.TxOut Api.CtxTx era]
  -- ^ Normal outputs
  -> Maybe Api.SlotNo
  -- ^ Tx lower bound
  -> Api.TxValidityUpperBound era
  -- ^ Tx upper bound
  -> (Api.Value, [Api.ScriptWitness Api.WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> [(Api.Certificate era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(Api.StakeAddress, Api.Lovelace, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -- ^ Withdrawals
  -> [Api.Hash Api.PaymentKey]
  -- ^ Required signers
  -> Api.Lovelace
  -- ^ Tx fee
  -> Api.TxAuxScripts era
  -> Api.TxMetadataInEra era
  -> Api.TxUpdateProposal era
  -> [(Api.VotingProcedures era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> [(Api.Proposal era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> Maybe (Api.TxCurrentTreasuryValue, CLI.TxTreasuryDonation)
  -- ^ The current treasury value and the donation. This is a stop gap as the
  -- semantics of the donation and treasury value depend on the script languages
  -- being used.
  -> Either CLI.TxCmdError (Api.TxBodyContent Api.BuildTx era)
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
        Arrow.left CLI.TxCmdNotSupportedInEraValidationError $ CLI.validateTxTotalCollateral sbe mTotCollateral
      validatedRetCol <-
        Arrow.left CLI.TxCmdNotSupportedInEraValidationError $ CLI.validateTxReturnCollateral sbe mReturnCollateral
      let txFee = Api.TxFeeExplicit sbe fee
      validatedLowerBound <-
        Arrow.left CLI.TxCmdNotSupportedInEraValidationError $ CLI.validateTxValidityLowerBound sbe mLowerBound
      validatedReqSigners <-
        Arrow.left CLI.TxCmdNotSupportedInEraValidationError $ CLI.validateRequiredSigners sbe reqSigners
      validatedMintValue <- createTxMintValue sbe valuesWithScriptWits
      validatedTxScriptValidity <-
        Arrow.left CLI.TxCmdNotSupportedInEraValidationError $ CLI.validateTxScriptValidity sbe mScriptValidity
      validatedVotingProcedures <-
        Arrow.left (CLI.TxCmdTxGovDuplicateVotes . CLI.TxGovDuplicateVotes) $
          Api.mkTxVotingProcedures @Api.BuildTx votingProcedures -- was: (Map.fromList votingProcedures)
      let txProposals = Api.forShelleyBasedEraInEonMaybe sbe $ \w -> do
            let txp :: Api.TxProposalProcedures Api.BuildTx era
                txp = Api.conwayEraOnwardsConstraints w $ Api.mkTxProposalProcedures $ map (Arrow.first Api.unProposal) proposals
            Api.Featured w txp
      validatedCurrentTreasuryValue <-
        Arrow.left
          CLI.TxCmdNotSupportedInEraValidationError
          (CLI.validateTxCurrentTreasuryValue sbe (fst <$> mCurrentTreasuryValueAndDonation))
      validatedTreasuryDonation <-
        Arrow.left
          CLI.TxCmdNotSupportedInEraValidationError
          (CLI.validateTxTreasuryDonation sbe (snd <$> mCurrentTreasuryValueAndDonation))
      return $
        Api.shelleyBasedEraConstraints
          sbe
          ( Api.defaultTxBodyContent sbe
              & Api.setTxIns (validateTxIns inputsAndMaybeScriptWits)
              & Api.setTxInsCollateral validatedCollateralTxIns
              & Api.setTxInsReference validatedRefInputs
              & Api.setTxOuts txouts
              & Api.setTxTotalCollateral validatedTotCollateral
              & Api.setTxReturnCollateral validatedRetCol
              & Api.setTxFee txFee
              & Api.setTxValidityLowerBound validatedLowerBound
              & Api.setTxValidityUpperBound mUpperBound
              & Api.setTxMetadata txMetadata
              & Api.setTxAuxScripts txAuxScripts
              & Api.setTxExtraKeyWits validatedReqSigners
              & Api.setTxProtocolParams (Api.BuildTxWith $ Api.LedgerProtocolParameters <$> mPparams)
              & Api.setTxWithdrawals (Api.TxWithdrawals sbe $ map convertWithdrawals withdrawals)
              & Api.setTxCertificates (convertCertificates sbe certsAndMaybeScriptWits)
              & Api.setTxUpdateProposal txUpdateProposal
              & Api.setTxMintValue validatedMintValue
              & Api.setTxScriptValidity validatedTxScriptValidity
              & Api.setTxVotingProcedures (Api.mkFeatured validatedVotingProcedures)
              & Api.setTxProposalProcedures txProposals
              & Api.setTxCurrentTreasuryValue validatedCurrentTreasuryValue
              & Api.setTxTreasuryDonation validatedTreasuryDonation
          )
   where
    convertWithdrawals
      :: (Api.StakeAddress, Api.Lovelace, Maybe (Api.ScriptWitness Api.WitCtxStake era))
      -> (Api.StakeAddress, Api.Lovelace, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxStake era))
    convertWithdrawals (sAddr, ll, mScriptWitnessFiles) =
      case mScriptWitnessFiles of
        Just sWit -> (sAddr, ll, Api.BuildTxWith $ Api.ScriptWitness Api.ScriptWitnessForStakeAddr sWit)
        Nothing -> (sAddr, ll, Api.BuildTxWith $ Api.KeyWitness Api.KeyWitnessForStakeAddr)

runTxBuild
  :: ()
  => Api.ShelleyBasedEra era
  -> Api.SocketPath
  -> Api.NetworkId
  -> Maybe Api.ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(Api.TxIn, Maybe (Api.ScriptWitness Api.WitCtxTxIn era))]
  -- ^ Read only reference inputs
  -> [Api.TxIn]
  -- ^ TxIn with potential script witness
  -> [Api.TxIn]
  -- ^ TxIn for collateral
  -> Maybe (Api.TxOut Api.CtxTx era)
  -- ^ Return collateral
  -> Maybe Coin
  -- ^ Total collateral
  -> [Api.TxOut Api.CtxTx era]
  -- ^ Normal outputs
  -> CLI.TxOutChangeAddress
  -- ^ A change output
  -> (Api.Value, [Api.ScriptWitness Api.WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> Maybe Api.SlotNo
  -- ^ Tx lower bound
  -> Api.TxValidityUpperBound era
  -- ^ Tx upper bound
  -> [(Api.Certificate era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(Api.StakeAddress, Coin, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> [Api.Hash Api.PaymentKey]
  -- ^ Required signers
  -> Api.TxAuxScripts era
  -> Api.TxMetadataInEra era
  -> Api.TxUpdateProposal era
  -> Maybe Word
  -> [(Api.VotingProcedures era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> [(Api.Proposal era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> Maybe (Api.TxCurrentTreasuryValue, CLI.TxTreasuryDonation)
  -- ^ The current treasury value and the donation.
  -> Api.ExceptT CLI.TxCmdError IO (Api.BalancedTxBody era)
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
  (CLI.TxOutChangeAddress changeAddr)
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
    Api.shelleyBasedEraConstraints sbe $ do
      -- TODO: All functions should be parameterized by ShelleyBasedEra
      -- as it's not possible to call this function with ByronEra
      let era = Api.toCardanoEra sbe
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
            Api.LocalNodeConnectInfo
              { localConsensusModeParams = Api.CardanoModeParams $ Api.EpochSlots 21600
              , localNodeNetworkId = networkId
              , localNodeSocketPath = socketPath
              }

      Api.AnyCardanoEra nodeEra <-
        Api.lift (Api.executeLocalStateQueryExpr localNodeConnInfo Consensus.VolatileTip Api.queryCurrentEra)
          & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . Api.AcqFailure)
          & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . Api.QceUnsupportedNtcVersion)

      Equality.Refl <-
        Equality.testEquality era nodeEra
          & Api.hoistMaybe (CLI.TxCmdTxNodeEraMismatchError $ NodeEraMismatchError era nodeEra)

      let certs =
            case convertCertificates sbe certsAndMaybeScriptWits of
              Api.TxCertificates _ cs _ -> cs
              _ -> []

       -- use readProtocolParametersOrDie from ApiTest.hs
       -- also protocolParamMaxBlockExUnits and protocolParamMaxTxExUnits
      (txEraUtxo, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits, drepDelegDeposits, _) <-
        Api.lift
          ( Api.executeLocalStateQueryExpr localNodeConnInfo Consensus.VolatileTip $
              Api.queryStateForBalancedTx nodeEra allTxInputs certs
          )
          & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . Api.AcqFailure)
          & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError)

      txBodyContent <-
        Api.hoistEither $
          constructTxBodyContent
            sbe
            mScriptValidity
            (Just $ Api.unLedgerProtocolParameters pparams)
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

      Api.firstExceptT CLI.TxCmdTxInsDoNotExist
        . Api.hoistEither
        $ Api.txInsExistInUTxO allTxInputs txEraUtxo
      Api.firstExceptT CLI.TxCmdQueryNotScriptLocked
        . Api.hoistEither
        $ Api.notScriptLockedTxIns txinsc txEraUtxo

      cAddr <-
        pure (Api.anyAddressInEra era changeAddr)
          & Api.onLeft (error $ "runTxBuild: Byron address used: " <> show changeAddr) -- should this throw instead?
      balancedTxBody@(Api.BalancedTxBody _ _ _ fee) <-
        Api.firstExceptT (CLI.TxCmdBalanceTxBody . AnyTxBodyErrorAutoBalance)
          . Api.hoistEither
          $ Api.makeTransactionBodyAutoBalance
            sbe
            systemStart
            (Api.toLedgerEpochInfo eraHistory)
            pparams
            stakePools
            stakeDelegDeposits
            drepDelegDeposits
            txEraUtxo
            txBodyContent
            cAddr
            mOverrideWits

      Api.liftIO $ putStrLn $ "Estimated transaction fee: " <> (show fee :: String)

      return balancedTxBody

convertCertificates
  :: ()
  => Api.ShelleyBasedEra era
  -> [(Api.Certificate era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> Api.TxCertificates Api.BuildTx era
convertCertificates sbe certsAndScriptWitnesses =
  Api.TxCertificates sbe certs $ Api.BuildTxWith reqWits
 where
  certs = map fst certsAndScriptWitnesses
  reqWits = Maybe.mapMaybe convert certsAndScriptWitnesses
  convert
    :: (Api.Certificate era, Maybe (Api.ScriptWitness Api.WitCtxStake era))
    -> Maybe (Api.StakeCredential, Api.Witness Api.WitCtxStake era)
  convert (cert, mScriptWitnessFiles) = do
    sCred <- Api.selectStakeCredentialWitness cert
    Just $ case mScriptWitnessFiles of
      Just sWit -> (sCred, Api.ScriptWitness Api.ScriptWitnessForStakeAddr sWit)
      Nothing -> (sCred, Api.KeyWitness Api.KeyWitnessForStakeAddr)


-- ----------------------------------------------------------------------------
-- Transaction body validation and conversion
--

txFeatureMismatch
  :: ()
  => Monad m
  => Api.CardanoEra era
  -> CLI.TxFeature
  -> Api.ExceptT CLI.TxCmdError m a
txFeatureMismatch era feature =
  Api.hoistEither . Left $ CLI.TxCmdTxFeatureMismatch (Api.anyCardanoEra era) feature

txFeatureMismatchPure
  :: Api.CardanoEra era
  -> CLI.TxFeature
  -> Either CLI.TxCmdError a
txFeatureMismatchPure era feature =
  Left (CLI.TxCmdTxFeatureMismatch (Api.anyCardanoEra era) feature)

validateTxIns
  :: [(Api.TxIn, Maybe (Api.ScriptWitness Api.WitCtxTxIn era))]
  -> [(Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn era))]
validateTxIns = map convert
 where
  convert
    :: (Api.TxIn, Maybe (Api.ScriptWitness Api.WitCtxTxIn era))
    -> (Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn era))
  convert (txin, mScriptWitness) =
    case mScriptWitness of
      Just sWit ->
        (txin, Api.BuildTxWith $ Api.ScriptWitness Api.ScriptWitnessForSpending sWit)
      Nothing ->
        (txin, Api.BuildTxWith $ Api.KeyWitness Api.KeyWitnessForSpending)

validateTxInsCollateral
  :: Api.ShelleyBasedEra era
  -> [Api.TxIn]
  -> Either CLI.TxCmdError (Api.TxInsCollateral era)
validateTxInsCollateral _ [] = return Api.TxInsCollateralNone
validateTxInsCollateral era txins = do
  Api.forShelleyBasedEraInEonMaybe era (`Api.TxInsCollateral` txins)
    & maybe (txFeatureMismatchPure (Api.toCardanoEra era) CLI.TxFeatureCollateral) Right

validateTxInsReference
  :: Api.ShelleyBasedEra era
  -> [Api.TxIn]
  -> Either CLI.TxCmdError (Api.TxInsReference era)
validateTxInsReference _ [] = return Api.TxInsReferenceNone
validateTxInsReference sbe allRefIns = do
  Api.forShelleyBasedEraInEonMaybe sbe (`Api.TxInsReference` allRefIns)
    & maybe (txFeatureMismatchPure (Api.toCardanoEra sbe) CLI.TxFeatureReferenceInputs) Right

getAllReferenceInputs
  :: [(Api.TxIn, Maybe (Api.ScriptWitness Api.WitCtxTxIn era))]
  -> [Api.ScriptWitness Api.WitCtxMint era]
  -> [(Api.Certificate era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> [(Api.StakeAddress, Coin, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> [(Api.VotingProcedures era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> [(Api.Proposal era, Maybe (Api.ScriptWitness Api.WitCtxStake era))]
  -> [Api.TxIn]
  -- ^ Read only reference inputs
  -> [Api.TxIn]
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
      :: Api.ScriptWitness witctx era
      -> Maybe Api.TxIn
    getReferenceInput sWit =
      case sWit of
        Api.PlutusScriptWitness _ _ (Api.PReferenceScript refIn _) _ _ _ -> Just refIn
        Api.PlutusScriptWitness _ _ Api.PScript{} _ _ _ -> Nothing
        Api.SimpleScriptWitness _ (Api.SReferenceScript refIn _) -> Just refIn
        Api.SimpleScriptWitness _ Api.SScript{} -> Nothing

toAddressInAnyEra
  :: Api.CardanoEra era
  -> Api.AddressAny
  -> Either CLI.TxCmdError (Api.AddressInEra era)
toAddressInAnyEra era addrAny = Api.runExcept $ do
  case addrAny of
    Api.AddressByron bAddr -> pure (Api.AddressInEra Api.ByronAddressInAnyEra bAddr)
    Api.AddressShelley sAddr -> do
      sbe <-
        Api.requireShelleyBasedEra era
          & Api.onNothing (txFeatureMismatch era CLI.TxFeatureShelleyAddresses)

      pure (Api.AddressInEra (Api.ShelleyAddressInEra sbe) sAddr)

toAddressInShelleyBasedEra
  :: Api.ShelleyBasedEra era
  -> Api.Address Api.ShelleyAddr
  -> Either CLI.TxCmdError (Api.AddressInEra era)
toAddressInShelleyBasedEra sbe sAddr =
  Api.runExcept $
    pure (Api.AddressInEra (Api.ShelleyAddressInEra sbe) sAddr)

toTxOutValueInAnyEra
  :: Api.ShelleyBasedEra era
  -> Api.Value
  -> Either CLI.TxCmdError (Api.TxOutValue era)
toTxOutValueInAnyEra era val =
  Api.caseShelleyToAllegraOrMaryEraOnwards
    ( \_ -> case Api.valueToLovelace val of
        Just l -> return (Api.TxOutValueShelleyBased era l)
        Nothing -> txFeatureMismatchPure (Api.toCardanoEra era) CLI.TxFeatureMultiAssetOutputs
    )
    (\w -> return (Api.TxOutValueShelleyBased era (Api.toLedgerValue w val)))
    era

toTxOutValueInShelleyBasedEra
  :: Api.ShelleyBasedEra era
  -> Api.Value
  -> Either CLI.TxCmdError (Api.TxOutValue era)
toTxOutValueInShelleyBasedEra sbe val =
  Api.caseShelleyToAllegraOrMaryEraOnwards
    ( \_ -> case Api.valueToLovelace val of
        Just l -> return (Api.TxOutValueShelleyBased sbe l)
        Nothing -> txFeatureMismatchPure (Api.toCardanoEra sbe) CLI.TxFeatureMultiAssetOutputs
    )
    (\w -> return (Api.TxOutValueShelleyBased sbe (Api.toLedgerValue w val)))
    sbe

toTxOutInShelleyBasedEra
  :: Api.ShelleyBasedEra era
  -> CLI.TxOutShelleyBasedEra
  -> Api.ExceptT CLI.TxCmdError IO (Api.TxOut Api.CtxTx era)
toTxOutInShelleyBasedEra era (CLI.TxOutShelleyBasedEra addr' val' mDatumHash refScriptFp) = do
  addr <- Api.hoistEither $ toAddressInShelleyBasedEra era addr'
  val <- Api.hoistEither $ toTxOutValueInShelleyBasedEra era val'

  datum <-
    Api.caseShelleyToMaryOrAlonzoEraOnwards
      (const (pure Api.TxOutDatumNone))
      (`toTxAlonzoDatum` mDatumHash)
      era

  refScript <-
    Api.inEonForEra
      (pure Api.ReferenceScriptNone)
      (`getReferenceScript` refScriptFp)
      (Api.toCardanoEra era)

  pure $ Api.TxOut addr val datum refScript

toTxOutByronEra
  :: CLI.TxOutAnyEra
  -> Api.ExceptT CLI.TxCmdError IO (Api.TxOut Api.CtxTx Api.ByronEra)
toTxOutByronEra (CLI.TxOutAnyEra addr' val' _ _) = do
  addr <- Api.hoistEither $ toAddressInAnyEra Api.ByronEra addr'
  let ada = Api.TxOutValueByron $ Api.selectLovelace val'
  pure $ Api.TxOut addr ada Api.TxOutDatumNone Api.ReferenceScriptNone

-- TODO: toTxOutInAnyEra eventually will not be needed because
-- byron related functionality will be treated
-- separately
toTxOutInAnyEra
  :: Api.ShelleyBasedEra era
  -> CLI.TxOutAnyEra
  -> Api.ExceptT CLI.TxCmdError IO (Api.TxOut Api.CtxTx era)
toTxOutInAnyEra era (CLI.TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  let cEra = Api.toCardanoEra era
  addr <- Api.hoistEither $ toAddressInAnyEra cEra addr'
  val <- Api.hoistEither $ toTxOutValueInAnyEra era val'

  datum <-
    Api.caseShelleyToMaryOrAlonzoEraOnwards
      (const (pure Api.TxOutDatumNone))
      (`toTxAlonzoDatum` mDatumHash)
      era

  refScript <-
    Api.caseShelleyToAlonzoOrBabbageEraOnwards
      (const (pure Api.ReferenceScriptNone))
      (`getReferenceScript` refScriptFp)
      era
  pure $ Api.TxOut addr val datum refScript

getReferenceScript
  :: ()
  => Api.BabbageEraOnwards era
  -> CLI.ReferenceScriptAnyEra
  -> Api.ExceptT CLI.TxCmdError IO (Api.ReferenceScript era)
getReferenceScript w = \case
  CLI.ReferenceScriptAnyEraNone -> return Api.ReferenceScriptNone
  CLI.ReferenceScriptAnyEra fp -> Api.ReferenceScript w <$> Api.firstExceptT CLI.TxCmdScriptFileError (CLI.readFileScriptInAnyLang fp)

toTxAlonzoDatum
  :: ()
  => Api.AlonzoEraOnwards era
  -> CLI.TxOutDatumAnyEra
  -> Api.ExceptT CLI.TxCmdError IO (Api.TxOutDatum Api.CtxTx era)
toTxAlonzoDatum supp cliDatum =
  case cliDatum of
    CLI.TxOutDatumByNone -> pure Api.TxOutDatumNone
    CLI.TxOutDatumByHashOnly h -> pure (Api.TxOutDatumHash supp h)
    CLI.TxOutDatumByHashOf sDataOrFile -> do
      sData <- Api.firstExceptT CLI.TxCmdScriptDataError $ CLI.readScriptDataOrFile sDataOrFile
      pure (Api.TxOutDatumHash supp $ Api.hashScriptDataBytes sData)
    CLI.TxOutDatumByValue sDataOrFile -> do
      sData <- Api.firstExceptT CLI.TxCmdScriptDataError $ CLI.readScriptDataOrFile sDataOrFile
      pure (Api.TxOutDatumInTx supp sData)
    CLI.TxOutInlineDatumByValue sDataOrFile -> do
      let cEra = Api.toCardanoEra supp
      Api.forEraInEon cEra (txFeatureMismatch cEra CLI.TxFeatureInlineDatums) $ \babbageOnwards -> do
        sData <- Api.firstExceptT CLI.TxCmdScriptDataError $ CLI.readScriptDataOrFile sDataOrFile
        pure $ Api.TxOutDatumInline babbageOnwards sData

-- TODO: Currently we specify the policyId with the '--mint' option on the cli
-- and we added a separate '--policy-id' parser that parses the policy id for the
-- given reference input (since we don't have the script in this case). To avoid asking
-- for the policy id twice (in the build command) we can potentially query the UTxO and
-- access the script (and therefore the policy id).
createTxMintValue
  :: forall era
   . Api.ShelleyBasedEra era
  -> (Api.Value, [Api.ScriptWitness Api.WitCtxMint era])
  -> Either CLI.TxCmdError (Api.TxMintValue Api.BuildTx era)
createTxMintValue era (val, scriptWitnesses) =
  if List.null (Api.valueToList val) && List.null scriptWitnesses
    then return Api.TxMintNone
    else do
      Api.caseShelleyToAllegraOrMaryEraOnwards
        (const (txFeatureMismatchPure (Api.toCardanoEra era) CLI.TxFeatureMintValue))
        ( \w -> do
            -- The set of policy ids for which we need witnesses:
            let witnessesNeededSet :: Set Api.PolicyId
                witnessesNeededSet =
                  Set.fromList [pid | (Api.AssetId pid _, _) <- Api.valueToList val]

            let witnessesProvidedMap :: Map Api.PolicyId (Api.ScriptWitness Api.WitCtxMint era)
                witnessesProvidedMap =  Map.fromList $ gatherMintingWitnesses scriptWitnesses
                witnessesProvidedSet =  Map.keysSet witnessesProvidedMap

            -- Check not too many, nor too few:
            validateAllWitnessesProvided witnessesNeededSet witnessesProvidedSet
            validateNoUnnecessaryWitnesses witnessesNeededSet witnessesProvidedSet
            return (Api.TxMintValue w val (Api.BuildTxWith witnessesProvidedMap))
        )
        era
 where
  gatherMintingWitnesses
    :: [Api.ScriptWitness Api.WitCtxMint era]
    -> [(Api.PolicyId, Api.ScriptWitness Api.WitCtxMint era)]
  gatherMintingWitnesses [] = []
  gatherMintingWitnesses (sWit : rest) =
    case scriptWitnessPolicyId sWit of
      Nothing -> gatherMintingWitnesses rest
      Just pid -> (pid, sWit) : gatherMintingWitnesses rest

  validateAllWitnessesProvided witnessesNeeded witnessesProvided
    | null witnessesMissing = return ()
    | otherwise = Left (CLI.TxCmdPolicyIdsMissing witnessesMissing (Set.toList witnessesProvided))
   where
    witnessesMissing = Set.elems (witnessesNeeded Set.\\ witnessesProvided)

  validateNoUnnecessaryWitnesses witnessesNeeded witnessesProvided
    | null witnessesExtra = return ()
    | otherwise = Left (CLI.TxCmdPolicyIdsExcess witnessesExtra)
   where
    witnessesExtra = Set.elems (witnessesProvided Set.\\ witnessesNeeded)

scriptWitnessPolicyId :: forall witctx era . ()
  => Api.ScriptWitness witctx era -> Maybe Api.PolicyId
scriptWitnessPolicyId (Api.SimpleScriptWitness _ (Api.SScript script)) =
  Just . Api.scriptPolicyId $ Api.SimpleScript script
scriptWitnessPolicyId (Api.SimpleScriptWitness _ (Api.SReferenceScript _ mPid)) =
  Api.PolicyId <$> mPid
scriptWitnessPolicyId (Api.PlutusScriptWitness _ plutusVersion (Api.PScript script) _ _ _) =
  Just . Api.scriptPolicyId $ Api.PlutusScript plutusVersion script
scriptWitnessPolicyId (Api.PlutusScriptWitness _ _ (Api.PReferenceScript _ mPid) _ _ _) =
  Api.PolicyId <$> mPid

readValueScriptWitnesses
  :: Api.ShelleyBasedEra era
  -> (Api.Value, [CLI.ScriptWitnessFiles Api.WitCtxMint])
  -> Api.ExceptT CLI.TxCmdError IO (Api.Value, [Api.ScriptWitness Api.WitCtxMint era])
readValueScriptWitnesses era (v, sWitFiles) = do
  sWits <- mapM (Api.firstExceptT CLI.TxCmdScriptWitnessError . CLI.readScriptWitness era) sWitFiles
  return (v, sWits)

-- ----------------------------------------------------------------------------
-- Transaction signing
--

runTransactionSignCmd
  :: ()
  => Cmd.TransactionSignCmdArgs
  -> Api.ExceptT CLI.TxCmdError IO ()
runTransactionSignCmd
  Cmd.TransactionSignCmdArgs
    { txOrTxBodyFile = txOrTxBody
    , witnessSigningData = witnessSigningData
    , mNetworkId = mNetworkId
    , outTxFile = outTxFile
    } = do
    sks <- Monad.forM witnessSigningData $ \d ->
      Api.lift (CLI.readWitnessSigningData d)
        & Api.onLeft (Api.left . CLI.TxCmdReadWitnessSigningDataError)

    let (sksByron, sksShelley) = partitionSomeWitnesses $ map CLI.categoriseSomeSigningWitness sks

    case txOrTxBody of
      CLI.InputTxFile (Api.File inputTxFilePath) -> do
        inputTxFile <- Api.liftIO $ CLI.fileOrPipe inputTxFilePath
        anyTx <- Api.lift (CLI.readFileTx inputTxFile) & Api.onLeft (Api.left . CLI.TxCmdTextEnvCddlError)

        Api.InAnyShelleyBasedEra sbe tx <- pure anyTx

        let (txbody, existingTxKeyWits) = Api.getTxBodyAndWitnesses tx

        byronWitnesses <-
          pure (mkShelleyBootstrapWitnesses sbe mNetworkId txbody sksByron)
            & Api.onLeft (Api.left . CLI.TxCmdBootstrapWitnessError)

        let newShelleyKeyWits = map (Api.makeShelleyKeyWitness sbe txbody) sksShelley
            allKeyWits = existingTxKeyWits ++ newShelleyKeyWits ++ byronWitnesses
            signedTx = Api.makeSignedTransaction allKeyWits txbody

        Api.lift (Api.writeTxFileTextEnvelopeCddl sbe outTxFile signedTx)
          & Api.onLeft (Api.left . CLI.TxCmdWriteFileError)
      CLI.InputTxBodyFile (Api.File txbodyFilePath) -> do
        txbodyFile <- Api.liftIO $ CLI.fileOrPipe txbodyFilePath
        CLI.IncompleteCddlTxBody { .. } <-
          Api.firstExceptT CLI.TxCmdTextEnvCddlError . Api.newExceptT $
            CLI.readFileTxBody txbodyFile

        case unIncompleteCddlTxBody of
          Api.InAnyShelleyBasedEra sbe txbody -> do

            -- Byron witnesses require the network ID. This can either be provided
            -- directly or derived from a provided Byron address.
            byronWitnesses <-
              Api.firstExceptT CLI.TxCmdBootstrapWitnessError
                . Api.hoistEither
                $ mkShelleyBootstrapWitnesses sbe mNetworkId txbody sksByron

            let shelleyKeyWitnesses = map (Api.makeShelleyKeyWitness sbe txbody) sksShelley
                tx = Api.makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

            Api.lift (Api.writeTxFileTextEnvelopeCddl sbe outTxFile tx)
              & Api.onLeft (Api.left . CLI.TxCmdWriteFileError)

-- ----------------------------------------------------------------------------
-- Transaction submission
--

runTransactionSubmitCmd
  :: ()
  => Cmd.TransactionSubmitCmdArgs
  -> Api.ExceptT CLI.TxCmdError IO ()
runTransactionSubmitCmd
  Cmd.TransactionSubmitCmdArgs
    { nodeSocketPath
    , consensusModeParams
    , networkId
    , txFile
    } = do
    txFileOrPipe <- Api.liftIO $ CLI.fileOrPipe txFile
    Api.InAnyShelleyBasedEra era tx <-
      Api.lift (CLI.readFileTx txFileOrPipe) & Api.onLeft (Api.left . CLI.TxCmdTextEnvCddlError)
    let txInMode = Api.TxInMode era tx
        localNodeConnInfo =
          Api.LocalNodeConnectInfo
            { localConsensusModeParams = consensusModeParams
            , localNodeNetworkId = networkId
            , localNodeSocketPath = nodeSocketPath
            }

    res <- Api.liftIO $ Api.submitTxToNodeLocal localNodeConnInfo txInMode
    case res of
      Net.Tx.SubmitSuccess -> Api.liftIO $ Text.putStrLn "Transaction successfully submitted."
      Net.Tx.SubmitFail reason ->
        case reason of
          Api.TxValidationErrorInCardanoMode err -> Api.left . CLI.TxCmdTxSubmitError . Text.pack $ show err
          Api.TxValidationEraMismatch mismatchErr -> Api.left $ CLI.TxCmdTxSubmitErrorEraMismatch mismatchErr

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTransactionCalculateMinFeeCmd
  :: ()
  => Cmd.TransactionCalculateMinFeeCmdArgs
  -> Api.ExceptT CLI.TxCmdError IO ()
runTransactionCalculateMinFeeCmd
  Cmd.TransactionCalculateMinFeeCmdArgs
    { txBodyFile = Api.File txbodyFilePath
    , protocolParamsFile = protocolParamsFile
    , txShelleyWitnessCount = CLI.TxShelleyWitnessCount nShelleyKeyWitnesses
    , txByronWitnessCount = CLI.TxByronWitnessCount nByronKeyWitnesses
    , referenceScriptSize = CLI.ReferenceScriptSize sReferenceScript
    , outputFormat
    , outFile
    } = do
    txbodyFile <- Api.liftIO $ CLI.fileOrPipe txbodyFilePath
    CLI.IncompleteCddlTxBody { .. } <-
      Api.firstExceptT CLI.TxCmdTextEnvCddlError . Api.newExceptT $
        CLI.readFileTxBody txbodyFile

    let nShelleyKeyWitW32 = fromIntegral nShelleyKeyWitnesses

    Api.InAnyShelleyBasedEra sbe txbody <- pure unIncompleteCddlTxBody

    lpparams <-
      Api.firstExceptT CLI.TxCmdProtocolParamsError $
        CLI.readProtocolParameters sbe protocolParamsFile

    let shelleyfee = Api.evaluateTransactionFee sbe lpparams txbody nShelleyKeyWitW32 0 sReferenceScript

    let byronfee =
          Api.shelleyBasedEraConstraints sbe $
            calculateByronWitnessFees (lpparams ^. Ledger.ppMinFeeAL) nByronKeyWitnesses

    let Coin fee = shelleyfee + byronfee
        textToWrite = Text.pack ((show fee :: String) <> " Lovelace")
        jsonToWrite = Aeson.encodePretty $ Aeson.object ["fee" .= fee]

    -- The Query prefix should go away whenever I can get to actually
    -- link against the new cardano-cli.
    -- This whole code block can't really be used until the update works.
    -- What used to be there before the above was:
    Api.liftIO $ Text.putStrLn textToWrite
    case (CLI.newOutputFormat outputFormat outFile, outFile) of
      (CLI.OutputFormatText, Nothing) ->
        Api.liftIO $ Text.putStrLn textToWrite
      (CLI.OutputFormatText, Just file) ->
        Api.firstExceptT CLI.TxCmdWriteFileError . Api.newExceptT $ Api.writeTextFile file textToWrite
      (CLI.OutputFormatJson, Nothing) ->
        Api.liftIO $ LBS8.putStrLn jsonToWrite
      (CLI.OutputFormatJson, Just file) ->
        Api.firstExceptT CLI.TxCmdWriteFileError . Api.newExceptT $ Api.writeLazyByteStringFile file jsonToWrite

-- Extra logic to handle byron witnesses.
-- TODO: move this to Cardano.API.Fee.Api.evaluateTransactionFee.
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
  -> Api.ExceptT CLI.TxCmdError IO ()
runTransactionCalculateMinValueCmd
  Cmd.TransactionCalculateMinValueCmdArgs
    { eon
    , protocolParamsFile
    , txOut
    } = do
    pp <- Api.firstExceptT CLI.TxCmdProtocolParamsError (CLI.readProtocolParameters eon protocolParamsFile)
    out <- toTxOutInShelleyBasedEra eon txOut

    let minValue = Api.calculateMinimumUTxO eon out pp
    Api.liftIO . IO.print $ minValue

runTransactionPolicyIdCmd
  :: ()
  => Cmd.TransactionPolicyIdCmdArgs
  -> Api.ExceptT CLI.TxCmdError IO ()
runTransactionPolicyIdCmd
  Cmd.TransactionPolicyIdCmdArgs
    { scriptFile = Api.File sFile
    } = do
    Api.ScriptInAnyLang _ script <-
      Api.firstExceptT CLI.TxCmdScriptFileError $
        CLI.readFileScriptInAnyLang sFile
    Api.liftIO . Text.putStrLn . Api.serialiseToRawBytesHexText $ Api.hashScript script

partitionSomeWitnesses
  :: [CLI.ByronOrShelleyWitness]
  -> ( [CLI.ShelleyBootstrapWitnessSigningKeyData]
     , [Api.ShelleyWitnessSigningKey]
     )
partitionSomeWitnesses = reversePartitionedWits . List.foldl' go mempty
 where
  reversePartitionedWits (bw, skw) =
    (reverse bw, reverse skw)

  go (byronAcc, shelleyKeyAcc) byronOrShelleyWit =
    case byronOrShelleyWit of
      CLI.AByronWitness byronWit ->
        (byronWit : byronAcc, shelleyKeyAcc)
      CLI.AShelleyKeyWitness shelleyKeyWit ->
        (byronAcc, shelleyKeyWit : shelleyKeyAcc)

-- | Construct a Shelley bootstrap witness (i.e. a Byron key witness in the
-- Shelley era).
mkShelleyBootstrapWitness
  :: ()
  => Api.ShelleyBasedEra era
  -> Maybe Api.NetworkId
  -> Api.TxBody era
  -> CLI.ShelleyBootstrapWitnessSigningKeyData
  -> Either BootstrapWitnessError (Api.KeyWitness era)
mkShelleyBootstrapWitness _ Nothing _ (CLI.ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrByronAddressError
mkShelleyBootstrapWitness sbe (Just nw) txBody (CLI.ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ Api.makeShelleyBootstrapWitness sbe (Byron.WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness sbe _ txBody (CLI.ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ Api.makeShelleyBootstrapWitness sbe (Byron.WitnessByronAddress addr) txBody skey

-- | Attempt to construct Shelley bootstrap witnesses until an error is
-- encountered.
mkShelleyBootstrapWitnesses
  :: ()
  => Api.ShelleyBasedEra era
  -> Maybe Api.NetworkId
  -> Api.TxBody era
  -> [CLI.ShelleyBootstrapWitnessSigningKeyData]
  -> Either BootstrapWitnessError [Api.KeyWitness era]
mkShelleyBootstrapWitnesses sbe mnw txBody =
  mapM (mkShelleyBootstrapWitness sbe mnw txBody)

-- ----------------------------------------------------------------------------
-- Other misc small commands
--

runTransactionHashScriptDataCmd
  :: ()
  => Cmd.TransactionHashScriptDataCmdArgs
  -> Api.ExceptT CLI.TxCmdError IO ()
runTransactionHashScriptDataCmd
  Cmd.TransactionHashScriptDataCmdArgs
    { scriptDataOrFile
    } = do
    d <- Api.firstExceptT CLI.TxCmdScriptDataError $ CLI.readScriptDataOrFile scriptDataOrFile
    Api.liftIO $ BS8.putStrLn $ Api.serialiseToRawBytesHex (Api.hashScriptDataBytes d)

runTransactionTxIdCmd
  :: ()
  => Cmd.TransactionTxIdCmdArgs
  -> Api.ExceptT CLI.TxCmdError IO ()
runTransactionTxIdCmd
  Cmd.TransactionTxIdCmdArgs
    { inputTxBodyOrTxFile
    } = do
    Api.InAnyShelleyBasedEra _era txbody <-
      case inputTxBodyOrTxFile of
        CLI.InputTxBodyFile (Api.File txbodyFilePath) -> do
          txbodyFile <- Api.liftIO $ CLI.fileOrPipe txbodyFilePath
          CLI.IncompleteCddlTxBody { .. } <-
            Api.firstExceptT CLI.TxCmdTextEnvCddlError . Api.newExceptT $
              CLI.readFileTxBody txbodyFile
          pure unIncompleteCddlTxBody
        CLI.InputTxFile (Api.File txFilePath) -> do
          txFile <- Api.liftIO $ CLI.fileOrPipe txFilePath
          Api.InAnyShelleyBasedEra era tx <- Api.lift (CLI.readFileTx txFile) & Api.onLeft (Api.left . CLI.TxCmdTextEnvCddlError)
          return . Api.InAnyShelleyBasedEra era $ Api.getTxBody tx

    Api.liftIO $ BS8.putStrLn $ Api.serialiseToRawBytesHex (Api.getTxId txbody)

runTransactionViewCmd
  :: ()
  => Cmd.TransactionViewCmdArgs
  -> Api.ExceptT CLI.TxCmdError IO ()
runTransactionViewCmd
  Cmd.TransactionViewCmdArgs =
    Api.liftIO $ do
      IO.hPutStrLn
        IO.stderr
        "Command \"era transaction view\" has been removed. Please use \"debug transaction view\" instead."
      Exit.exitWith (Exit.ExitFailure 1)

-- ----------------------------------------------------------------------------
-- Witness commands
--

runTransactionWitnessCmd
  :: ()
  => Cmd.TransactionWitnessCmdArgs
  -> Api.ExceptT CLI.TxCmdError IO ()
runTransactionWitnessCmd
  Cmd.TransactionWitnessCmdArgs
    { txBodyFile = Api.File txbodyFilePath
    , witnessSigningData
    , mNetworkId
    , outFile
    } = do
    txbodyFile <- Api.liftIO $ CLI.fileOrPipe txbodyFilePath
    CLI.IncompleteCddlTxBody { unIncompleteCddlTxBody = Api.InAnyShelleyBasedEra sbe txbody } <-
      Api.firstExceptT CLI.TxCmdTextEnvCddlError . Api.newExceptT $
        CLI.readFileTxBody txbodyFile
    someWit <-
      Api.firstExceptT CLI.TxCmdReadWitnessSigningDataError
        . Api.newExceptT
        $ CLI.readWitnessSigningData witnessSigningData
    witness <-
      case CLI.categoriseSomeSigningWitness someWit :: CLI.ByronOrShelleyWitness of
        -- Byron witnesses require the network ID. This can either be provided
        -- directly or derived from a provided Byron address.
        CLI.AByronWitness bootstrapWitData ->
          Api.firstExceptT CLI.TxCmdBootstrapWitnessError
            . Api.hoistEither
            $ mkShelleyBootstrapWitness sbe mNetworkId txbody bootstrapWitData
        CLI.AShelleyKeyWitness skShelley ->
          pure $ Api.makeShelleyKeyWitness sbe txbody skShelley

    Api.firstExceptT CLI.TxCmdWriteFileError . Api.newExceptT $
      Api.writeTxWitnessFileTextEnvelopeCddl sbe outFile witness

runTransactionSignWitnessCmd
  :: ()
  => Cmd.TransactionSignWitnessCmdArgs
  -> Api.ExceptT CLI.TxCmdError IO ()
runTransactionSignWitnessCmd
  Cmd.TransactionSignWitnessCmdArgs
    { txBodyFile = Api.File txbodyFilePath
    , witnessFiles = witnessFiles
    , outFile = outFile
    } = do
    txbodyFile <- Api.liftIO $ CLI.fileOrPipe txbodyFilePath
    CLI.IncompleteCddlTxBody { unIncompleteCddlTxBody = Api.InAnyShelleyBasedEra era txbody } <- Api.lift (CLI.readFileTxBody txbodyFile) & Api.onLeft (Api.left . CLI.TxCmdTextEnvCddlError)
    -- TODO: Left off here. Remember we were never reading byron key witnesses anyways!
    witnesses <-
      sequence
        [ do
            Api.InAnyShelleyBasedEra era' witness <-
              Api.lift (CLI.readFileTxKeyWitness file) & Api.onLeft (Api.left . CLI.TxCmdCddlWitnessError)

            case Equality.testEquality era era' of
              Nothing ->
                Api.left $
                  CLI.TxCmdWitnessEraMismatch
                    (Api.AnyCardanoEra $ Api.toCardanoEra era)
                    (Api.AnyCardanoEra $ Api.toCardanoEra era')
                    witnessFile
              Just Equality.Refl -> return witness
        | witnessFile@(CLI.WitnessFile file) <- witnessFiles
        ]

    let tx = Api.makeSignedTransaction witnesses txbody

    Api.lift (Api.writeTxFileTextEnvelopeCddl era outFile tx) & Api.onLeft (Api.left . CLI.TxCmdWriteFileError)
