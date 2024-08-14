{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
 -* OPTIONS_GHC -Wno-unrecognised-pragmas          *-
 -}

-- Okay, this one is too useful to leave unset while coding.
{-# OPTIONS_GHC -Wno-error=partial-type-signatures #-}

-- To get by for the moment.
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

module  Cardano.TxGenerator.GovExample where

-- import           Cardano.Api hiding (StakeAddress, StakeCredential)
import qualified Cardano.Api as Api
                   ( CardanoEra (..)
                   , Error (..)
                   -- export not in CHaP yet? , IsConwayBasedEra (..)
                   , IsCardanoEra (..)
                   , NetworkId (..)
                   , TxMetadataInEra (..))
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
import           Cardano.Api.Shelley
                   ( Address (..)
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
                   , CardanoEra (..)
                   , Certificate (..)
                   , ConsensusModeParams (..)
                   , GovernanceAction (..)
                   , EpochSlots (..)
                   , ExceptT (..)
                   , Featured (..)
                   , File (..)
                   , Hash (..)
                   , InAnyCardanoEra (..)
                   , InAnyShelleyBasedEra (..)
                   , KeyWitnessInCtx (..)
                   , LedgerProtocolParameters (..)
                   , LocalNodeConnectInfo (..)
                   , NetworkId (..)
                   , PlutusScriptOrReferenceInput (..)
                   , PolicyId (..)
                   , PoolId
                   , Proposal (..)
                   , QueryConvenienceError (..)
                   , Script (..)
                   , ScriptInAnyLang (..)
                   , ScriptValidity (..)
                   , ScriptWitness (..)
                   , ScriptWitnessInCtx (..)
                   , ShelleyAddr
                   , ShelleyLedgerEra
                   , ShelleyWitnessSigningKey (..)
                   , SimpleScriptOrReferenceInput (..)
                   , SlotNo (..)
                   , SocketPath
                   , StakeAddress (..)
                   , StakeCredential (..)
                   , TextEnvelope (..)
                   , TextEnvelopeType (..)
                   , Value
                   , Vote (..)
                   , VotingProcedures (..)
                   , WitCtxMint
                   , WitCtxStake
                   , WitCtxTxIn)
import qualified Cardano.Api.Shelley as Api
                   ( BabbageEraOnwards (..)
                   , BuildTx
                   , BuildTxWith (..)
                   , ConwayEra
                   , ConwayEraOnwards (..)
                   , CtxTx
                   , IsBabbageBasedEra (..)
                   , IsShelleyBasedEra (..)
                   , IsConwayBasedEra (..)
                   , KeyWitness (..)
                   , ProtocolParameters (..)
                   , ReferenceScript (..)
                   , ShelleyBasedEra (..)
                   , type ShelleyEra
                   , ShelleyLedgerEra
                   , Tx (..)
                   , TxAuxScripts (..)
                   , TxBody (..)
                   , TxBodyContent (..)
                   , TxBodyError (..)
                   , TxCertificates (..)
                   , TxCurrentTreasuryValue (..)
                   , TxFee (..)
                   , TxId (..)
                   , TxIn (..)
                   , TxInMode (..)
                   , TxInsReference (..)
                   , TxInsCollateral (..)
                   , TxIx (..)
                   , TxMintValue (..)
                   , TxOut (..)
                   , TxOutDatum (..)
                   , TxOutValue (..)
                   , TxUpdateProposal (..)
                   , TxValidationErrorInCardanoMode (..)
                   , TxValidityLowerBound (..)
                   , TxValidityUpperBound (..)
                   , TxWithdrawals (..)
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
                   , convertToLedgerProtocolParameters
                   , conwayEraOnwardsConstraints
                   , conwayEraOnwardsToShelleyBasedEra
                   , createAndValidateTransactionBody
                   , createVotingProcedure
                   , defaultTxBodyContent
                   , defaultTxValidityUpperBound
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
                   , lovelaceToTxOutValue
                   , makeShelleyBootstrapWitness
                   , makeShelleyKeyWitness
                   , makeSignedTransaction
                   , makeTransactionBodyAutoBalance
                   , maryEraOnwardsToShelleyBasedEra
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
                   , setTxProtocolParams
                   , setTxScriptValidity
                   , setTxReturnCollateral
                   , setTxTotalCollateral
                   , setTxTreasuryDonation
                   , setTxUpdateProposal
                   , setTxValidityLowerBound
                   , setTxValidityUpperBound
                   , setTxWithdrawals
                   , shelleyBasedEra
                   , shelleyBasedEraConstraints
                   , shelleyToBabbageEraConstraints
                   , signShelleyTransaction
                   , submitTxToNodeLocal
                   , toCardanoEra
                   , toLedgerEpochInfo
                   , toLedgerValue
                   , txInsExistInUTxO
                   , unFeatured
                   , valueToList
                   , valueToLovelace
                   , writeTxFileTextEnvelopeCddl
                   , writeTxWitnessFileTextEnvelopeCddl)

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
import           Cardano.CLI.Json.Friendly
                   (FriendlyFormat (..))
import qualified Cardano.CLI.Json.Friendly as CLI (friendlyTx , friendlyTxBody)
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
import qualified Cardano.CLI.EraBased.Run.Genesis.Common as CLI
                    (readProtocolParameters)
import qualified Cardano.CLI.Types.Common as CLI
                   ( TxBuildOutputOptions (..)
                   , TxByronWitnessCount (..)
                   , TxOutAnyEra (..)
                   , TxOutChangeAddress (..)
                   , TxOutDatumAnyEra (..)
                   , TxOutShelleyBasedEra (..)
                   , TxShelleyWitnessCount (..)
                   , TxTreasuryDonation (..))
import           Cardano.CLI.Types.Common
                   ( CertificateFile (..)
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
                   , ViewOutputFormat (..)
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
                   ( convertToTxVotingProcedures
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
import qualified Cardano.CLI.Types.TxFeature as CLI (TxFeature (..))
import qualified Cardano.Ledger.Api.PParams as Ledger
                 ( EraPParams (..)
                 , ppMinFeeAL
                 )
import qualified Cardano.Ledger.Api.Tx.Body as Ledger
                  (EraTxBody (..))
import qualified Cardano.Ledger.Api.Tx.Cert as Ledger
                  (pattern RetirePoolTxCert)
import           Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..), Url)
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Ledger
                 ( Era (..)
                 , EraTx (..)
                 , EraTxBody (..)
                 , PreviousEra
                 , downgradePParams
                 , upgradePParams)
import qualified Cardano.Ledger.Api.Era as Ledger
                 ( BabbageEra
                 , ConwayEra
                 , ShelleyEra)
import           Cardano.Ledger.Crypto -- exports only types and classes
import qualified Cardano.Ledger.Crypto as Crypto
                 (Crypto (..))
import           Cardano.TxGenerator.FundQueue (Fund (..), FundInEra (..), FundQueue)
import qualified Cardano.TxGenerator.FundQueue as FundQueue
                   ( emptyFundQueue
                   , getFundCoin
                   , getFundKey
                   , getFundTxIn
                   , getFundWitness
                   , insertFund
                   , toList)
import           Cardano.TxGenerator.Setup.SigningKey
                   ( PaymentKey
                   , SigningKey)
import qualified Cardano.TxGenerator.Setup.SigningKey as TxGen
                    (parseSigningKeyTE)
import           Cardano.TxGenerator.Types (FundSource
                   , FundToStoreList, TxEnvironment (..)
                   , TxGenError (..), TxGenerator)
import qualified Cardano.TxGenerator.Utils as TxGen
                    (inputsToOutputsWithFee)
import           Cardano.TxGenerator.UTxO (ToUTxO, ToUTxOList, makeToUTxOList, mkUTxOVariant)

import           Control.Arrow ((&&&))
import qualified Control.Arrow as Arrow (first, left, right)
import qualified Control.Monad as Monad (foldM, forM)
import           Control.Monad.Trans.State.Strict
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson (eitherDecodeFileStrict', object)
import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Default.Class as Default
                  (Default (..))
import           Data.Either (fromRight)
import           Data.Function ((&))
import           Data.Functor.Identity (Identity (..))
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
import           Data.Void (Void)
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


{-
demo :: IO ()
demo = getDataFileName "data/protocol-parameters.json" >>= demo'

demo' :: FilePath -> IO ()
demo' parametersFile = do
  protocolParameters <- either die pure =<< Aeson.eitherDecodeFileStrict' parametersFile
  let
      demoEnv :: TxEnvironment Api.ConwayEra
      demoEnv = TxEnvironment {
          txEnvNetworkId = Api.Mainnet
        , txEnvProtocolParams = protocolParameters
        , txEnvFee = Api.TxFeeExplicit Api.ShelleyBasedEraConway 100000
        , txEnvMetadata = Api.TxMetadataNone
        }

  run1 <- Monad.foldM (worker $ generateTx demoEnv) (FundQueue.emptyFundQueue `FundQueue.insertFund` genesisFund) [1..10]
  run2 <- Monad.foldM (worker $ generateTxM demoEnv) (FundQueue.emptyFundQueue `FundQueue.insertFund` genesisFund) [1..10]
  putStrLn $ "Are run results identical? " ++ show (FundQueue.toList run1 == FundQueue.toList run2)
  where
    worker ::
         Generator (Either TxGenError (Api.Tx Api.ConwayEra))
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
-}

signingKey :: SigningKey PaymentKey
signingKey = fromRight (error "signingKey: parseError") $ TxGen.parseSigningKeyTE keyData
  where
    keyData = TextEnvelope { teType = TextEnvelopeType "GenesisUTxOSigningKey_ed25519"
              , teDescription = fromString "Genesis Initial UTxO Signing Key"
              , teRawCBOR = "X \vl1~\182\201v(\152\250A\202\157h0\ETX\248h\153\171\SI/m\186\242D\228\NAK\182(&\162"}

drepSigningKey :: SigningKey PaymentKey
drepSigningKey = fromRight (error "drepSigningKey: parseError") $ TxGen.parseSigningKeyTE keyData
  where
    keyData = TextEnvelope { teType = TextEnvelopeType "DRepSigningKey_ed25519"
                           , teDescription = fromString "Delegate Representative Signing Key"
                           -- This is actually the CBOR as it appeared
                           -- in the JSON file and needs conversion to raw CBOR.
                           , teRawCBOR = "5820ac0757312cf883baa809d8cf6c3c48e86acc70db9c6eb5511666c8b128d9020a" }

genesisTxIn :: Api.TxIn
genesisValue :: Api.TxOutValue Api.ConwayEra

(genesisTxIn, genesisValue) =
  ( Api.TxIn "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162" (Api.TxIx 0)
  , Api.lovelaceToTxOutValue Api.ShelleyBasedEraConway $ Coin 90000000000000
  )

genesisFund :: Fund
genesisFund
  = Fund $ InAnyCardanoEra Api.ConwayEra fundInEra
  where
    fundInEra :: FundInEra Api.ConwayEra
    fundInEra  = FundInEra {
        _fundTxIn = genesisTxIn
      , _fundVal = genesisValue
      , _fundWitness = Api.KeyWitness KeyWitnessForSpending
      , _fundSigningKey = Just signingKey
      }

type Generator = State FundQueue

-- Need to ask Carlos or Aniket what anchors are about.
-- The particular issue is what could be substituted for the URL if it
-- turns out fake ones like I used earlier error out.
-- Cardano.Api.Governance.Actions.createAnchor
--         :: Url -> ByteString -> Anchor StandardCrypto
-- There's a lingering undefined here, but it's worth keeping because
-- this is at least meant to push directly towards issuing votes.
localGenVote :: forall era .
  {- ConwayEraOnwardsConstraints era => -}
     Api.ConwayEraOnwards era
  -> Vote
  -> IO ()
localGenVote era vote = do
  let _procedure = Api.createVotingProcedure
                             (era {- eon -} :: Api.ConwayEraOnwards era)
                             (vote {- votingChoice -} :: Vote)
                             (Nothing :: Maybe (Url, Text))
  _ <- Api.shelleyBasedEraConstraints localShelleyBasedEra do
    _ <- pure (undefined :: AnyVotingStakeVerificationKeyOrHashOrFile)
    pure undefined
  pure ()
  where
    localShelleyBasedEra = Api.conwayEraOnwardsToShelleyBasedEra era

{-
mkSignedTx :: forall shelleyBasedEra
                     -- era
                     -- shelleyEra conwayEra
                     -- _stubEra1 _stubEra2 .
                     shelleyBasedConwayEra
                     shelleyBasedShelleyEra
                     .
  (
  --   shelleyBasedEra ~ Api.ShelleyBasedEra era
  -- , shelleyBasedShelleyEra ~ Api.ShelleyBasedEra shelleyEra
  -- , shelleyBasedConwayEra ~ Api.ShelleyBasedEra conwayEra
  )
  -- This is a very suspicious constraint.
  -- => Api.IsShelleyBasedEra era
  => Api.IsShelleyBasedEra shelleyBasedEra
  => Api.IsShelleyBasedEra shelleyBasedShelleyEra
  => Api.IsShelleyBasedEra shelleyBasedConwayEra
  => shelleyBasedEra
  -> LedgerProtocolParameters _ -- _stubEra1
  -> [Fund] -- inFunds
  -> (Api.TxInsCollateral _ {- _stubEra2 -}, [Fund]) -- (collateral, collFunds)
  -> Api.TxFee shelleyBasedEra -- fee
  -> Api.TxMetadataInEra shelleyBasedEra -- metadata
  -> [Api.TxOut Api.CtxTx shelleyBasedEra] -- outputs
  -> Either TxGenError (Api.Tx shelleyBasedEra, Api.TxId)
-}
mkSignedTx :: forall
                  shelleyBasedEra
            . ()
  => Api.IsShelleyBasedEra shelleyBasedEra
  => _
  -> _
  -> _
  -> _
  -> _
  -> _
  -> _
  -> Either _ (Api.Tx shelleyBasedEra, Api.TxId)
mkSignedTx
     shelleyBasedEra
     ledgerParameters
     inFunds
     (collateral, collFunds)
     fee
     metadata
     outputs = undefined
{-
     outputs = case shelleyBasedEra of
  Api.ShelleyBasedEraAllegra -> eraErr "Allegra"
  Api.ShelleyBasedEraAlonzo -> eraErr "Alonzo"
  Api.ShelleyBasedEraBabbage -> eraErr "Babbage"
  Api.ShelleyBasedEraMary -> eraErr "Mary"
  Api.ShelleyBasedEraConway ->
    case mkTxBody shelleyBasedEra
               ledgerParameters
               inFunds
               (collateral, collFunds)
               fee
               metadata
               outputs of
        Left err -> Left $ ApiError err
        Right body ->
          Right (Api.signShelleyTransaction (Api.shelleyBasedEra {- @shelleyBasedConwayEra-}) body signingKeys, Api.getTxId body)
  Api.ShelleyBasedEraShelley ->
    case mkTxBody shelleyBasedEra
               ledgerParameters
               inFunds
               (collateral, collFunds)
               fee
               metadata
               outputs of
        Left err -> Left $ ApiError err
        Right body ->
          Right (Api.signShelleyTransaction (Api.shelleyBasedEra {- @shelleyBasedShelleyEra -}) body signingKeys, Api.getTxId body)
-}
  where
    eraErr eraStr = error $ "mkTxBody: unexpected era " <> eraStr
    signingKeys :: [ShelleyWitnessSigningKey]
    signingKeys = map WitnessPaymentKey allKeys
    allKeys :: [SigningKey PaymentKey]
    allKeys = Maybe.mapMaybe FundQueue.getFundKey $ inFunds ++ collFunds

mkTxBody :: forall -- shelleyBasedConwayEra shelleyBasedShelleyEra
                   -- conwayEra shelleyEra
                   shelleyBasedEra era .
  ( shelleyBasedEra ~ Api.ShelleyBasedEra era
  -- , Api.ShelleyBasedEra conwayEra ~ shelleyBasedConwayEra
  -- , shelleyBasedShelleyEra ~ Api.ShelleyBasedEra Api.ShelleyEra
  )
  => Api.IsCardanoEra era
  => Api.IsShelleyBasedEra shelleyBasedEra
  -- => Api.IsShelleyBasedEra shelleyBasedConwayEra
  -- => Api.IsShelleyBasedEra shelleyBasedShelleyEra
  => shelleyBasedEra
  -> LedgerProtocolParameters era
  -> [Fund] -- inFunds
  -> (Api.TxInsCollateral era, _) -- (collateral, collFunds)
  -> Api.TxFee era -- fee
  -> Api.TxMetadataInEra era -- metadata
  -> [Api.TxOut Api.CtxTx era] -- outputs
  -> Either Api.TxBodyError (Api.TxBody era)
mkTxBody
     shelleyBasedEra
     ledgerParameters
     inFunds
     (collateral, collFunds)
     fee
     metadata
     outputs = Api.inEonForEra (eraErr "Unsupported era")
       (\eonEra -> Api.createAndValidateTransactionBody eonEra $
         mkTxBodyContent eonEra
                         ledgerParameters
                         inFunds
                         (collateral, collFunds)
                         fee
                         metadata
                         outputs)
       (Api.toCardanoEra shelleyBasedEra)
  where
    -- shelleyEra = Api.toCardanoEra shelleyBasedEra
    eraErr eraStr = error $ "mkTxBody: unexpected era " <> eraStr

upgradeLedgerPParams :: forall crypto {- functor -} . ()
  -- => Functor functor
  => Crypto crypto
  => Default.Default (Ledger.UpgradePParams Identity (Ledger.ConwayEra crypto))
  => Ledger.PParams (Ledger.BabbageEra crypto)
  -> Ledger.PParams (Ledger.ConwayEra crypto)
upgradeLedgerPParams ledgerParams =
  Ledger.upgradePParams (Default.def :: Ledger.UpgradePParams Identity (Ledger.ConwayEra crypto)) ledgerParams

-- forall era . Api.ShelleyBasedEra _ -> Api.TxBodyContent Api.BuildTx era
{-
mkTxBodyContent :: forall era shelleyBasedEra .
  ( Api.ShelleyBasedEra era ~ shelleyBasedEra
  )
  -- This is a very suspicious constraint.
  -- => Api.IsShelleyBasedEra era
  => Api.IsShelleyBasedEra shelleyBasedEra
  => shelleyBasedEra
  -> LedgerProtocolParameters _
  -> [Fund] -- inFunds
  -> (Api.TxInsCollateral _, _) -- (collateral, collFunds)
  -> Api.TxFee _ -- fee
  -> Api.TxMetadataInEra _ -- metadata
  -> [Api.TxOut Api.CtxTx _] -- outputs
  -> Api.TxBodyContent Api.BuildTx _
-}
mkTxBodyContent :: forall era . ()
  => Api.IsCardanoEra era
  => Api.ShelleyBasedEra era
  -> LedgerProtocolParameters era
  -> [Fund] -- inFunds
  -> (Api.TxInsCollateral era, _) -- (collateral, collFunds)
  -> Api.TxFee era -- fee
  -> Api.TxMetadataInEra era -- metadata
  -> [Api.TxOut Api.CtxTx era] -- outputs
  -> Api.TxBodyContent Api.BuildTx era
mkTxBodyContent
     shelleyBasedEra
     ledgerParameters
     inFunds
     (collateral, collFunds)
     fee
     metadata
     outputs =
  Api.defaultTxBodyContent shelleyBasedEra
    -- & Api.setTxIns (map (\f -> (FundQueue.getFundTxIn f, Api.BuildTxWith $ FundQueue.getFundWitness f)) inFunds)
    -- & Api.setTxIns (map (FundQueue.getFundTxIn &&& (Api.BuildTxWith . FundQueue.getFundWitness) [(FundQueue.getFundTxIn f, Api.BuildTxWith $ FundQueue.getFundWitness f)) inFunds)
    -- & Api.setTxIns [(FundQueue.getFundTxIn f, Api.BuildTxWith $ FundQueue.getFundWitness f) | f <- inFunds]
    & Api.setTxIns (map getTxIn inFunds)
    & Api.setTxInsCollateral collateral
    & Api.setTxOuts outputs -- (map downgradeTxOut outputs)
    & Api.setTxFee fee
    & Api.setTxValidityLowerBound Api.TxValidityNoLowerBound
    & Api.setTxValidityUpperBound (Api.defaultTxValidityUpperBound shelleyBasedEra)
    & Api.setTxMetadata metadata
    & Api.setTxProtocolParams (Api.BuildTxWith $ Just ledgerParameters)

-- getTxIn :: Fund -> (Api.TxIn, Api.Witness WitCtxTxIn era)
getTxIn :: forall shelleyBasedEra . ()
  => Api.IsCardanoEra shelleyBasedEra
  => Fund
  -> (Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness WitCtxTxIn shelleyBasedEra))
getTxIn = FundQueue.getFundTxIn &&& Api.BuildTxWith . FundQueue.getFundWitness

-- runTxBuildRaw from CLI
localGenTx :: forall era. ()
  => Ledger.EraTx (Ledger.PreviousEra (ShelleyLedgerEra era))
  => Ledger.EraTx (ShelleyLedgerEra era)
  => Api.IsConwayBasedEra era
  => Api.IsShelleyBasedEra era
  => Api.ConwayEraOnwards era
  -> LedgerProtocolParameters era
  -> (Api.TxInsCollateral era, [Fund])
  -> Api.TxFee era
  -> Api.TxMetadataInEra era
  -- -> TxGenerator era
  -> [Fund]
  -> [Api.TxOut Api.CtxTx era]
  -> Either TxGenError (Api.Tx era, Api.TxId)
localGenTx ceo ledgerParameters (collateral, collFunds) fee metadata inFunds outputs
  = case txBody of
      Left err -> Left $ ApiError err
      Right txBody' ->
        case signTxBody txBody' of
          (signedBody, txId) ->
            case upgradeTx signedBody of
              Left err -> Left $ (((undefined :: _ -> TxGenError) err) :: TxGenError) -- Left . TxGenError $ show _err
              Right upgradedBody -> Right (upgradedBody, txId)
 -- signShelleyTransaction :: ShelleyBasedEra era -> TxBody era -> [ShelleyWitnessSigningKey] -> Tx era
 -- createAndValidateTransactionBody :: ShelleyBasedEra era -> TxBodyContent Api.BuildTx era -> Either TxBodyError (Tx era)
 where
  sbe :: Api.ShelleyBasedEra tyVar10
  sbe = Api.conwayEraOnwardsToShelleyBasedEra (undefined {- Api.ConwayEraOnwardsConway -} :: eraVar1 {- Api.ConwayEraOnwards era -})
  signingKeys :: [ShelleyWitnessSigningKey]
  signingKeys = map WitnessPaymentKey allKeys
  signTxBody :: Api.TxBody signEra -> (Api.Tx signEra, Api.TxId)
  signTxBody body =
   (Api.signShelleyTransaction sbe body signingKeys, Api.getTxId body)
  downgradedProtocolParameters :: LedgerProtocolParameters tyVar9
  downgradedProtocolParameters = undefined {- LedgerProtocolParameters
    . Ledger.downgradePParams (undefined :: _ {- Void -})
    $ unLedgerProtocolParameters ledgerParameters -}
  allKeys :: [SigningKey PaymentKey]
  allKeys = Maybe.mapMaybe FundQueue.getFundKey $ inFunds ++ collFunds
  {- upgradeTx :: (Ledger.Tx (ShelleyLedgerEra tyVar8)
                  ~ Ledger.Tx (Ledger.PreviousEra (ShelleyLedgerEra tyVar8))
               , tyVar8 ~ era)
    -- => Ledger.EraTx (Ledger.PreviousEra (ShelleyLedgerEra tyVar8))
    => Ledger.EraTx (Ledger.PreviousEra (ShelleyLedgerEra tyVar8))
    => Ledger.EraTx (ShelleyLedgerEra tyVar8)
    => Api.Tx tyVar8 -> Either (Ledger.TxUpgradeError (ShelleyLedgerEra tyVar8) {- Ledger.TxUpgradeError era -}) (Api.Tx tyVar8 {- tyVar2 -} {- Api.Tx era -}) -}
  upgradeTx :: Api.Tx tyVar8 -> Either d (Api.Tx tyVar8)
  upgradeTx (Api.ShelleyTx shelleyEra (shelleyTx :: Ledger.Tx (ShelleyLedgerEra tyVar8))) =
    Arrow.right (Api.ShelleyTx shelleyEra) $ undefined -- Ledger.upgradeTx undefined -- shelleyTx
    -- undefined -- Arrow.right (Api.ShelleyTx shelleyEra) $ Ledger.upgradeTx shelleyTx
  downgradedMetadata :: Api.TxMetadataInEra tyVar7
  downgradedMetadata = case metadata of
    Api.TxMetadataNone -> Api.TxMetadataNone
    Api.TxMetadataInEra _ txmd -> Api.TxMetadataInEra (undefined :: tyVar6) txmd
  downgradeReferenceScript :: Api.IsBabbageBasedEra beo => Api.ReferenceScript beo -> Api.ReferenceScript tyVar5
  downgradeReferenceScript = \case
    Api.ReferenceScript babbageEra script -> undefined
    _ -> undefined
  {-
   case babbageEra of
     Api.BabbageEraOnwardsBabbage -> Api.ReferenceScript Api.BabbageEraOnwardsBabbage script
     Api.BabbageEraOnwardsConway  -> Api.ReferenceScript Api.BabbageEraOnwardsConway script
  -}
  downgradeTxOut :: Api.TxOut Api.CtxTx tyVar3 -> Api.TxOut Api.CtxTx era
  downgradeTxOut = undefined
  build :: Api.BuildTxWith Api.BuildTx (Maybe (LedgerProtocolParameters tyVar4))
  build = Api.BuildTxWith $ Just downgradedProtocolParameters
  txBody :: Either (Api.TxBodyError) (Api.TxBody era)
  txBody = Api.createAndValidateTransactionBody sbe txBodyContent
  txBodyContent :: Api.TxBodyContent Api.BuildTx era
  txBodyContent = Api.defaultTxBodyContent sbe
    & Api.setTxIns (map (\f -> (FundQueue.getFundTxIn f, Api.BuildTxWith $ FundQueue.getFundWitness f)) inFunds)
    & Api.setTxInsCollateral collateral
    & Api.setTxOuts (map downgradeTxOut outputs)
    & Api.setTxFee fee
    & Api.setTxValidityLowerBound Api.TxValidityNoLowerBound
    & Api.setTxValidityUpperBound (Api.defaultTxValidityUpperBound sbe)
    & Api.setTxMetadata downgradedMetadata
    & Api.setTxProtocolParams build


localSourceToStoreTransaction ::
     Monad m
  => TxGenerator era
  -> FundSource m
  -> ([Coin] -> split)
  -> ToUTxOList era split
  -> FundToStoreList m                --inline to ToUTxOList
  -> m (Either TxGenError (Api.Tx era))
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

{-
mkSignedTx' :: forall era shelleyBasedEra . ()
  => shelleyBasedEra ~ Api.ShelleyBasedEra era
  => Api.IsShelleyBasedEra shelleyBasedEra
  => LedgerProtocolParameters _
  -> (Api.TxInsCollateral shelleyBasedEra, [Fund])
  -> Api.TxFee era
  -> Api.TxMetadataInEra shelleyBasedEra
  -> [Fund]
  -> [Api.TxOut Api.CtxTx shelleyBasedEra]
  -> Either TxGenError (Api.Tx shelleyBasedEra, Api.TxId)
mkSignedTx' ledgerParameters collateralFunds fee metadata inFunds outputs =
  mkSignedTx (Api.shelleyBasedEra @shelleyBasedEra) ledgerParameters inFunds collateralFunds fee metadata outputs
-}

{-
generateTx :: forall era shelleyBasedEra .
  () -- (shelleyBasedEra ~ Api.ShelleyBasedEra era)
  => Api.IsShelleyBasedEra shelleyBasedEra
  => TxEnvironment shelleyBasedEra
  -> Generator (Either TxGenError (Api.Tx shelleyBasedEra))
generateTx TxEnvironment{..}
  = localSourceToStoreTransaction
        generator
        consumeInputFunds
        computeOutputValues
        (makeToUTxOList $ repeat computeUTxO)
        addNewOutputFunds
  where
    fee :: _
    Api.TxFeeExplicit _ fee = txEnvFee
    -- ceo :: shelleyBasedEra
    -- ceo = Api.shelleyBasedEra
    -- recall type TxGenerator era = [Fund] -> [TxOut CtxTx era] -> Either TxGenError (Tx era, TxId)
    generator :: TxGenerator shelleyBasedEra
    generator =
        case Api.convertToLedgerProtocolParameters undefined txEnvProtocolParams of
          Right ledgerParameters ->
            mkSignedTx' ledgerParameters collateralFunds txEnvFee txEnvMetadata @TxGenerator shelleyBasedEra
            -- \inFunds outputs -> mkSignedTx ceo ledgerParameters inFunds collateralFunds txEnvFee txEnvMetadata outputs
          Left err -> \_ _ -> Left (ApiError err)
      where
        -- collateralFunds are needed for Plutus transactions
        collateralFunds :: (Api.TxInsCollateral shelleyBasedEra, [Fund])
        collateralFunds = (Api.TxInsCollateralNone, [])

-- Create a transaction that uses all the available funds.
    consumeInputFunds :: Generator (Either TxGenError [Fund])
    consumeInputFunds = do
      funds <- FundQueue.toList <$> get
      put FundQueue.emptyFundQueue
      return $ Right funds

    addNewOutputFunds :: [Fund] -> Generator ()
    addNewOutputFunds = put . List.foldl' FundQueue.insertFund FundQueue.emptyFundQueue

    computeOutputValues :: [Coin] -> [Coin]
    computeOutputValues = TxGen.inputsToOutputsWithFee fee numOfOutputs
      where numOfOutputs = 2

    computeUTxO :: ToUTxO _
    computeUTxO = mkUTxOVariant txEnvNetworkId signingKey


{-
generateTxM ::
      TxEnvironment Api.ConwayEra
  ->  Generator (Either TxGenError (Api.Tx Api.ConwayEra))
generateTxM txEnv
  = do
      inFunds <- get
      case generateTxPure txEnv inFunds of
        Right (tx, outFunds)  -> put outFunds >> pure (Right tx)
        Left err              -> pure (Left err)
-}

generateTxPure :: forall era shelleyBasedEra .
  (shelleyBasedEra ~ Api.ShelleyBasedEra era)
  => Api.IsShelleyBasedEra shelleyBasedEra
  => TxEnvironment shelleyBasedEra
  -> FundQueue
  -> Either TxGenError (Api.Tx shelleyBasedEra, FundQueue)
generateTxPure TxEnvironment{..} inQueue
  = do
      (tx, txId) <- generator inputs outputs
      let outQueue = List.foldl' FundQueue.insertFund FundQueue.emptyFundQueue (toFunds txId)
      pure (tx, outQueue)
  where
    inputs = FundQueue.toList inQueue
    Api.TxFeeExplicit _ fee = txEnvFee
    -- ceo :: shelleyBasedEra
    -- ceo = Api.shelleyBasedEra
    generator :: TxGenerator shelleyBasedEra
    generator =
        case Api.convertToLedgerProtocolParameters undefined txEnvProtocolParams of
          Right ledgerParameters -> mkSignedTx' ledgerParameters collateralFunds txEnvFee txEnvMetadata
          Left err -> \_ _ -> Left (ApiError err)
      where
        -- collateralFunds are needed for Plutus transactions
        collateralFunds :: (Api.TxInsCollateral shelleyBasedEra, [Fund])
        collateralFunds = (Api.TxInsCollateralNone, [])

    outValues = computeOutputValues $ map FundQueue.getFundCoin inputs
    (outputs, toFunds) = makeToUTxOList (repeat computeUTxO) outValues

    computeOutputValues :: [Coin] -> [Coin]
    computeOutputValues = TxGen.inputsToOutputsWithFee fee numOfOutputs
      where numOfOutputs = 2

    computeUTxO :: ToUTxO _
    computeUTxO = mkUTxOVariant txEnvNetworkId signingKey
-}


{-
runTransactionCmds :: Cmd.TransactionCmds era -> ExceptT CLI.TxCmdError IO ()
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
  -> ExceptT CLI.TxCmdError IO ()
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
          LocalNodeConnectInfo
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
                Api.readFileTextEnvelope AsCertificate (File certFile)
            )
        | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
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
        mapM (CLI.readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      Api.hoistEither $ Arrow.left CLI.TxCmdAuxScriptsValidationError $ CLI.validateTxAuxScripts eon scripts

    mProp <- case mUpdateProposalFile of
      Just (Featured w (Just updateProposalFile)) ->
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
        (\w -> Api.firstExceptT CLI.TxCmdVoteError $ ExceptT (CLI.readVotingProceduresFiles w voteFiles))
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

    AnyCardanoEra nodeEra <-
      Api.lift (Api.executeLocalStateQueryExpr localNodeConnInfo Consensus.VolatileTip Api.queryCurrentEra)
        & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . AcqFailure)
        & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

    (txEraUtxo, _, eraHistory, systemStart, _, _, _, featuredCurrentTreasuryValueM) <-
      Api.lift
        ( Api.executeLocalStateQueryExpr
            localNodeConnInfo
            Consensus.VolatileTip
            (Api.queryStateForBalancedTx nodeEra allTxInputs [])
        )
        & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . AcqFailure)
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
      CLI.OutputScriptCostOnly fp -> do
        let Api.BuildTxWith mTxProtocolParams = Api.txProtocolParams txBodyContent

        pparams <- pure mTxProtocolParams & Api.onNothing (Api.left CLI.TxCmdProtocolParametersNotPresentInTxBody)
        executionUnitPrices <-
          pure (getExecutionUnitPrices era pparams) & Api.onNothing (Api.left CLI.TxCmdPParamExecutionUnitsNotAvailable)

        Refl <-
          testEquality era nodeEra
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
        Api.liftIO $ LBS8.writeFile (unFile fp) $ Aeson.encodePretty scriptCostOutput
      CLI.OutputTxBodyOnly fpath ->
        let noWitTx = Api.makeSignedTransaction [] balancedTxBody
         in Api.lift (Api.cardanoEraConstraints era $ Api.writeTxFileTextEnvelopeCddl eon fpath noWitTx)
              & Api.onLeft (Api.left . CLI.TxCmdWriteFileError)

runTransactionBuildEstimateCmd
  :: ()
  => Cmd.TransactionBuildEstimateCmdArgs era
  -> ExceptT CLI.TxCmdError IO ()
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
    -- The new field name:
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
        mapM (CLI.readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      Api.hoistEither $ Arrow.left CLI.TxCmdAuxScriptsValidationError $ CLI.validateTxAuxScripts sbe scripts

    txUpdateProposal <- case mUpdateProposalFile of
      Just (Featured w (Just updateProposalFile)) ->
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
            Api.firstExceptT CLI.TxCmdVoteError . ExceptT $
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
                  Api.readFileTextEnvelope AsCertificate (File certFile)
              )
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
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
            | (sWitIndex, AnyScriptWitness (PlutusScriptWitness _ _ _ _ _ execUnits)) <-
                Api.collectTxBodyScriptWitnesses sbe txBodyContent
            ]

    BalancedTxBody _ balancedTxBody _ _ <-
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
            (maybe 0 unReferenceScriptSize totalReferenceScriptSize)
            (Api.anyAddressInShelleyBasedEra sbe changeAddr)
            totalUTxOValue

    let noWitTx = Api.makeSignedTransaction [] balancedTxBody
    Api.lift (Api.writeTxFileTextEnvelopeCddl sbe txBodyOutFile noWitTx)
      & Api.onLeft (Api.left . CLI.TxCmdWriteFileError)

getPoolDeregistrationInfo
  :: Certificate era
  -> Maybe PoolId
getPoolDeregistrationInfo (ShelleyRelatedCertificate w cert) =
  Api.shelleyToBabbageEraConstraints w $ getShelleyDeregistrationPoolId cert
getPoolDeregistrationInfo (ConwayCertificate w cert) =
  Api.conwayEraOnwardsConstraints w $ getConwayDeregistrationPoolId cert

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
  Api.conwayEraOnwardsConstraints w $ getConwayDRepDeregistrationInfo cert

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
  Api.shelleyToBabbageEraConstraints w $ getShelleyDeregistrationInfo cert
getStakeDeregistrationInfo (ConwayCertificate w cert) =
  Api.conwayEraOnwardsConstraints w $ getConwayDeregistrationInfo cert

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
  Api.forEraInEonMaybe cEra $ \aeo ->
    Api.alonzoEraOnwardsConstraints aeo $
      pp ^. Ledger.ppPricesL

runTransactionBuildRawCmd
  :: ()
  => Cmd.TransactionBuildRawCmdArgs era
  -> ExceptT CLI.TxCmdError IO ()
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
        mapM (CLI.readFileScriptInAnyLang . unFile) scriptFiles
    txAuxScripts <-
      Api.hoistEither $ Arrow.left CLI.TxCmdAuxScriptsValidationError $ CLI.validateTxAuxScripts eon scripts

    pparams <- Monad.forM mProtocolParamsFile $ \ppf ->
      Api.firstExceptT CLI.TxCmdProtocolParamsError (CLI.readProtocolParameters eon ppf)

    let mLedgerPParams = LedgerProtocolParameters <$> pparams

    txUpdateProposal <- case mUpdateProprosalFile of
      Just (Featured w (Just updateProposalFile)) ->
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
            Api.firstExceptT CLI.TxCmdVoteError . ExceptT $
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
                  Api.readFileTextEnvelope AsCertificate (File certFile)
              )
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
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
-}

{-
runTxBuildRaw
  :: ()
  => Api.ShelleyBasedEra era
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(Api.TxIn, Maybe (Api.ScriptWitness WitCtxTxIn era))]
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
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Api.TxValidityUpperBound era
  -- ^ Tx upper bound
  -> Coin
  -- ^ Tx fee
  -> (Value, [Api.ScriptWitness WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> [(Certificate era, Maybe (Api.ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Coin, Maybe (Api.ScriptWitness WitCtxStake era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> Api.TxAuxScripts era
  -> Api.TxMetadataInEra era
  -> Maybe (LedgerProtocolParameters era)
  -> Api.TxUpdateProposal era
  -> [(VotingProcedures era, Maybe (Api.ScriptWitness WitCtxStake era))]
  -> [(Proposal era, Maybe (Api.ScriptWitness WitCtxStake era))]
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

    Arrow.left CLI.TxCmdTxBodyError $ Api.createAndValidateTransactionBody sbe txBodyContent

constructTxBodyContent
  :: Api.ShelleyBasedEra era
  -> Maybe ScriptValidity
  -> Maybe (Ledger.PParams (ShelleyLedgerEra era))
  -> [(Api.TxIn, Maybe (Api.ScriptWitness WitCtxTxIn era))]
  -- ^ TxIn with potential script witness
  -> [Api.TxIn]
  -- ^ Read only reference inputs
  -> [Api.TxIn]
  -- ^ TxIn for collateral
  -> Maybe (Api.TxOut Api.CtxTx era)
  -- ^ Return collateral
  -> Maybe Coin
  -- ^ Total collateral
  -> [Api.TxOut Api.CtxTx era]
  -- ^ Normal outputs
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Api.TxValidityUpperBound era
  -- ^ Tx upper bound
  -> (Value, [Api.ScriptWitness WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> [(Certificate era, Maybe (Api.ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Coin, Maybe (Api.ScriptWitness WitCtxStake era))]
  -- ^ Withdrawals
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> Coin
  -- ^ Tx fee
  -> Api.TxAuxScripts era
  -> Api.TxMetadataInEra era
  -> Api.TxUpdateProposal era
  -> [(VotingProcedures era, Maybe (Api.ScriptWitness WitCtxStake era))]
  -> [(Proposal era, Maybe (Api.ScriptWitness WitCtxStake era))]
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
        Arrow.left CLI.TxCmdTxGovDuplicateVotes $ CLI.convertToTxVotingProcedures votingProcedures
      validatedCurrentTreasuryValue <-
        Arrow.left
          CLI.TxCmdNotSupportedInEraValidationError
          (CLI.validateTxCurrentTreasuryValue sbe (fst <$> mCurrentTreasuryValueAndDonation))
      validatedTreasuryDonation <-
        Arrow.left
          CLI.TxCmdNotSupportedInEraValidationError
          (CLI.validateTxTreasuryDonation sbe (snd <$> mCurrentTreasuryValueAndDonation))
      return $
        Api.shelleyBasedEraConstraints sbe $
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
              & Api.setTxProtocolParams (Api.BuildTxWith $ LedgerProtocolParameters <$> mPparams)
              & Api.setTxWithdrawals (Api.TxWithdrawals sbe $ map convertWithdrawals withdrawals)
              & Api.setTxCertificates (convertCertificates sbe certsAndMaybeScriptWits)
              & Api.setTxUpdateProposal txUpdateProposal
              & Api.setTxMintValue validatedMintValue
              & Api.setTxScriptValidity validatedTxScriptValidity
          )
            { -- TODO: Create set* function for proposal procedures and voting procedures
              Api.txProposalProcedures =
                Api.forShelleyBasedEraInEonMaybe sbe (`Featured` CLI.convToTxProposalProcedures proposals)
            , Api.txVotingProcedures = Api.forShelleyBasedEraInEonMaybe sbe (`Featured` validatedVotingProcedures)
            }
            & Api.setTxCurrentTreasuryValue validatedCurrentTreasuryValue
            & Api.setTxTreasuryDonation validatedTreasuryDonation
   where
    convertWithdrawals
      :: (StakeAddress, Coin, Maybe (Api.ScriptWitness WitCtxStake era))
      -> (StakeAddress, Coin, Api.BuildTxWith Api.BuildTx (Api.Witness WitCtxStake era))
    convertWithdrawals (sAddr, ll, mScriptWitnessFiles) =
      case mScriptWitnessFiles of
        Just sWit -> (sAddr, ll, Api.BuildTxWith $ Api.ScriptWitness ScriptWitnessForStakeAddr sWit)
        Nothing -> (sAddr, ll, Api.BuildTxWith $ Api.KeyWitness KeyWitnessForStakeAddr)

runTxBuild
  :: ()
  => Api.ShelleyBasedEra era
  -> SocketPath
  -> NetworkId
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(Api.TxIn, Maybe (Api.ScriptWitness WitCtxTxIn era))]
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
  -> (Value, [Api.ScriptWitness WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Api.TxValidityUpperBound era
  -- ^ Tx upper bound
  -> [(Certificate era, Maybe (Api.ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Coin, Maybe (Api.ScriptWitness WitCtxStake era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> Api.TxAuxScripts era
  -> Api.TxMetadataInEra era
  -> Api.TxUpdateProposal era
  -> Maybe Word
  -> [(VotingProcedures era, Maybe (Api.ScriptWitness WitCtxStake era))]
  -> [(Proposal era, Maybe (Api.ScriptWitness WitCtxStake era))]
  -> Maybe (Api.TxCurrentTreasuryValue, CLI.TxTreasuryDonation)
  -- ^ The current treasury value and the donation.
  -> ExceptT CLI.TxCmdError IO (BalancedTxBody era)
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
            LocalNodeConnectInfo
              { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
              , localNodeNetworkId = networkId
              , localNodeSocketPath = socketPath
              }

      AnyCardanoEra nodeEra <-
        Api.lift (Api.executeLocalStateQueryExpr localNodeConnInfo Consensus.VolatileTip Api.queryCurrentEra)
          & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . AcqFailure)
          & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . QceUnsupportedNtcVersion)

      Refl <-
        testEquality era nodeEra
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
          & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError . AcqFailure)
          & Api.onLeft (Api.left . CLI.TxCmdQueryConvenienceError)

      txBodyContent <-
        Api.hoistEither $
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

      Api.firstExceptT CLI.TxCmdTxInsDoNotExist
        . Api.hoistEither
        $ Api.txInsExistInUTxO allTxInputs txEraUtxo
      Api.firstExceptT CLI.TxCmdQueryNotScriptLocked
        . Api.hoistEither
        $ Api.notScriptLockedTxIns txinsc txEraUtxo

      cAddr <-
        pure (Api.anyAddressInEra era changeAddr)
          & Api.onLeft (error $ "runTxBuild: Byron address used: " <> show changeAddr) -- should this throw instead?
      balancedTxBody@(BalancedTxBody _ _ _ fee) <-
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
  -> [(Certificate era, Maybe (Api.ScriptWitness WitCtxStake era))]
  -> Api.TxCertificates Api.BuildTx era
convertCertificates sbe certsAndScriptWitnesses =
  Api.TxCertificates sbe certs $ Api.BuildTxWith reqWits
 where
  certs = map fst certsAndScriptWitnesses
  reqWits = Map.fromList $ Maybe.mapMaybe convert certsAndScriptWitnesses
  convert
    :: (Certificate era, Maybe (Api.ScriptWitness WitCtxStake era))
    -> Maybe (StakeCredential, Api.Witness WitCtxStake era)
  convert (cert, mScriptWitnessFiles) = do
    sCred <- Api.selectStakeCredentialWitness cert
    Just $ case mScriptWitnessFiles of
      Just sWit -> (sCred, Api.ScriptWitness ScriptWitnessForStakeAddr sWit)
      Nothing -> (sCred, Api.KeyWitness KeyWitnessForStakeAddr)

-- ----------------------------------------------------------------------------
-- Transaction body validation and conversion
--

txFeatureMismatch
  :: ()
  => Monad m
  => CardanoEra era
  -> CLI.TxFeature
  -> ExceptT CLI.TxCmdError m a
txFeatureMismatch era feature =
  Api.hoistEither . Left $ CLI.TxCmdTxFeatureMismatch (Api.anyCardanoEra era) feature

txFeatureMismatchPure
  :: CardanoEra era
  -> CLI.TxFeature
  -> Either CLI.TxCmdError a
txFeatureMismatchPure era feature =
  Left (CLI.TxCmdTxFeatureMismatch (Api.anyCardanoEra era) feature)

validateTxIns
  :: [(Api.TxIn, Maybe (Api.ScriptWitness WitCtxTxIn era))]
  -> [(Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness WitCtxTxIn era))]
validateTxIns = map convert
 where
  convert
    :: (Api.TxIn, Maybe (Api.ScriptWitness WitCtxTxIn era))
    -> (Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness WitCtxTxIn era))
  convert (txin, mScriptWitness) =
    case mScriptWitness of
      Just sWit ->
        (txin, Api.BuildTxWith $ Api.ScriptWitness ScriptWitnessForSpending sWit)
      Nothing ->
        (txin, Api.BuildTxWith $ Api.KeyWitness KeyWitnessForSpending)

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
  -> Either CLI.TxCmdError (Api.TxInsReference Api.BuildTx era)
validateTxInsReference _ [] = return Api.TxInsReferenceNone
validateTxInsReference sbe allRefIns = do
  Api.forShelleyBasedEraInEonMaybe sbe (`Api.TxInsReference` allRefIns)
    & maybe (txFeatureMismatchPure (Api.toCardanoEra sbe) CLI.TxFeatureReferenceInputs) Right

getAllReferenceInputs
  :: [(Api.TxIn, Maybe (Api.ScriptWitness WitCtxTxIn era))]
  -> [Api.ScriptWitness WitCtxMint era]
  -> [(Certificate era, Maybe (Api.ScriptWitness WitCtxStake era))]
  -> [(StakeAddress, Coin, Maybe (Api.ScriptWitness WitCtxStake era))]
  -> [(VotingProcedures era, Maybe (Api.ScriptWitness WitCtxStake era))]
  -> [(Proposal era, Maybe (Api.ScriptWitness WitCtxStake era))]
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
      :: Api.ScriptWitness witctx era -> Maybe Api.TxIn
    getReferenceInput sWit =
      case sWit of
        PlutusScriptWitness _ _ (PReferenceScript refIn _) _ _ _ -> Just refIn
        PlutusScriptWitness _ _ PScript{} _ _ _ -> Nothing
        SimpleScriptWitness _ (SReferenceScript refIn _) -> Just refIn
        SimpleScriptWitness _ SScript{} -> Nothing

toAddressInAnyEra
  :: CardanoEra era
  -> AddressAny
  -> Either CLI.TxCmdError (AddressInEra era)
toAddressInAnyEra era addrAny = Api.runExcept $ do
  case addrAny of
    AddressByron bAddr -> pure (AddressInEra ByronAddressInAnyEra bAddr)
    AddressShelley sAddr -> do
      sbe <-
        Api.requireShelleyBasedEra era
          & Api.onNothing (txFeatureMismatch era CLI.TxFeatureShelleyAddresses)

      pure (AddressInEra (ShelleyAddressInEra sbe) sAddr)

toAddressInShelleyBasedEra
  :: Api.ShelleyBasedEra era
  -> Address ShelleyAddr
  -> Either CLI.TxCmdError (AddressInEra era)
toAddressInShelleyBasedEra sbe sAddr =
  Api.runExcept $
    pure (AddressInEra (ShelleyAddressInEra sbe) sAddr)

toTxOutValueInAnyEra
  :: Api.ShelleyBasedEra era
  -> Value
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
  -> Value
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
  -> ExceptT CLI.TxCmdError IO (Api.TxOut Api.CtxTx era)
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
  -> ExceptT CLI.TxCmdError IO (Api.TxOut Api.CtxTx ByronEra)
toTxOutByronEra (CLI.TxOutAnyEra addr' val' _ _) = do
  addr <- Api.hoistEither $ toAddressInAnyEra ByronEra addr'
  let ada = Api.TxOutValueByron $ Api.selectLovelace val'
  pure $ Api.TxOut addr ada Api.TxOutDatumNone Api.ReferenceScriptNone

-- TODO: toTxOutInAnyEra eventually will not be needed because
-- byron related functionality will be treated
-- separately
toTxOutInAnyEra
  :: Api.ShelleyBasedEra era
  -> CLI.TxOutAnyEra
  -> ExceptT CLI.TxCmdError IO (Api.TxOut Api.CtxTx era)
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
  -> ReferenceScriptAnyEra
  -> ExceptT CLI.TxCmdError IO (Api.ReferenceScript era)
getReferenceScript w = \case
  ReferenceScriptAnyEraNone -> return Api.ReferenceScriptNone
  ReferenceScriptAnyEra fp -> Api.ReferenceScript w <$> Api.firstExceptT CLI.TxCmdScriptFileError (CLI.readFileScriptInAnyLang fp)

toTxAlonzoDatum
  :: ()
  => AlonzoEraOnwards era
  -> CLI.TxOutDatumAnyEra
  -> ExceptT CLI.TxCmdError IO (Api.TxOutDatum Api.CtxTx era)
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
  -> (Value, [Api.ScriptWitness WitCtxMint era])
  -> Either CLI.TxCmdError (Api.TxMintValue Api.BuildTx era)
createTxMintValue era (val, scriptWitnesses) =
  if List.null (Api.valueToList val) && List.null scriptWitnesses
    then return Api.TxMintNone
    else do
      Api.caseShelleyToAllegraOrMaryEraOnwards
        (const (txFeatureMismatchPure (Api.toCardanoEra era) CLI.TxFeatureMintValue))
        ( \w -> do
            -- The set of policy ids for which we need witnesses:
            let witnessesNeededSet :: Set PolicyId
                witnessesNeededSet =
                  Set.fromList [pid | (AssetId pid _, _) <- Api.valueToList val]

            let witnessesProvidedMap :: Map PolicyId (Api.ScriptWitness WitCtxMint era)
                witnessesProvidedMap = Map.fromList $ gatherMintingWitnesses scriptWitnesses
                witnessesProvidedSet = Map.keysSet witnessesProvidedMap

            -- Check not too many, nor too few:
            validateAllWitnessesProvided witnessesNeededSet witnessesProvidedSet
            validateNoUnnecessaryWitnesses witnessesNeededSet witnessesProvidedSet
            return (Api.TxMintValue w val (Api.BuildTxWith witnessesProvidedMap))
        )
        era
 where
  gatherMintingWitnesses
    :: [Api.ScriptWitness WitCtxMint era]
    -> [(PolicyId, Api.ScriptWitness WitCtxMint era)]
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

scriptWitnessPolicyId :: Api.ScriptWitness witctx era -> Maybe PolicyId
scriptWitnessPolicyId (SimpleScriptWitness _ (SScript script)) =
  Just . Api.scriptPolicyId $ SimpleScript script
scriptWitnessPolicyId (SimpleScriptWitness _ (SReferenceScript _ mPid)) =
  PolicyId <$> mPid
scriptWitnessPolicyId (PlutusScriptWitness _ plutusVersion (PScript script) _ _ _) =
  Just . Api.scriptPolicyId $ PlutusScript plutusVersion script
scriptWitnessPolicyId (PlutusScriptWitness _ _ (PReferenceScript _ mPid) _ _ _) =
  PolicyId <$> mPid

readValueScriptWitnesses
  :: Api.ShelleyBasedEra era
  -> (Value, [ScriptWitnessFiles WitCtxMint])
  -> ExceptT CLI.TxCmdError IO (Value, [Api.ScriptWitness WitCtxMint era])
readValueScriptWitnesses era (v, sWitFiles) = do
  sWits <- mapM (Api.firstExceptT CLI.TxCmdScriptWitnessError . CLI.readScriptWitness era) sWitFiles
  return (v, sWits)

-- ----------------------------------------------------------------------------
-- Transaction signing
--

runTransactionSignCmd
  :: ()
  => Cmd.TransactionSignCmdArgs
  -> ExceptT CLI.TxCmdError IO ()
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
      InputTxFile (File inputTxFilePath) -> do
        inputTxFile <- Api.liftIO $ CLI.fileOrPipe inputTxFilePath
        anyTx <- Api.lift (CLI.readFileTx inputTxFile) & Api.onLeft (Api.left . CLI.TxCmdTextEnvCddlError)

        InAnyShelleyBasedEra sbe tx <- pure anyTx

        let (txbody, existingTxKeyWits) = Api.getTxBodyAndWitnesses tx

        byronWitnesses <-
          pure (mkShelleyBootstrapWitnesses sbe mNetworkId txbody sksByron)
            & Api.onLeft (Api.left . CLI.TxCmdBootstrapWitnessError)

        let newShelleyKeyWits = map (Api.makeShelleyKeyWitness sbe txbody) sksShelley
            allKeyWits = existingTxKeyWits ++ newShelleyKeyWits ++ byronWitnesses
            signedTx = Api.makeSignedTransaction allKeyWits txbody

        Api.lift (Api.writeTxFileTextEnvelopeCddl sbe outTxFile signedTx)
          & Api.onLeft (Api.left . CLI.TxCmdWriteFileError)
      InputTxBodyFile (File txbodyFilePath) -> do
        txbodyFile <- Api.liftIO $ CLI.fileOrPipe txbodyFilePath
        IncompleteCddlTxBody { .. } <-
          Api.firstExceptT CLI.TxCmdTextEnvCddlError . Api.newExceptT $
            CLI.readFileTxBody txbodyFile

        case unIncompleteCddlTxBody of
          InAnyShelleyBasedEra sbe txbody -> do

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
  -> ExceptT CLI.TxCmdError IO ()
runTransactionSubmitCmd
  Cmd.TransactionSubmitCmdArgs
    { nodeSocketPath
    , consensusModeParams
    , networkId
    , txFile
    } = do
    txFileOrPipe <- Api.liftIO $ CLI.fileOrPipe txFile
    InAnyShelleyBasedEra era tx <-
      Api.lift (CLI.readFileTx txFileOrPipe) & Api.onLeft (Api.left . CLI.TxCmdTextEnvCddlError)
    let txInMode = Api.TxInMode era tx
        localNodeConnInfo =
          LocalNodeConnectInfo
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
  -> ExceptT CLI.TxCmdError IO ()
runTransactionCalculateMinFeeCmd
  Cmd.TransactionCalculateMinFeeCmdArgs
    { txBodyFile = File txbodyFilePath
    , protocolParamsFile = protocolParamsFile
    , txShelleyWitnessCount = CLI.TxShelleyWitnessCount nShelleyKeyWitnesses
    , txByronWitnessCount = CLI.TxByronWitnessCount nByronKeyWitnesses
    , referenceScriptSize = ReferenceScriptSize sReferenceScript
    -- , outputFormat
    -- , outFile
    } = do
    txbodyFile <- Api.liftIO $ CLI.fileOrPipe txbodyFilePath
    IncompleteCddlTxBody { .. } <-
      Api.firstExceptT CLI.TxCmdTextEnvCddlError . Api.newExceptT $
        CLI.readFileTxBody txbodyFile

    let nShelleyKeyWitW32 = fromIntegral nShelleyKeyWitnesses

    InAnyShelleyBasedEra sbe txbody <- pure unIncompleteCddlTxBody

    lpparams <-
      Api.firstExceptT CLI.TxCmdProtocolParamsError $
        CLI.readProtocolParameters sbe protocolParamsFile

    let shelleyfee = Api.evaluateTransactionFee sbe lpparams txbody nShelleyKeyWitW32 0 sReferenceScript

    let byronfee =
          Api.shelleyBasedEraConstraints sbe $
            calculateByronWitnessFees (lpparams ^. Ledger.ppMinFeeAL) nByronKeyWitnesses

    let Coin fee = shelleyfee + byronfee
        textToWrite = Text.pack ((show fee :: String) <> " Lovelace")
        _jsonToWrite = Aeson.encodePretty $ Aeson.object ["fee" .= fee]

    -- The Query prefix should go away whenever I can get to actually
    -- link against the new cardano-cli.
    -- This whole code block can't really be used until the update works.
    -- What used to be there before the above was:
    Api.liftIO $ Text.putStrLn textToWrite
    {-
    case (newOutputFormat outputFormat outFile, outFile) of
      (OutputFormatText, Nothing) ->
        Api.liftIO $ Text.putStrLn textToWrite
      (OutputFormatText, Just file) ->
        Api.firstExceptT TxCmdWriteFileError . Api.newExceptT $ writeTextFile file textToWrite
      (OutputFormatJson, Nothing) ->
        Api.liftIO $ LBS8.putStrLn jsonToWrite
      (OutputFormatJson, Just file) ->
        Api.firstExceptT TxCmdWriteFileError . Api.newExceptT $ writeLazyByteStringFile file jsonToWrite
    -}

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
  -> ExceptT CLI.TxCmdError IO ()
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
  -> ExceptT CLI.TxCmdError IO ()
runTransactionPolicyIdCmd
  Cmd.TransactionPolicyIdCmdArgs
    { scriptFile = File sFile
    } = do
    ScriptInAnyLang _ script <-
      Api.firstExceptT CLI.TxCmdScriptFileError $
        CLI.readFileScriptInAnyLang sFile
    Api.liftIO . Text.putStrLn . Api.serialiseToRawBytesHexText $ Api.hashScript script

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
  => Api.ShelleyBasedEra era
  -> Maybe NetworkId
  -> Api.TxBody era
  -> ShelleyBootstrapWitnessSigningKeyData
  -> Either BootstrapWitnessError (Api.KeyWitness era)
mkShelleyBootstrapWitness _ Nothing _ (ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrByronAddressError
mkShelleyBootstrapWitness sbe (Just nw) txBody (ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ Api.makeShelleyBootstrapWitness sbe (Byron.WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness sbe _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ Api.makeShelleyBootstrapWitness sbe (Byron.WitnessByronAddress addr) txBody skey

-- | Attempt to construct Shelley bootstrap witnesses until an error is
-- encountered.
mkShelleyBootstrapWitnesses
  :: ()
  => Api.ShelleyBasedEra era
  -> Maybe NetworkId
  -> Api.TxBody era
  -> [ShelleyBootstrapWitnessSigningKeyData]
  -> Either BootstrapWitnessError [Api.KeyWitness era]
mkShelleyBootstrapWitnesses sbe mnw txBody =
  mapM (mkShelleyBootstrapWitness sbe mnw txBody)

-- ----------------------------------------------------------------------------
-- Other misc small commands
--

runTransactionHashScriptDataCmd
  :: ()
  => Cmd.TransactionHashScriptDataCmdArgs
  -> ExceptT CLI.TxCmdError IO ()
runTransactionHashScriptDataCmd
  Cmd.TransactionHashScriptDataCmdArgs
    { scriptDataOrFile
    } = do
    d <- Api.firstExceptT CLI.TxCmdScriptDataError $ CLI.readScriptDataOrFile scriptDataOrFile
    Api.liftIO $ BS8.putStrLn $ Api.serialiseToRawBytesHex (Api.hashScriptDataBytes d)

runTransactionTxIdCmd
  :: ()
  => Cmd.TransactionTxIdCmdArgs
  -> ExceptT CLI.TxCmdError IO ()
runTransactionTxIdCmd
  Cmd.TransactionTxIdCmdArgs
    { inputTxBodyOrTxFile
    } = do
    InAnyShelleyBasedEra _era txbody <-
      case inputTxBodyOrTxFile of
        InputTxBodyFile (File txbodyFilePath) -> do
          txbodyFile <- Api.liftIO $ CLI.fileOrPipe txbodyFilePath
          IncompleteCddlTxBody { .. } <-
            Api.firstExceptT CLI.TxCmdTextEnvCddlError . Api.newExceptT $
              CLI.readFileTxBody txbodyFile
          pure unIncompleteCddlTxBody
        InputTxFile (File txFilePath) -> do
          txFile <- Api.liftIO $ CLI.fileOrPipe txFilePath
          InAnyShelleyBasedEra era tx <- Api.lift (CLI.readFileTx txFile) & Api.onLeft (Api.left . CLI.TxCmdTextEnvCddlError)
          return . InAnyShelleyBasedEra era $ Api.getTxBody tx

    Api.liftIO $ BS8.putStrLn $ Api.serialiseToRawBytesHex (Api.getTxId txbody)

runTransactionViewCmd
  :: ()
  => Cmd.TransactionViewCmdArgs
  -> ExceptT CLI.TxCmdError IO ()
runTransactionViewCmd
  Cmd.TransactionViewCmdArgs
    { outputFormat
    , mOutFile
    , inputTxBodyOrTxFile
    } =
    case inputTxBodyOrTxFile of
      InputTxBodyFile (File txbodyFilePath) -> do
        txbodyFile <- Api.liftIO $ CLI.fileOrPipe txbodyFilePath
        IncompleteCddlTxBody { unIncompleteCddlTxBody = InAnyShelleyBasedEra era txbody } <-
          Api.firstExceptT CLI.TxCmdTextEnvCddlError . Api.newExceptT $
            CLI.readFileTxBody txbodyFile
        -- Why are we differentiating between a transaction body and a transaction?
        -- In the case of a transaction body, we /could/ simply call @Api.makeSignedTransaction []@
        -- to get a transaction which would allow us to reuse friendlyTxBS. However,
        -- this would mean that we'd have an empty list of witnesses mentioned in the output, which
        -- is arguably not part of the transaction body.
        Api.firstExceptT CLI.TxCmdWriteFileError . Api.newExceptT $
          case outputFormat of
            ViewOutputFormatYaml -> CLI.friendlyTxBody FriendlyYaml mOutFile (Api.toCardanoEra era) txbody
            ViewOutputFormatJson -> CLI.friendlyTxBody FriendlyJson mOutFile (Api.toCardanoEra era) txbody
      InputTxFile (File txFilePath) -> do
        txFile <- Api.liftIO $ CLI.fileOrPipe txFilePath
        InAnyShelleyBasedEra era tx <- Api.lift (CLI.readFileTx txFile) & Api.onLeft (Api.left . CLI.TxCmdTextEnvCddlError)
        Api.firstExceptT CLI.TxCmdWriteFileError . Api.newExceptT $
          case outputFormat of
            ViewOutputFormatYaml -> CLI.friendlyTx FriendlyYaml mOutFile (Api.toCardanoEra era) tx
            ViewOutputFormatJson -> CLI.friendlyTx FriendlyJson mOutFile (Api.toCardanoEra era) tx

-- ----------------------------------------------------------------------------
-- Witness commands
--

runTransactionWitnessCmd
  :: ()
  => Cmd.TransactionWitnessCmdArgs
  -> ExceptT CLI.TxCmdError IO ()
runTransactionWitnessCmd
  Cmd.TransactionWitnessCmdArgs
    { txBodyFile = File txbodyFilePath
    , witnessSigningData
    , mNetworkId
    , outFile
    } = do
    txbodyFile <- Api.liftIO $ CLI.fileOrPipe txbodyFilePath
    IncompleteCddlTxBody { unIncompleteCddlTxBody = InAnyShelleyBasedEra sbe txbody } <-
      Api.firstExceptT CLI.TxCmdTextEnvCddlError . Api.newExceptT $
        CLI.readFileTxBody txbodyFile
    someWit <-
      Api.firstExceptT CLI.TxCmdReadWitnessSigningDataError
        . Api.newExceptT
        $ CLI.readWitnessSigningData witnessSigningData
    witness <-
      case CLI.categoriseSomeSigningWitness someWit :: ByronOrShelleyWitness of
        -- Byron witnesses require the network ID. This can either be provided
        -- directly or derived from a provided Byron address.
        AByronWitness bootstrapWitData ->
          Api.firstExceptT CLI.TxCmdBootstrapWitnessError
            . Api.hoistEither
            $ mkShelleyBootstrapWitness sbe mNetworkId txbody bootstrapWitData
        AShelleyKeyWitness skShelley ->
          pure $ Api.makeShelleyKeyWitness sbe txbody skShelley

    Api.firstExceptT CLI.TxCmdWriteFileError . Api.newExceptT $
      Api.writeTxWitnessFileTextEnvelopeCddl sbe outFile witness

runTransactionSignWitnessCmd
  :: ()
  => Cmd.TransactionSignWitnessCmdArgs
  -> ExceptT CLI.TxCmdError IO ()
runTransactionSignWitnessCmd
  Cmd.TransactionSignWitnessCmdArgs
    { txBodyFile = File txbodyFilePath
    , witnessFiles = witnessFiles
    , outFile = outFile
    } = do
    txbodyFile <- Api.liftIO $ CLI.fileOrPipe txbodyFilePath
    IncompleteCddlTxBody { unIncompleteCddlTxBody = InAnyShelleyBasedEra era txbody } <- Api.lift (CLI.readFileTxBody txbodyFile) & Api.onLeft (Api.left . CLI.TxCmdTextEnvCddlError)
    -- TODO: Left off here. Remember we were never reading byron key witnesses anyways!
    witnesses <-
      sequence
        [ do
            InAnyShelleyBasedEra era' witness <-
              Api.lift (CLI.readFileTxKeyWitness file) & Api.onLeft (Api.left . CLI.TxCmdCddlWitnessError)

            case testEquality era era' of
              Nothing ->
                Api.left $
                  CLI.TxCmdWitnessEraMismatch
                    (AnyCardanoEra $ Api.toCardanoEra era)
                    (AnyCardanoEra $ Api.toCardanoEra era')
                    witnessFile
              Just Refl -> return witness
        | witnessFile@(WitnessFile file) <- witnessFiles
        ]

    let tx = Api.makeSignedTransaction witnesses txbody

    Api.lift (Api.writeTxFileTextEnvelopeCddl era outFile tx) & Api.onLeft (Api.left . CLI.TxCmdWriteFileError)
-}
