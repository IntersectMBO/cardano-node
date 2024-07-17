{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module  Cardano.TxGenerator.GovExample
        -- (demo)
        where

import           Cardano.Api
import           Cardano.Api.Shelley (LedgerProtocolParameters, Vote, convertToLedgerProtocolParameters, createVotingProcedure)

import qualified Cardano.CLI.Types.Governance as CLI (AnyVotingStakeVerificationKeyOrHashOrFile (..))
import qualified Cardano.Ledger.BaseTypes as Ledger (Url)
import qualified Cardano.Ledger.Coin as Ledger
import           Cardano.TxGenerator.FundQueue
import           Cardano.TxGenerator.Setup.SigningKey
import           Cardano.TxGenerator.Types (FundSource, FundToStoreList, TxEnvironment (..), TxGenError (..), TxGenerator)
import           Cardano.TxGenerator.Utils (inputsToOutputsWithFee)
import           Cardano.TxGenerator.UTxO (ToUTxOList, makeToUTxOList, mkUTxOVariant)

import           Control.Monad (foldM)
import           Control.Monad.Trans.State.Strict
import           Data.Aeson (eitherDecodeFileStrict')
import           Data.Bifunctor (bimap)
import           Data.Either (fromRight)
import           Data.Function ((&))
import           Data.List (foldl')
import           Data.Maybe (mapMaybe)
import           Data.String (fromString)
import           Data.Text (Text)
import           System.Exit (die)

import           Paths_tx_generator


demo :: IO ()
demo = getDataFileName "data/protocol-parameters.json" >>= demo'

demo' :: FilePath -> IO ()
demo' parametersFile = do
  protocolParameters <- either die pure =<< eitherDecodeFileStrict' parametersFile
  let
      demoEnv :: TxEnvironment ConwayEra
      demoEnv = TxEnvironment {
          txEnvNetworkId = Mainnet
        , txEnvProtocolParams = protocolParameters
        , txEnvFee = TxFeeExplicit ShelleyBasedEraConway 100000
        , txEnvMetadata = TxMetadataNone
        }

  run1 <- foldM (worker $ generateTx demoEnv) (emptyFundQueue `insertFund` genesisFund) [1..10]
  run2 <- foldM (worker $ generateTxM demoEnv) (emptyFundQueue `insertFund` genesisFund) [1..10]
  putStrLn $ "Are run results identical? " ++ show (toList run1 == toList run2)
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

genesisTxIn :: TxIn
genesisValue :: TxOutValue ConwayEra

(genesisTxIn, genesisValue) =
  ( TxIn "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162" (TxIx 0)
  , lovelaceToTxOutValue ShelleyBasedEraConway $ Ledger.Coin 90000000000000
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
localGenVote :: ConwayEraOnwards era -> Vote -> IO ()
localGenVote era vote = do
  _procedure <- pure $ createVotingProcedure
                             (era {- eon :: ConwayEraOnwards era {- the type signature chokes? -} -} )
                             (vote {- votingChoice -} :: Vote)
                             (Nothing :: Maybe (Ledger.Url, Text))
  _ <- shelleyBasedEraConstraints localShelleyBasedEra do
    _ <- pure (undefined :: CLI.AnyVotingStakeVerificationKeyOrHashOrFile)
    pure undefined
  pure ()
  where
    localShelleyBasedEra = conwayEraOnwardsToShelleyBasedEra era

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
  allKeys = mapMaybe getFundKey $ inFunds ++ collFunds
  txBodyContent = defaultTxBodyContent sbe
    & setTxIns (map (\f -> (getFundTxIn f, BuildTxWith $ getFundWitness f)) inFunds)
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
  -> ([Ledger.Coin] -> split)
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
      outValues = inToOut $ map getFundCoin inputFunds
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
        case convertToLedgerProtocolParameters shelleyBasedEra txEnvProtocolParams of
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
      funds <- toList <$> get
      put emptyFundQueue
      return $ Right funds

    addNewOutputFunds :: [Fund] -> Generator ()
    addNewOutputFunds = put . foldl' insertFund emptyFundQueue

    computeOutputValues :: [Ledger.Coin] -> [Ledger.Coin]
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
      let outQueue = foldl' insertFund emptyFundQueue (toFunds txId)
      pure (tx, outQueue)
  where
    inputs = toList inQueue
    TxFeeExplicit _ fee = txEnvFee

    generator :: TxGenerator ConwayEra
    generator =
        case convertToLedgerProtocolParameters shelleyBasedEra txEnvProtocolParams of
          Right ledgerParameters ->
            localGenTx ShelleyBasedEraConway ledgerParameters collateralFunds txEnvFee txEnvMetadata
          Left err -> \_ _ -> Left (ApiError err)
      where
        -- collateralFunds are needed for Plutus transactions
        collateralFunds :: (TxInsCollateral ConwayEra, [Fund])
        collateralFunds = (TxInsCollateralNone, [])

    outValues = computeOutputValues $ map getFundCoin inputs
    (outputs, toFunds) = makeToUTxOList (repeat computeUTxO) outValues

    computeOutputValues :: [Ledger.Coin] -> [Ledger.Coin]
    computeOutputValues = inputsToOutputsWithFee fee numOfOutputs
      where numOfOutputs = 2

    computeUTxO = mkUTxOVariant txEnvNetworkId signingKey
