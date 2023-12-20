{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module  Cardano.TxGenerator.PureExample
        (demo)
        where

import           Control.Monad (foldM)
import           Control.Monad.Trans.State.Strict
import           Data.Either (fromRight)
import           Data.List (foldl')
import           Data.String (fromString)
import           System.Exit (die)

import           Cardano.Api
import           Cardano.Api.Shelley (convertToLedgerProtocolParameters)

import           Data.Aeson (eitherDecodeFileStrict')

import           Cardano.TxGenerator.FundQueue
import           Cardano.TxGenerator.Setup.SigningKey
import           Cardano.TxGenerator.Tx (genTx, sourceToStoreTransaction)
import           Cardano.TxGenerator.Types (TxEnvironment (..), TxGenError (..), TxGenerator)
import           Cardano.TxGenerator.Utils (inputsToOutputsWithFee)
import           Cardano.TxGenerator.UTxO (makeToUTxOList, mkUTxOVariant)

import           Paths_tx_generator


demo :: IO ()
demo = getDataFileName "data/protocol-parameters.json" >>= demo'

demo' :: FilePath -> IO ()
demo' parametersFile = do
  protocolParameters <- either die pure =<< eitherDecodeFileStrict' parametersFile
  let
      demoEnv :: TxEnvironment BabbageEra
      demoEnv = TxEnvironment {
          txEnvNetworkId = Mainnet
        , txEnvProtocolParams = protocolParameters
        , txEnvFee = TxFeeExplicit ShelleyBasedEraBabbage 100000
        , txEnvMetadata = TxMetadataNone
        }

  run1 <- foldM (worker $ generateTx demoEnv) (emptyFundQueue `insertFund` genesisFund) [1..10]
  run2 <- foldM (worker $ generateTxM demoEnv) (emptyFundQueue `insertFund` genesisFund) [1..10]
  putStrLn $ "Are run results identical? " ++ show (toList run1 == toList run2)
  where
    worker ::
         Generator (Either TxGenError (Tx BabbageEra))
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
genesisValue :: TxOutValue BabbageEra

(genesisTxIn, genesisValue) =
  ( TxIn "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162" (TxIx 0)
  , lovelaceToTxOutValue ShelleyBasedEraBabbage $ Lovelace 90000000000000
  )

genesisFund :: Fund
genesisFund
  = Fund $ InAnyCardanoEra BabbageEra fundInEra
  where
    fundInEra :: FundInEra BabbageEra
    fundInEra  = FundInEra {
        _fundTxIn = genesisTxIn
      , _fundVal = genesisValue
      , _fundWitness = KeyWitness KeyWitnessForSpending
      , _fundSigningKey = Just signingKey
      }

type Generator = State FundQueue

generateTx ::
     TxEnvironment BabbageEra
  -> Generator (Either TxGenError (Tx BabbageEra))
generateTx TxEnvironment{..}
  = sourceToStoreTransaction
        generator
        consumeInputFunds
        computeOutputValues
        (makeToUTxOList $ repeat computeUTxO)
        addNewOutputFunds
  where
    TxFeeExplicit _ fee = txEnvFee

    generator :: TxGenerator BabbageEra
    generator =
        case convertToLedgerProtocolParameters shelleyBasedEra txEnvProtocolParams of
          Right ledgerParameters ->
            genTx ShelleyBasedEraBabbage ledgerParameters collateralFunds txEnvFee txEnvMetadata
          Left err -> \_ _ -> Left (ApiError err)
      where
        -- collateralFunds are needed for Plutus transactions
        collateralFunds :: (TxInsCollateral BabbageEra, [Fund])
        collateralFunds = (TxInsCollateralNone, [])

-- Create a transaction that uses all the available funds.
    consumeInputFunds :: Generator (Either TxGenError [Fund])
    consumeInputFunds = do
      funds <- toList <$> get
      put emptyFundQueue
      return $ Right funds

    addNewOutputFunds :: [Fund] -> Generator ()
    addNewOutputFunds = put . foldl' insertFund emptyFundQueue

    computeOutputValues :: [Lovelace] -> [Lovelace]
    computeOutputValues = inputsToOutputsWithFee fee numOfOutputs
      where numOfOutputs = 2

    computeUTxO = mkUTxOVariant txEnvNetworkId signingKey


generateTxM ::
      TxEnvironment BabbageEra
  ->  Generator (Either TxGenError (Tx BabbageEra))
generateTxM txEnv
  = do
      inFunds <- get
      case generateTxPure txEnv inFunds of
        Right (tx, outFunds)  -> put outFunds >> pure (Right tx)
        Left err              -> pure (Left err)

generateTxPure ::
     TxEnvironment BabbageEra
  -> FundQueue
  -> Either TxGenError (Tx BabbageEra, FundQueue)
generateTxPure TxEnvironment{..} inQueue
  = do
      (tx, txId) <- generator inputs outputs
      let outQueue = foldl' insertFund emptyFundQueue (toFunds txId)
      pure (tx, outQueue)
  where
    inputs = toList inQueue
    TxFeeExplicit _ fee = txEnvFee

    generator :: TxGenerator BabbageEra
    generator =
        case convertToLedgerProtocolParameters shelleyBasedEra txEnvProtocolParams of
          Right ledgerParameters ->
            genTx ShelleyBasedEraBabbage ledgerParameters collateralFunds txEnvFee txEnvMetadata
          Left err -> \_ _ -> Left (ApiError err)
      where
        -- collateralFunds are needed for Plutus transactions
        collateralFunds :: (TxInsCollateral BabbageEra, [Fund])
        collateralFunds = (TxInsCollateralNone, [])

    outValues = computeOutputValues $ map getFundLovelace inputs
    (outputs, toFunds) = makeToUTxOList (repeat computeUTxO) outValues

    computeOutputValues :: [Lovelace] -> [Lovelace]
    computeOutputValues = inputsToOutputsWithFee fee numOfOutputs
      where numOfOutputs = 2

    computeUTxO = mkUTxOVariant txEnvNetworkId signingKey
