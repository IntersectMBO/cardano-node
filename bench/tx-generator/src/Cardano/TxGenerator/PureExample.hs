module  Cardano.TxGenerator.PureExample
        (demo)
        where

import           Control.Monad (foldM_)
import           Control.Monad.Trans.State.Strict
import           Data.Either (fromRight)
import           Data.String (fromString)
import           System.Exit (die)

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters)

import           Data.Aeson (eitherDecodeFileStrict')

import           Cardano.Benchmarking.Script.Core (parseSigningKey)

import           Cardano.TxGenerator.Fund (Fund (..), FundInEra (..))
import           Cardano.TxGenerator.Tx (genTx, sourceToStoreTransaction)
import           Cardano.TxGenerator.Types (TxGenError (..), TxGenerator)
import           Cardano.TxGenerator.UTxO (makeToUTxOList, mkUTxOVariant)
import           Cardano.TxGenerator.Utils (inputsToOutputsWithFee)

import           Paths_tx_generator


demo :: IO ()
demo = getDataFileName "data/protocol-parameters.json" >>= demo'

demo' :: FilePath -> IO ()
demo' parametersFile = do
  protocolParameters <- either die pure =<< eitherDecodeFileStrict' parametersFile
  foldM_ (worker $ generateTx protocolParameters) [genesisFund] [1..10]
  where
    worker ::
         Generator (Either TxGenError (Tx BabbageEra))
      -> [Fund]
      -> Int
      -> IO [Fund]
    worker pureGenerator generatorState counter = do
      putStrLn $ "running tx-generator. Iteration : " ++ show counter
      let (res, newState) = runState pureGenerator generatorState
      case res of
        Right tx -> print tx
        Left err -> print err
      return newState

signingKey :: SigningKey PaymentKey
signingKey = fromRight (error "signingKey: parseError") $ parseSigningKey keyData
  where
    keyData = TextEnvelope { teType = TextEnvelopeType "GenesisUTxOSigningKey_ed25519"
              , teDescription = fromString "Genesis Initial UTxO Signing Key"
              , teRawCBOR = "X \vl1~\182\201v(\152\250A\202\157h0\ETX\248h\153\171\SI/m\186\242D\228\NAK\182(&\162"}

genesisTxIn :: TxIn
genesisValue :: TxOutValue BabbageEra

(genesisTxIn, genesisValue) =
  ( TxIn "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162" (TxIx 0)
  , lovelaceToTxOutValue $ Lovelace 90000000000000
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

type Generator = State [Fund]

generateTx ::
     ProtocolParameters
  -> Generator (Either TxGenError (Tx BabbageEra))
generateTx protocolParameters
  = sourceToStoreTransaction
        generator
        consumeInputFunds
        computeOutputValues
        (makeToUTxOList $ repeat computeUTxO)
        addNewOutputFunds
  where
    -- In the simplest form of the tx-generator
    -- fees, metadata etc.. are all hardcoded.

    networkId :: NetworkId
    networkId = Mainnet

    -- hardcoded fees
    fee :: Lovelace
    fee = 100000

    babbageFee :: TxFee BabbageEra
    babbageFee = TxFeeExplicit TxFeesExplicitInBabbageEra fee

    metadata :: TxMetadataInEra BabbageEra
    metadata = TxMetadataNone

    generator :: TxGenerator BabbageEra
    generator = genTx protocolParameters collateralFunds babbageFee metadata
      where
        -- collateralFunds are needed for Plutus transactions
        collateralFunds :: (TxInsCollateral BabbageEra, [Fund])
        collateralFunds = (TxInsCollateralNone, [])

-- Create a transaction that uses all the available funds.
    consumeInputFunds :: Generator (Either TxGenError [Fund])
    consumeInputFunds = do
      funds <- get
      put []
      return $ Right funds

    addNewOutputFunds :: [Fund] -> Generator ()
    addNewOutputFunds = put

    computeOutputValues :: [Lovelace] -> [Lovelace]
    computeOutputValues = inputsToOutputsWithFee fee numOfOutputs
      where numOfOutputs = 2

    computeUTxO = mkUTxOVariant networkId signingKey
