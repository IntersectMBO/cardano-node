{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.Wallet
where
import           Prelude

<<<<<<< HEAD
import           Streaming
=======
>>>>>>> 2b6fb848a (bench conway changes)
import           Control.Concurrent.MVar
import           Data.Maybe

import           Cardano.Api

<<<<<<< HEAD
import           Cardano.TxGenerator.FundQueue as FundQueue
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Tx
import           Cardano.TxGenerator.UTxO
=======
import           Cardano.Api.Shelley (ProtocolParameters, ReferenceScript (..))
import           Cardano.Benchmarking.Fifo as Fifo
import           Cardano.Benchmarking.FundSet as FundSet
import           Cardano.Benchmarking.Types (NumberOfTxs (..))
>>>>>>> 2b6fb848a (bench conway changes)

-- All the actual functionality of Wallet / WalletRef has been removed
-- and WalletRef has been stripped down to MVar FundQueue.
-- The implementation of Wallet has become trivial.
-- Todo: Remove trivial wrapper functions.

type WalletRef = MVar FundQueue

-- 'ToUTxOList era' is more powerful than '[ ToUTxO era ]' but
-- '[ ToUTxO era ]` is easier to construct.

--type TxStream m era = Stream (Of (Tx era)) m (Maybe String)
type TxStream m era = Stream (Of (Either String (Tx era))) m ()

createAndStore :: ToUTxO era -> (Fund -> m ()) -> CreateAndStore m era
createAndStore create store lovelace = (utxo, toStore)
  where
    (utxo, mkFund) = create lovelace
    toStore txIx txId = store $ mkFund txIx txId

initWallet :: IO WalletRef
initWallet = newMVar emptyFundQueue

askWalletRef :: WalletRef -> (FundQueue -> a) -> IO a
askWalletRef r f = do
  w <- readMVar r
  return $ f w

walletRefInsertFund :: WalletRef -> Fund -> IO ()
walletRefInsertFund ref fund = modifyMVar_  ref $ \w -> return $ FundQueue.insertFund w fund

mkWalletFundStoreList :: WalletRef -> FundToStoreList IO
mkWalletFundStoreList walletRef funds = modifyMVar_  walletRef
  $ \wallet -> return (foldl FundQueue.insertFund wallet funds)

mkWalletFundStore :: WalletRef -> FundToStore IO
mkWalletFundStore walletRef fund = modifyMVar_  walletRef
  $ \wallet -> return $ FundQueue.insertFund wallet fund

walletSource :: WalletRef -> Int -> FundSource IO
walletSource ref munch = modifyMVar ref $ \fifo -> return $ case removeFunds munch fifo of
  Nothing -> (fifo, Left "WalletSource: out of funds")
  Just (newFifo, funds) -> (newFifo, Right funds)

<<<<<<< HEAD
=======
makeToUTxOList :: [ ToUTxO era ] -> ToUTxOList era [ Lovelace ]
makeToUTxOList fkts values
  = (outs, \txId -> map (\f -> f txId) fs)
  where
    (outs, fs) =unzip $ map worker $ zip3 fkts values [TxIx 0 ..]
    worker (toUTxO, value, idx)
      = let (o, f ) = toUTxO value
         in  (o, f idx)

data PayWithChange
  = PayExact [Lovelace]
  | PayWithChange Lovelace [Lovelace]

>>>>>>> 2b6fb848a (bench conway changes)
mangleWithChange :: Monad m => CreateAndStore m era -> CreateAndStore m era -> CreateAndStoreList m era PayWithChange
mangleWithChange mkChange mkPayment outs = case outs of
  PayExact l -> mangle (repeat mkPayment) l
  PayWithChange change payments -> mangle (mkChange : repeat mkPayment) (change : payments)

mangle :: Monad m => [ CreateAndStore m era ] -> CreateAndStoreList m era [ Lovelace ]
mangle fkts values
  = (outs, \txId -> mapM_ (\f -> f txId) fs)
  where
    (outs, fs) =unzip $ map worker $ zip3 fkts values [TxIx 0 ..]
    worker (toUTxO, value, idx)
      = let (o, f ) = toUTxO value
<<<<<<< HEAD
         in  (o, f idx) 
=======
         in  (o, f idx)

--TODO use Error monad
--TODO need to break this up
sourceToStoreTransaction ::
     TxGenerator era
  -> FundSource IO
  -> ([Lovelace] -> split)
  -> ToUTxOList era split
  -> FundToStoreList IO                --inline to ToUTxOList
  -> IO (Either String (Tx era))
sourceToStoreTransaction txGenerator fundSource inToOut mkTxOut fundToStore = do
  fundSource >>= \case
    Left err -> return $ Left err
    Right inputFunds -> work inputFunds
 where
  work inputFunds = do
    let
      outValues = inToOut $ map getFundLovelace inputFunds
      (outputs, toFunds) = mkTxOut outValues
    case txGenerator inputFunds outputs of
        Left err -> return $ Left err
        Right (tx, txId) -> do
          fundToStore $ toFunds txId
          return $ Right tx

sourceToStoreTransactionNew ::
     TxGenerator era
  -> FundSource IO
  -> ([Lovelace] -> split)
  -> CreateAndStoreList IO era split
  -> IO (Either String (Tx era))
sourceToStoreTransactionNew txGenerator fundSource valueSplitter toStore = do
  fundSource >>= \case
    Left err -> return $ Left err
    Right inputFunds -> work inputFunds
 where
  work inputFunds = do
    let
      split = valueSplitter $ map getFundLovelace inputFunds
      (outputs, storeAction) = toStore split
    case txGenerator inputFunds outputs of
        Left err -> return $ Left err
        Right (tx, txId) -> do
          storeAction txId
          return $ Right tx

includeChange :: Lovelace -> [Lovelace] -> [Lovelace] -> [Lovelace]
includeChange fee spend have = case compare changeValue 0 of
  GT -> changeValue : spend
  EQ -> spend
  LT -> error "genTX: Bad transaction: insufficient funds"
  where changeValue = sum have - sum spend - fee

includeChangeNew :: Lovelace -> [Lovelace] -> [Lovelace] -> PayWithChange
includeChangeNew fee spend have = case compare changeValue 0 of
  GT -> PayWithChange changeValue spend
  EQ -> PayExact spend
  LT -> error "genTX: Bad transaction: insufficient funds"
  where changeValue = sum have - sum spend - fee

mkUTxOVariant :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> ToUTxO era
mkUTxOVariant networkId key value
  = ( mkTxOut value
    , mkNewFund value
    )
 where
  mkTxOut v = TxOut (keyAddress @ era networkId key) (lovelaceToTxOutValue v) TxOutDatumNone ReferenceScriptNone

  mkNewFund :: Lovelace -> TxIx -> TxId -> Fund
  mkNewFund val txIx txId = Fund $ InAnyCardanoEra (cardanoEra @ era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundWitness = KeyWitness KeyWitnessForSpending
    , _fundVal = lovelaceToTxOutValue val
    , _fundSigningKey = Just key
    }

-- to be merged with mkUTxOVariant
mkUTxOScript :: forall era.
     IsShelleyBasedEra era
  => NetworkId
  -> (Script PlutusScriptV1, ScriptData)
  -> Witness WitCtxTxIn era
  -> ToUTxO era
mkUTxOScript networkId (script, txOutDatum) witness value
  = ( mkTxOut value
    , mkNewFund value
    )
 where
  plutusScriptAddr = makeShelleyAddressInEra
                       networkId
                       (PaymentCredentialByScript $ hashScript script)
                       NoStakeAddress

  mkTxOut v = case scriptDataSupportedInEra (cardanoEra @ era) of
    Nothing -> error " mkUtxOScript scriptDataSupportedInEra==Nothing"
    Just tag -> TxOut
                  plutusScriptAddr
                  (lovelaceToTxOutValue v)
                  (TxOutDatumHash tag $ hashScriptData txOutDatum)
                  ReferenceScriptNone

  mkNewFund :: Lovelace -> TxIx -> TxId -> Fund
  mkNewFund val txIx txId = Fund $ InAnyCardanoEra (cardanoEra @ era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundWitness = witness
    , _fundVal = lovelaceToTxOutValue val
    , _fundSigningKey = Nothing
    }

genTx :: forall era. IsShelleyBasedEra era =>
     ProtocolParameters
  -> (TxInsCollateral era, [Fund])
  -> TxFee era
  -> TxMetadataInEra era
  -> TxGenerator era
genTx protocolParameters (collateral, collFunds) fee metadata inFunds outputs
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signShelleyTransaction b $ map WitnessPaymentKey allKeys
                       , getTxId b
                       )
 where
  allKeys = mapMaybe getFundKey $ inFunds ++ collFunds
  txBodyContent = TxBodyContent {
      txIns = map (\f -> (getFundTxIn f, BuildTxWith $ getFundWitness f)) inFunds
    , txInsCollateral = collateral
    , txInsReference = TxInsReferenceNone
    , txOuts = outputs
    , txFee = fee
    , txValidityRange = (TxValidityNoLowerBound, upperBound)
    , txMetadata = metadata
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith $ Just protocolParameters
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    , txReturnCollateral = TxReturnCollateralNone
    , txTotalCollateral = TxTotalCollateralNone
    }

  upperBound :: TxValidityUpperBound era
  upperBound = case shelleyBasedEra @ era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra $ SlotNo maxBound
    ShelleyBasedEraAllegra -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
    ShelleyBasedEraMary    -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
    ShelleyBasedEraAlonzo  -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra
    ShelleyBasedEraBabbage -> TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
    ShelleyBasedEraConway  -> TxValidityNoUpperBound ValidityNoUpperBoundInConwayEra

newtype WalletScript era = WalletScript { runWalletScript :: IO (WalletStep era) }

data WalletStep era
  = Done
  | NextTx !(WalletScript era) !(Tx era)
  | Error String

-- TODO:
-- Define generator for a single transaction and define combinator for
-- repeat and sequence.


benchmarkWalletScript :: forall era .
     IsShelleyBasedEra era
  => IO (Either String (Tx era)) -- make polymorphic
  -> NumberOfTxs
  -> WalletScript era
benchmarkWalletScript sourceToStore totalCount
  = WalletScript $ walletStep totalCount
 where
  walletStep :: NumberOfTxs -> IO (WalletStep era)
  walletStep (NumberOfTxs 0) = return Done
  walletStep count = sourceToStore >>= \case
    Left err -> return $ Error err
    Right tx -> return $ NextTx (benchmarkWalletScript sourceToStore (pred count)) tx

limitSteps ::
     NumberOfTxs
  -> WalletScript era
  -> WalletScript era
limitSteps = undefined

keyAddress :: forall era. IsShelleyBasedEra era => NetworkId -> SigningKey PaymentKey -> AddressInEra era
keyAddress networkId k
  = makeShelleyAddressInEra
      networkId
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
      NoStakeAddress
>>>>>>> 2b6fb848a (bench conway changes)
