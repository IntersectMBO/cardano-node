{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Benchmarking.Wallet
where
import           Prelude

import           Control.Concurrent.MVar

import           Cardano.Api

import           Cardano.Benchmarking.Types (NumberOfTxs(..))
import           Cardano.Benchmarking.GeneratorTx.Tx as Tx hiding (Fund)
import           Cardano.Benchmarking.FundSet as FundSet

type WalletRef = MVar Wallet

type TxGenerator era = [Fund] -> [TxOut era] -> Either String (Tx era, TxId)
type ToUTxO era = [Lovelace] -> ([TxOut era], TxId -> [Fund])

data Wallet = Wallet {
    walletNetworkId :: !NetworkId
  , walletKey :: !(SigningKey PaymentKey)
  , walletSeqNumber :: !SeqNumber
  , walletFunds :: !FundSet
  }

initWallet :: NetworkId -> SigningKey PaymentKey -> IO (MVar Wallet)
initWallet network key = newMVar $ Wallet {
    walletNetworkId = network
  , walletKey = key
  , walletSeqNumber = SeqNumber 1
  , walletFunds = emptyFunds
  }

askWalletRef :: WalletRef -> (Wallet -> a) -> IO a
askWalletRef r f = do
  w <- readMVar r
  return $ f w

modifyWalletRef :: WalletRef -> (Wallet -> IO (Wallet, a)) -> IO a
modifyWalletRef = modifyMVar

modifyWalletRefEither :: WalletRef -> (Wallet -> IO (Either err (Wallet,a))) -> IO (Either err a)
modifyWalletRefEither ref action
  = modifyMVar ref $ \w -> action w >>= \case
     Right (newWallet, res) -> return (newWallet, Right res)
     Left err -> return (w, Left err)

walletRefInsertFund :: WalletRef -> Fund -> IO ()
walletRefInsertFund ref fund = modifyMVar_  ref $ \w -> return $ walletInsertFund fund w

walletInsertFund :: Fund -> Wallet -> Wallet
walletInsertFund f w
  = w { walletFunds = FundSet.insertFund (walletFunds w) f }

walletDeleteFund :: Fund -> Wallet -> Wallet
walletDeleteFund f w
  = w { walletFunds = FundSet.deleteFund (walletFunds w) f }

walletSelectFunds :: Wallet -> FundSelector -> Either String [Fund]
walletSelectFunds w s = s $ walletFunds w

walletExtractFunds :: Wallet -> FundSelector -> Either String (Wallet, [Fund])
walletExtractFunds w s
  = case walletSelectFunds w s of
    Left err -> Left err
    Right funds -> Right (foldl (flip walletDeleteFund) w funds, funds)

mkWalletFundSource :: WalletRef -> FundSelector -> FundSource
mkWalletFundSource walletRef selector
  = modifyWalletRefEither walletRef (\wallet -> return $ walletExtractFunds wallet selector)

mkWalletFundStore :: WalletRef -> FundToStore
mkWalletFundStore walletRef funds = modifyWalletRef walletRef
  $ \wallet -> return (foldl (flip walletInsertFund) wallet funds, ())

--TODO use Error monad
sourceToStoreTransaction ::
     TxGenerator era
  -> FundSource
  -> ([Lovelace] -> [Lovelace])
  -> ToUTxO era
  -> FundToStore
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

includeChange :: Lovelace -> [Lovelace] -> [Lovelace] -> [Lovelace]
includeChange fee spend have = case compare changeValue 0 of
  GT -> changeValue : spend
  EQ -> spend
  LT -> error "genTX: Bad transaction: insufficient funds"
  where changeValue = sum have - sum spend - fee

mkUTxO :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> Validity
  -> ToUTxO era
mkUTxO = mkUTxOVariant PlainOldFund

mkUTxOVariant :: forall era. IsShelleyBasedEra era
  => Variant
  -> NetworkId
  -> SigningKey PaymentKey
  -> Validity
  -> ToUTxO era
mkUTxOVariant variant networkId key validity values
  = ( map mkTxOut values
    , newFunds
    )
 where
  mkTxOut v = TxOut (Tx.keyAddress @ era networkId key) (mkTxOutValueAdaOnly v) TxOutDatumHashNone

  newFunds txId = zipWith (mkNewFund txId) [TxIx 0 ..] values

  mkNewFund :: TxId -> TxIx -> Lovelace -> Fund
  mkNewFund txId txIx val = Fund $ InAnyCardanoEra (cardanoEra @ era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = mkTxOutValueAdaOnly val
    , _fundSigningKey = key
    , _fundValidity = validity
    , _fundVariant = variant
    }

genTx :: forall era. IsShelleyBasedEra era
  => TxFee era
  -> TxMetadataInEra era
  -> TxGenerator era
genTx fee metadata inFunds outputs
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signShelleyTransaction b (map (WitnessPaymentKey . getFundKey) inFunds)
                       , getTxId b
                       )
 where
  txBodyContent = TxBodyContent {
      txIns = map (\f -> (getFundTxIn f, BuildTxWith $ KeyWitness KeyWitnessForSpending)) inFunds
    , txInsCollateral = TxInsCollateralNone
    , txOuts = outputs
    , txFee = fee
    , txValidityRange = (TxValidityNoLowerBound, upperBound)
    , txMetadata = metadata
    , txAuxScripts = TxAuxScriptsNone
    , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    }

  upperBound :: TxValidityUpperBound era
  upperBound = case shelleyBasedEra @ era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra $ SlotNo maxBound
    ShelleyBasedEraAllegra -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
    ShelleyBasedEraMary    -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
    ShelleyBasedEraAlonzo  -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra

newtype WalletScript era = WalletScript { runWalletScript :: IO (WalletStep era) }

data WalletStep era
  = Done
  | NextTx !(WalletScript era) !(Tx era)
  | Error String

benchmarkWalletScript :: forall era .
     IsShelleyBasedEra era
  => WalletRef
  -> TxGenerator era
  -> NumberOfTxs
  -> (Target -> FundSource)
  -> ([Lovelace] -> [Lovelace])
  -> (Target -> SeqNumber -> ToUTxO era)
  -> FundToStore
  -> Target
  -> WalletScript era
benchmarkWalletScript wRef txGenerator (NumberOfTxs maxCount) fundSource inOut toUTxO fundToStore targetNode
  = WalletScript walletStep
 where
  nextCall = benchmarkWalletScript wRef txGenerator (NumberOfTxs maxCount) fundSource inOut toUTxO fundToStore targetNode

  walletStep :: IO (WalletStep era)
  walletStep = modifyMVarMasked wRef nextSeqNumber >>= \case
    Nothing -> return Done
    Just seqNumber -> do
      sourceToStoreTransaction txGenerator (fundSource targetNode) inOut (toUTxO targetNode seqNumber) fundToStore >>= \case
        Left err -> return $ Error err
        Right tx -> return $ NextTx nextCall tx

  nextSeqNumber :: Wallet -> IO (Wallet, Maybe SeqNumber)
  nextSeqNumber w = if n > SeqNumber (fromIntegral maxCount)
      then return (w, Nothing)
      else return (w {walletSeqNumber = succ n }, Just n)
    where n = walletSeqNumber w

limitSteps ::
     NumberOfTxs
  -> WalletScript era
  -> WalletScript era
limitSteps = undefined
