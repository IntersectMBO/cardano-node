{-# LANGUAGE GADTs #-}
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
type TxGenerator era = [Fund] -> [Lovelace] -> Validity -> Either String (Tx era, [Fund])

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

modifyWalletRefEither :: WalletRef -> (Wallet -> Either err (Wallet,a)) -> IO (Either err a)
modifyWalletRefEither ref action
  = modifyMVar ref $ \w -> case action w of
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

walletUpdateFunds :: [Fund] -> [Fund] -> Wallet -> Wallet
walletUpdateFunds add del w
  = foldl (flip walletInsertFund) w2 add
 where w2 = foldl (flip walletDeleteFund) w del

walletSelectFunds :: Wallet -> FundSelector -> Either String [Fund]
walletSelectFunds w s = s $ walletFunds w

walletExtractFunds :: Wallet -> FundSelector -> Either String (Wallet, [Fund])
walletExtractFunds w s
  = case walletSelectFunds w s of
    Left err -> Left err
    Right funds -> Right (walletUpdateFunds [] funds w, funds)

walletCreateCoins ::
     TxGenerator era
  -> FundSelector
  -> ([Lovelace] -> [Lovelace])
  -> Wallet
  -> Either String (Wallet, Tx era)
walletCreateCoins txGenerator selector inOut wallet = do
  inputFunds <- selector (walletFunds wallet)
  (tx, newFunds) <- txGenerator inputFunds (inOut $ map getFundLovelace inputFunds) Confirmed
  Right (walletUpdateFunds newFunds inputFunds wallet, tx)

includeChange :: Lovelace -> [Lovelace] -> [Lovelace] -> [Lovelace]
includeChange fee spend have = case compare changeValue 0 of
  GT -> changeValue : spend
  EQ -> spend
  LT -> error "genTX: Bad transaction: insufficient funds"
  where changeValue = sum have - sum spend - fee

-- genTx assumes that inFunds and outValues are of equal value.
genTx :: forall era. IsShelleyBasedEra era
  => SigningKey PaymentKey
  -> NetworkId
  -> TxFee era
  -> TxMetadataInEra era
  -> TxGenerator era
genTx key networkId fee metadata inFunds outValues validity
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signShelleyTransaction b (map (WitnessPaymentKey . getFundKey) inFunds)
                       , newFunds $ getTxId b
                       )
 where
  txBodyContent = TxBodyContent {
      txIns = map (\f -> (getFundTxIn f, BuildTxWith $ KeyWitness KeyWitnessForSpending)) inFunds
    , txInsCollateral = TxInsCollateralNone
    , txOuts = map mkTxOut outValues
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
    , txScriptValidity = BuildTxWith TxScriptValidityNone
    }

  mkTxOut v = TxOut (Tx.keyAddress @ era networkId key) (mkTxOutValueAdaOnly v) TxOutDatumHashNone

  upperBound :: TxValidityUpperBound era
  upperBound = case shelleyBasedEra @ era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra $ SlotNo maxBound
    ShelleyBasedEraAllegra -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
    ShelleyBasedEraMary    -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
    ShelleyBasedEraAlonzo  -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra

  newFunds txId = zipWith (mkNewFund txId) [TxIx 0 ..] outValues

  mkNewFund :: TxId -> TxIx -> Lovelace -> Fund
  mkNewFund txId txIx val = Fund $ InAnyCardanoEra (cardanoEra @ era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = mkTxOutValueAdaOnly val
    , _fundSigningKey = key
    , _fundValidity = validity
    , _fundVariant = PlainOldFund
    }

benchmarkTransaction ::
     Wallet
  -> FundSelector
  -> ([Lovelace] -> [Lovelace])
  -> TxGenerator era
  -> Target
  -> Either String (Wallet, Tx era)
benchmarkTransaction wallet selector inOut txGenerator targetNode = do
  inputFunds <- selector (walletFunds wallet)
  let outValues = inOut $ map getFundLovelace inputFunds
  (tx, newFunds) <- txGenerator inputFunds outValues $ InFlight targetNode newSeqNumber
  let
    newWallet = (walletUpdateFunds newFunds inputFunds wallet) {walletSeqNumber = newSeqNumber}
  Right (newWallet , tx)
 where
  newSeqNumber = succ $ walletSeqNumber wallet

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
  -> (Target -> FundSelector)
  -> ([Lovelace] -> [Lovelace])
  -> Target
  -> WalletScript era
benchmarkWalletScript wRef txGenerator (NumberOfTxs maxCount) selector inOut targetNode
  = WalletScript (modifyMVarMasked wRef nextTx)
 where
  nextCall = benchmarkWalletScript wRef txGenerator (NumberOfTxs maxCount) selector inOut targetNode

  nextTx :: Wallet -> IO (Wallet, WalletStep era)
  nextTx w = if walletSeqNumber w > SeqNumber (fromIntegral maxCount)
    then return (w, Done)
    else case benchmarkTransaction w (selector targetNode) inOut txGenerator targetNode of
      Right (wNew, tx) -> return (wNew, NextTx nextCall tx)
      Left err -> return (w, Error err)

limitSteps ::
     NumberOfTxs
  -> WalletScript era
  -> WalletScript era
limitSteps = undefined
