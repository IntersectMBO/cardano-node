{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Benchmarking.Wallet
where
import           Prelude

import           Data.IxSet.Typed as IxSet
import           Data.Proxy
import           Control.Concurrent.MVar

import           Cardano.Api

import           Cardano.Benchmarking.Types (NumberOfOutputsPerTx(..), NumberOfTxs(..))
import           Cardano.Benchmarking.GeneratorTx.Tx as Tx hiding (Fund)
import           Cardano.Benchmarking.FundSet as FundSet

type WalletRef = MVar Wallet

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

walletRefInsertFund :: WalletRef -> Fund -> IO ()
walletRefInsertFund ref fund = modifyMVar_  ref $ \w -> return $ walletInsertFund w fund

walletInsertFund :: Wallet -> Fund -> Wallet
walletInsertFund w f
  = w { walletFunds = FundSet.insertFund (walletFunds w) f }

walletDeleteFund :: Wallet -> Fund -> Wallet
walletDeleteFund w f
  = w { walletFunds = FundSet.deleteFund (walletFunds w) f }

walletUpdateFunds :: Wallet -> [Fund] -> [Fund] -> Wallet
walletUpdateFunds w add del
  = foldl walletInsertFund w2 add
 where w2 = foldl walletDeleteFund w del

walletRefCreateCoins :: forall era. IsShelleyBasedEra era
  => WalletRef
  -> [Lovelace]
  -> IO (Either String (Tx era))
walletRefCreateCoins ref coins
  = modifyMVar ref $ \w -> case walletCreateCoins w coins of
     Right (newWallet, tx) -> return (newWallet, Right tx)
     Left err -> return (w, Left err)

walletCreateCoins :: forall era. IsShelleyBasedEra era
  => Wallet
  -> [Lovelace]
  -> Either String (Wallet, Tx era)
walletCreateCoins wallet genValues = do
  inputCoin <- findSufficientCoin (walletFunds wallet) (sum genValues)
  let outValues = includeChange [getFundLovelace inputCoin] genValues
  (tx, txId) <- genTx (walletKey wallet) (walletNetworkId wallet) [inputCoin] outValues
  let newFunds = zipWith (mkNewFund txId) [TxIx 0 ..] outValues
  Right (walletUpdateFunds wallet newFunds [inputCoin] , tx)
 where
  mkNewFund :: TxId -> TxIx -> Lovelace -> Fund
  mkNewFund txId txIx val = Fund $ InAnyCardanoEra (cardanoEra @ era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = mkTxOutValueAdaOnly val
    , _fundSigningKey = walletKey wallet
    , _fundValidity = Confirmed
    }

  findSufficientCoin :: FundSet -> Lovelace -> Either String Fund
  findSufficientCoin fs minValue = case coins of
    [] -> Left $ "findSufficientCoin: no single coin with min value >= " ++ show minValue
    (c:_) -> Right c
    where coins = toAscList ( Proxy :: Proxy Lovelace) (fs @= IsConfirmed @>= minValue)

includeChange :: [Lovelace] -> [Lovelace] -> [Lovelace]
includeChange have spend = case compare changeValue 0 of
  GT -> changeValue : spend
  EQ -> spend
  LT -> error "genTX: Bad transaction: insufficient funds"
  where changeValue = sum have - sum spend

-- genTx assumes that inFunds and outValues are of equal value.
genTx :: forall era. IsShelleyBasedEra era
  => SigningKey PaymentKey
  -> NetworkId
  -> [Fund]
  -> [Lovelace]
  -> Either String (Tx era, TxId)
genTx key networkId inFunds outValues
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signShelleyTransaction b (map (WitnessPaymentKey . getFundKey) inFunds)
                       , getTxId b
                       )
 where
  txBodyContent = TxBodyContent {
      txIns = map (\f -> (getFundTxIn f, BuildTxWith $ KeyWitness KeyWitnessForSpending)) inFunds
    , txInsCollateral = TxInsCollateralNone
    , txOuts = map mkTxOut outValues
    , txFee = mkFee 0
    , txValidityRange = (TxValidityNoLowerBound, upperBound)
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    }

  mkTxOut v = TxOut (Tx.keyAddress @ era networkId key) (mkTxOutValueAdaOnly v) TxOutDatumHashNone

  upperBound :: TxValidityUpperBound era
  upperBound = case shelleyBasedEra @ era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra $ SlotNo maxBound
    ShelleyBasedEraAllegra -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
    ShelleyBasedEraMary    -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
    ShelleyBasedEraAlonzo  -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra

benchmarkTransaction :: forall era. IsShelleyBasedEra era
  => Wallet
  -> Int
  -> Target
  -> Either String (Wallet, Tx era)
benchmarkTransaction wallet numInputs targetNode =
  walletTransaction wallet numInputs targetNode fundsToTx
 where
  fundsToTx w inputFunds outValues
    = genTx (walletKey w) (walletNetworkId w) inputFunds outValues

walletTransaction :: forall era. IsShelleyBasedEra era
  => Wallet
  -> Int
  -> Target
  -> (Wallet -> [Fund] -> [Lovelace] -> Either String (Tx era, TxId) )
  -> Either String (Wallet, Tx era)
walletTransaction wallet numInputs targetNode fundsToTx = do
  inputFunds <- findInputFunds (walletFunds wallet) targetNode
  let outValues = map getFundLovelace inputFunds
  (tx, txId) <- fundsToTx wallet inputFunds outValues
  let
    newFunds = zipWith (mkNewFund txId) [TxIx 0 ..] outValues
    newWallet = (walletUpdateFunds wallet newFunds inputFunds) {walletSeqNumber = newSeqNumber}
  Right (newWallet , tx)
 where
  newSeqNumber = succ $ walletSeqNumber wallet

  mkNewFund :: TxId -> TxIx -> Lovelace -> Fund
  mkNewFund txId txIx val = Fund $ InAnyCardanoEra (cardanoEra @ era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = mkTxOutValueAdaOnly val
    , _fundSigningKey = walletKey wallet
    , _fundValidity = InFlight targetNode newSeqNumber
    }

  findInputFunds :: FundSet -> Target -> Either String [Fund]
  findInputFunds fs _target =
    if length coins == numInputs
      then Right coins
      else Left "could not find enough input coins"
    where
      -- Just take confirmed coins.
      -- TODO: extend this to unconfimed coins to the same target node
      coins = take numInputs $ toAscList ( Proxy :: Proxy Lovelace) (fs @= IsConfirmed)

newtype WalletScript era = WalletScript { runWalletScript :: IO (WalletStep era) }

data WalletStep era
  = Done
  | NextTx !(WalletScript era) !(Tx era)
  | Error String

benchmarkWalletScript :: forall era .
     IsShelleyBasedEra era
  => WalletRef
  -> NumberOfTxs
  -> NumberOfOutputsPerTx
  -- in this version : numberOfInputs == numberOfOutputs
  -> Target
  -> WalletScript era
benchmarkWalletScript wRef (NumberOfTxs maxCount) (NumberOfOutputsPerTx numInputs) targetNode
  = WalletScript (modifyMVarMasked wRef nextTx)
  where
    nextCall = benchmarkWalletScript wRef (NumberOfTxs maxCount) (NumberOfOutputsPerTx numInputs) targetNode
    nextTx :: Wallet -> IO (Wallet, WalletStep era)
    nextTx w = if walletSeqNumber w > SeqNumber (fromIntegral maxCount)
      then return (w, Done)
      else case benchmarkTransaction w numInputs targetNode of
        Right (wNew, tx) -> return (wNew, NextTx nextCall tx)
        Left err -> return (w, Error err)
