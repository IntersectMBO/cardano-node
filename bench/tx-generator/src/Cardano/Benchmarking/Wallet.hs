{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Benchmarking.Wallet
where
import           Prelude

import           Data.Maybe
import           Control.Concurrent.MVar

import           Cardano.Api

import           Cardano.Benchmarking.FundSet as FundSet
import           Cardano.Benchmarking.Types (NumberOfTxs (..))
import           Cardano.Api.Shelley (ProtocolParameters, ReferenceScript(..))
type WalletRef = MVar Wallet

type TxGenerator era = [Fund] -> [TxOut CtxTx era] -> Either String (Tx era, TxId)

type ToUTxO era = Lovelace -> (TxOut CtxTx era, TxIx -> TxId -> Fund)

data Wallet = Wallet {
    walletSeqNumber :: !SeqNumber
  , walletFunds :: !FundSet
  }

initWallet :: IO (MVar Wallet)
initWallet = newMVar $ Wallet {
    walletSeqNumber = SeqNumber 1
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
  -> [ToUTxO era]
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
      outs = zipWith ($) mkTxOut outValues
    case txGenerator inputFunds $ map fst outs of
        Left err -> return $ Left err
        Right (tx, txId) -> do
          let
            fkt :: (a, TxIx -> TxId -> Fund) -> TxIx -> Fund
            fkt a txIx = snd a txIx txId
          fundToStore $ zipWith fkt outs [TxIx 0 ..]
          return $ Right tx

includeChange :: Lovelace -> [Lovelace] -> [Lovelace] -> [Lovelace]
includeChange fee spend have = case compare changeValue 0 of
  GT -> changeValue : spend
  EQ -> spend
  LT -> error "genTX: Bad transaction: insufficient funds"
  where changeValue = sum have - sum spend - fee

mkUTxOVariant :: forall era. IsShelleyBasedEra era
  => Variant
  -> NetworkId
  -> SigningKey PaymentKey
  -> Validity
  -> ToUTxO era
mkUTxOVariant variant networkId key validity value
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
    , _fundValidity = validity
    , _fundVariant = variant
    }

-- to be merged with mkUTxOVariant
mkUTxOScript :: forall era.
     IsShelleyBasedEra era
  => NetworkId
  -> (Script PlutusScriptV1, ScriptData)
  -> Witness WitCtxTxIn era
  -> Validity
  -> ToUTxO era
mkUTxOScript networkId (script, txOutDatum) witness validity value
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
    , _fundValidity = validity
    , _fundVariant = PlutusScriptFund
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

newtype WalletScript era = WalletScript { runWalletScript :: IO (WalletStep era) }

data WalletStep era
  = Done
  | NextTx !(WalletScript era) !(Tx era)
  | Error String

-- TODO:
-- use explicit tx- counter for each walletscript
-- Do not rely on global walletSeqNum
benchmarkWalletScript :: forall era .
     IsShelleyBasedEra era
  => WalletRef
  -> TxGenerator era
  -> NumberOfTxs
  -> (Target -> FundSource)
  -> ([Lovelace] -> [Lovelace])
  -> ( Target -> SeqNumber -> [ToUTxO era])
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

keyAddress :: forall era. IsShelleyBasedEra era => NetworkId -> SigningKey PaymentKey -> AddressInEra era
keyAddress networkId k
  = makeShelleyAddressInEra
      networkId
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
      NoStakeAddress
