{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.Tx
        (module Cardano.TxGenerator.Tx)
        where

import           Cardano.Api hiding (txId)
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp

import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Core as L (PParams, addrTxWitsL, txIdTx, witsTxL)
import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types

import           Data.Bifunctor (first)
import qualified Data.ByteString as BS (length)
import           Data.Function ((&))
import           Data.List.Extra (nubOrd)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Lens.Micro ((.~), (^.))


-- | 'genTx' builds and signs a transaction with the experimental API
-- ('Exp.makeUnsignedTx', 'Exp.makeKeyWitness' and 'Exp.signTx'), lifting
-- a 'Exp.MakeUnsignedTxError' to 'Cardano.TxGenerator.Types.TxGenError' as
-- an 'Cardano.TxGenerator.Types.ApiError' case. The signed ledger transaction
-- is returned as an old-API 'Tx', which is what the submission code consumes.
-- Partially applied to its first 4 arguments, it is a 'TxGenerator': a
-- function from the funds to spend and the outputs to create.
genTx :: forall era. ()
  => Exp.EraCommonConstraints era
  => L.PParams (ShelleyLedgerEra era)
  -> ([TxIn], [Fund])
  -- ^ Collateral inputs, and the funds they spend (for their signing keys)
  -> L.Coin
  -> TxMetadata
  -> TxGenerator era
genTx ledgerParameters collateral fee metadata inFunds outputs =
  signedTopLevelTx ledgerParameters collateral fee metadata inFunds outputs id

-- | 'genNestedTx' is 'genTx' for a Dijkstra transaction that also carries
-- sub-transactions. Each sub-transaction spends its funds into its outputs,
-- with no fee, and is signed with the keys of its own funds; the top-level
-- transaction pays the fee for all of them.
genNestedTx ::
     L.PParams (ShelleyLedgerEra DijkstraEra)
  -> ([TxIn], [Fund])
  -- ^ Collateral inputs, and the funds they spend (for their signing keys)
  -> L.Coin
  -> TxMetadata
  -> NestedTxGenerator DijkstraEra
genNestedTx ledgerParameters collateral fee metadata inFunds outputs subTxs = do
  signedSubTxs <- mapM (uncurry signedSubTx) subTxs
  (tx, txId) <- signedTopLevelTx ledgerParameters collateral fee metadata inFunds outputs
                  (Exp.setTxSignedSubTransactions signedSubTxs)
  return (tx, txId, map Exp.getSignedSubTxId signedSubTxs)
 where
  signedSubTx :: [Fund] -> [Exp.TxOut (ShelleyLedgerEra DijkstraEra)] -> Either TxGenError Exp.SignedSubTx
  signedSubTx subFunds subOutputs = do
    unsignedSubTx <- first ApiError $ Exp.makeUnsignedSubTx $ Exp.defaultSubTxBodyContent
      & Exp.setTxIns (map (\f -> (getFundTxIn f, getFundWitness @DijkstraEra f)) subFunds)
      & Exp.setTxOuts subOutputs
      & Exp.setTxProtocolParams ledgerParameters
    let keyWitnesses = map (Exp.makeSubTxKeyWitness unsignedSubTx . WitnessPaymentKey) $ mapMaybe getFundKey subFunds
    return $ Exp.signSubTx [] keyWitnesses unsignedSubTx

-- | 'nestedTx' builds a transaction with sub-transactions, from the funds and
-- outputs of the top-level transaction and of each sub-transaction, and pairs
-- each group of outputs with the id of the transaction that creates them: the
-- top-level transaction's own, then each sub-transaction's, as that is the id
-- their 'TxIn's carry. @txOut@ is the ledger output of an @out@.
nestedTx ::
     (out -> Exp.TxOut (ShelleyLedgerEra era))
  -> NestedTxGenerator era
  -> ([Fund], [out])
  -> [([Fund], [out])]
  -> Either TxGenError (Tx era, [(TxId, [out])])
nestedTx txOut txGenerator (topFunds, topOutputs) subTxs = do
  (tx, txId, subTxIds) <- txGenerator topFunds (map txOut topOutputs)
                            [ (funds, map txOut outputs) | (funds, outputs) <- subTxs ]
  return (tx, (txId, topOutputs) : zip subTxIds (map snd subTxs))

-- | Builds and signs a top-level transaction; @extendBody@ can add to its
-- body content, e.g. sub-transactions.
signedTopLevelTx :: forall era. ()
  => Exp.EraCommonConstraints era
  => L.PParams (ShelleyLedgerEra era)
  -> ([TxIn], [Fund])
  -> L.Coin
  -> TxMetadata
  -> [Fund]
  -> [Exp.TxOut (ShelleyLedgerEra era)]
  -> (Exp.TxBodyContent (ShelleyLedgerEra era) -> Exp.TxBodyContent (ShelleyLedgerEra era))
  -> Either TxGenError (Tx era, TxId)
signedTopLevelTx ledgerParameters (collateral, collFunds) fee metadata inFunds outputs extendBody = do
  unsignedTx <- first ApiError $ Exp.makeUnsignedTx era txBodyContent
  let keyWitnesses = map (Exp.makeKeyWitness era unsignedTx . WitnessPaymentKey) allKeys
  case Exp.signTx era [] keyWitnesses unsignedTx of
    Exp.SignedTx ledgerTx ->
      return (ShelleyTx (shelleyBasedEra @era) ledgerTx, fromShelleyTxId $ L.txIdTx ledgerTx)
 where
  era = Exp.useEra @era
  allKeys = mapMaybe getFundKey $ inFunds ++ collFunds
  -- A fund whose script is supplied by reference needs that output among the
  -- transaction's reference inputs.
  referenceInputs = nubOrd $ mapMaybe getFundReferenceInput inFunds
  txBodyContent = extendBody $ Exp.defaultTxBodyContent
    & Exp.setTxIns (map (\f -> (getFundTxIn f, getFundWitness @era f)) inFunds)
    & Exp.setTxInsReference (Exp.TxInsReference referenceInputs Set.empty)
    & Exp.setTxInsCollateral collateral
    & Exp.setTxOuts outputs
    & Exp.setTxFee fee
    & Exp.setTxMetadata metadata
    & Exp.setTxProtocolParams ledgerParameters


-- | 'estimateTxFee' estimates the minimum fee of a transaction, counting
-- @keyWitnesses@ key witnesses instead of the ones it already carries, so that
-- a signed transaction preview is estimated like its unsigned counterpart, and
-- @refScriptsSize@ bytes of the reference scripts it uses.
estimateTxFee :: Exp.EraCommonConstraints era
  => L.PParams (ShelleyLedgerEra era)
  -> Tx era
  -> Word
  -> Int
  -> L.Coin
estimateTxFee ledgerParameters (ShelleyTx _ ledgerTx) keyWitnesses =
  Exp.evaluateTransactionFee ledgerParameters unsignedTx keyWitnesses 0
 where
  unsignedTx = Exp.UnsignedTx $ ledgerTx & L.witsTxL . L.addrTxWitsL .~ mempty

-- | The number of key witnesses a signed transaction carries: one for each
-- key among the funds it spends and its collateral, a key used twice once.
txKeyWitnesses :: Exp.EraCommonConstraints era => Tx era -> Word
txKeyWitnesses (ShelleyTx _ ledgerTx) = fromIntegral $ Set.size $ ledgerTx ^. L.witsTxL . L.addrTxWitsL

txSizeInBytes :: forall era. IsShelleyBasedEra era =>
     Tx era
  -> Int
txSizeInBytes
  = BS.length . serialiseToCBOR
