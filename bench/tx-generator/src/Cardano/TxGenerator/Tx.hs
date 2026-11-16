{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.Tx
        (module Cardano.TxGenerator.Tx)
        where

import           Cardano.Api hiding (txId)
import           Cardano.Api.Compatible (CompatibleTxBodyContent (..), CompatibleTxError,
                   createCompatibleTx, defaultCompatibleTxBodyContent)
import qualified Cardano.Api.Compatible as Compatible (addWitnesses)
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp

import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Core as L (PParams, addrTxWitsL, bodyTxL, txIdTx, witsTxL)
import           Cardano.Ledger.Tools (estimateMinFeeTx)
import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utils (txIdFromSignedTx)

import           Data.Bifunctor (first)
import qualified Data.ByteString as BS (length)
import           Data.Function ((&))
import           Data.List.Extra (nubOrd)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Lens.Micro ((.~), (^.))


-- | 'genTx' builds and signs a transaction with
-- 'Cardano.Api.Compatible.createCompatibleTx', in any Shelley-based era.
-- Partially applied to its first 4 arguments, it is a 'TxGenerator': a
-- function from the funds to spend and the outputs to create.
genTx :: forall era. IsShelleyBasedEra era
  => L.PParams (ShelleyLedgerEra era)
  -> ([TxIn], [Fund])
  -- ^ Collateral inputs, and the funds they spend (for their signing keys)
  -> L.Coin
  -> TxMetadataInEra era
  -> TxGenerator era
genTx ledgerParameters (collateralIns, collFunds) fee metadata inFunds outputs =
  shelleyBasedEraConstraints sbe $ do
    let bodyContent = (defaultCompatibleTxBodyContent sbe)
          { compatibleTxIns = map (\f -> (getFundTxIn f, getFundWitness @era f)) inFunds
          , compatibleTxOuts = outputs
          , compatibleTxFee = fee
          , compatibleTxInsCollateral = collateralIns
          , compatibleTxProtocolParams = Just ledgerParameters
          , compatibleTxMetadata = metadata
          }
    unsignedTx@(ShelleyTx _ unsignedLedgerTx) <-
      first (\err -> TxGenError $ "genTx: " ++ show (err :: CompatibleTxError)) $
        createCompatibleTx sbe bodyContent
    let ledgerBody = unsignedLedgerTx ^. L.bodyTxL
        witVKeys = [ makeShelleyKeyWitness' sbe ledgerBody (WitnessPaymentKey key)
                   | key <- mapMaybe getFundKey $ inFunds ++ collFunds ]
        ShelleyTx _ signedLedgerTx = Compatible.addWitnesses witVKeys unsignedTx
        tx = Exp.SignedTx signedLedgerTx
    Right (tx, txIdFromSignedTx tx)
 where
  sbe = shelleyBasedEra @era

-- | 'genNestedTx' is 'genTx' for a Dijkstra transaction that also carries
-- sub-transactions. Each sub-transaction spends its funds into its outputs,
-- with no fee, and is signed with the keys of its own funds; the top-level
-- transaction pays the fee for all of them.
--
-- It builds with the experimental API rather than 'Cardano.Api.Compatible',
-- which has no sub-transactions yet. Once it does, 'genNestedTx' shall go,
-- and 'Cardano.Benchmarking.Script.Types.NestedNtoM' build with 'genTx'.
-- Until then, without sub-transactions it builds the same transaction as
-- 'genTx' (checked by the tests).
genNestedTx ::
     L.PParams (ShelleyLedgerEra DijkstraEra)
  -> ([TxIn], [Fund])
  -- ^ Collateral inputs, and the funds they spend (for their signing keys)
  -> L.Coin
  -> TxMetadata
  -> NestedTxGenerator DijkstraEra
genNestedTx ledgerParameters (collateralIns, collFunds) fee metadata inFunds outputs subTxs = do
  signedSubTxs <- mapM (uncurry signedSubTx) subTxs
  unsignedTx <- first ApiError $ Exp.makeUnsignedTx Exp.DijkstraEra $ Exp.defaultTxBodyContent
    & Exp.setTxIns (map (\f -> (getFundTxIn f, getFundWitness @DijkstraEra f)) inFunds)
    -- A fund whose script is supplied by reference needs that output among
    -- the transaction's reference inputs ('genTx' adds them itself).
    & Exp.setTxInsReference (Exp.TxInsReference referenceInputs Set.empty)
    & Exp.setTxInsCollateral collateralIns
    & Exp.setTxOuts outputs
    & Exp.setTxFee fee
    & Exp.setTxMetadata metadata
    & Exp.setTxProtocolParams ledgerParameters
    & Exp.setTxSignedSubTransactions signedSubTxs
  let keyWitnesses = map (Exp.makeKeyWitness Exp.DijkstraEra unsignedTx . WitnessPaymentKey) $
        mapMaybe getFundKey $ inFunds ++ collFunds
      tx@(Exp.SignedTx ledgerTx) = Exp.signTx Exp.DijkstraEra [] keyWitnesses unsignedTx
  return (tx, fromShelleyTxId $ L.txIdTx ledgerTx, map Exp.getSignedSubTxId signedSubTxs)
 where
  referenceInputs = nubOrd $ mapMaybe getFundReferenceInput inFunds
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
  -> Either TxGenError (Exp.SignedTx era, [(TxId, [out])])
nestedTx txOut txGenerator (topFunds, topOutputs) subTxs = do
  (tx, txId, subTxIds) <- txGenerator topFunds (map txOut topOutputs)
                            [ (funds, map txOut outputs) | (funds, outputs) <- subTxs ]
  return (tx, (txId, topOutputs) : zip subTxIds (map snd subTxs))

-- | The metadata of 'genNestedTx', from that of 'genTx'.
fromTxMetadataInEra :: TxMetadataInEra era -> TxMetadata
fromTxMetadataInEra TxMetadataNone = mempty
fromTxMetadataInEra (TxMetadataInEra _ metadata) = metadata

-- | 'estimateTxFee' estimates the minimum fee of a transaction, counting
-- @keyWitnesses@ key witnesses instead of the ones it already carries, so that
-- a signed transaction preview is estimated like its unsigned counterpart, and
-- @refScriptsSize@ bytes of the reference scripts it uses.
estimateTxFee :: forall era. IsShelleyBasedEra era
  => L.PParams (ShelleyLedgerEra era)
  -> Exp.SignedTx era
  -> Word
  -> Int
  -> L.Coin
estimateTxFee ledgerParameters (Exp.SignedTx ledgerTx) keyWitnesses refScriptsSize =
  shelleyBasedEraConstraints (shelleyBasedEra @era) $
    estimateMinFeeTx ledgerParameters (ledgerTx & L.witsTxL . L.addrTxWitsL .~ mempty)
      (fromIntegral keyWitnesses) 0 refScriptsSize

-- | The number of key witnesses a signed transaction carries: one for each
-- key among the funds it spends and its collateral, a key used twice once.
txKeyWitnesses :: forall era. IsShelleyBasedEra era => Exp.SignedTx era -> Word
txKeyWitnesses (Exp.SignedTx ledgerTx) = shelleyBasedEraConstraints (shelleyBasedEra @era) $
  fromIntegral $ Set.size $ ledgerTx ^. L.witsTxL . L.addrTxWitsL

txSizeInBytes :: forall era. IsShelleyBasedEra era =>
     Exp.SignedTx era
  -> Int
txSizeInBytes tx@(Exp.SignedTx _)
  = BS.length $ serialiseToRawBytes tx
