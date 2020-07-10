{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Api.TxSubmit
  ( submitTx
  , TxForMode(..)
  , TxSubmitResultForMode(..)
  , renderTxSubmitResult
  ) where

import           Cardano.Prelude

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, mkShelleyTx)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Consensus.Cardano.Block
                   (GenTx (GenTxByron, GenTxShelley),
                    CardanoApplyTxErr, HardForkApplyTxErr (ApplyTxErrByron,
                      ApplyTxErrShelley, ApplyTxErrWrongEra))

import           Cardano.Api.Typed
import           Cardano.Api.TxSubmit.ErrorRender


data TxForMode mode where

     TxForByronMode
       :: Tx Byron
       -> TxForMode ByronMode

     TxForShelleyMode
       :: Tx Shelley
       -> TxForMode ShelleyMode

     TxForCardanoMode
       :: Either (Tx Byron) (Tx Shelley)
       -> TxForMode CardanoMode


data TxSubmitResultForMode mode where

     TxSubmitSuccess
       :: TxSubmitResultForMode mode

     TxSubmitFailureByronMode
       :: ApplyTxErr ByronBlock
       -> TxSubmitResultForMode ByronMode

     TxSubmitFailureShelleyMode
       :: ApplyTxErr (ShelleyBlock TPraosStandardCrypto)
       -> TxSubmitResultForMode ShelleyMode

     TxSubmitFailureCardanoMode
       :: CardanoApplyTxErr TPraosStandardCrypto
       -> TxSubmitResultForMode CardanoMode

deriving instance Show (TxSubmitResultForMode ByronMode)
deriving instance Show (TxSubmitResultForMode ShelleyMode)
deriving instance Show (TxSubmitResultForMode CardanoMode)

submitTx :: forall mode block.
            LocalNodeConnectInfo mode block
         -> TxForMode mode
         -> IO (TxSubmitResultForMode mode)
submitTx connctInfo txformode =
    case (localNodeConsensusMode connctInfo, txformode) of
      (ByronMode{}, TxForByronMode (ByronTx tx)) -> do
        let genTx = Byron.ByronTx (Byron.byronIdTx tx) tx
        result <- submitTxToNodeLocal connctInfo genTx
        case result of
          SubmitSuccess      -> return TxSubmitSuccess
          SubmitFail failure -> return (TxSubmitFailureByronMode failure)

      (ShelleyMode{}, TxForShelleyMode (ShelleyTx tx)) -> do
        let genTx = mkShelleyTx tx
        result <- submitTxToNodeLocal connctInfo genTx
        case result of
          SubmitSuccess      -> return TxSubmitSuccess
          SubmitFail failure -> return (TxSubmitFailureShelleyMode failure)

      (CardanoMode{}, TxForCardanoMode etx) -> do
        let genTx = case etx of
              Left  (ByronTx   tx) -> GenTxByron (Byron.ByronTx (Byron.byronIdTx tx) tx)
              Right (ShelleyTx tx) -> GenTxShelley (mkShelleyTx tx)
        result <- submitTxToNodeLocal connctInfo genTx
        case result of
          SubmitSuccess      -> return TxSubmitSuccess
          SubmitFail failure -> return (TxSubmitFailureCardanoMode failure)


renderTxSubmitResult :: TxSubmitResultForMode mode -> Text
renderTxSubmitResult res =
  case res of
    TxSubmitSuccess -> "Transaction submitted successfully."

    TxSubmitFailureByronMode err ->
      "Failed to submit Byron transaction: " <> renderApplyMempoolPayloadErr err

    TxSubmitFailureShelleyMode err ->
      -- TODO: Write render function for Shelley tx submission errors.
      "Failed to submit Shelley transaction: " <> show err

    TxSubmitFailureCardanoMode (ApplyTxErrByron err) ->
      "Failed to submit Byron transaction: " <> renderApplyMempoolPayloadErr err

    TxSubmitFailureCardanoMode (ApplyTxErrShelley err) ->
      -- TODO: Write render function for Shelley tx submission errors.
      "Failed to submit Shelley transaction: " <> show err

    TxSubmitFailureCardanoMode (ApplyTxErrWrongEra mismatch) ->
      "Failed to submit transaction due to era mismatch: " <> show mismatch
