{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.TxSubmit.Tx
  ( TxSubmitError (..)
  , renderTxSubmitError
  , submitTx
  ) where

import Cardano.Prelude

import Cardano.Api
    ( LocalNodeConnectInfo,
      InAnyCardanoEra(InAnyCardanoEra),
      CardanoEra(ShelleyEra, ByronEra),
      ShelleyEra,
      ByronEra,
      TxId,
      getTxId,
      Tx,
      getTxBody,
      submitTxToNodeLocal,
      ConsensusModeParams(..))
import Cardano.TxSubmit.ErrorRender
    ( renderApplyMempoolPayloadErr, renderEraMismatch )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock )
import Ouroboros.Consensus.Cardano.Block
    ( EraMismatch (..), HardForkApplyTxErr (..) )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( ApplyTxErr )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( StandardCrypto )

import Cardano.Api.TxInMode (TxValidationErrorInMode(..), TxInMode(..), TxValidationError(..))
import Cardano.Api.Modes (EraInMode(..))
import Cardano.Api.IPC (localConsensusModeParams, SubmitResult(..))
import qualified Cardano.Ledger.Allegra as LedgerA
import qualified Cardano.Ledger.Mary as LedgerM
import qualified Cardano.Ledger.Shelley as LedgerS

-- | An error that can occur while submitting a transaction to a local node.
data TxSubmitError
  = TxSubmitByronError !(ApplyTxErr ByronBlock)
  | TxSubmitShelleyError !(ApplyTxErr (ShelleyBlock (LedgerS.ShelleyEra StandardCrypto)))
  | TxSubmitAllegraError !(ApplyTxErr (ShelleyBlock (LedgerA.AllegraEra StandardCrypto)))
  | TxSubmitMaryError !(ApplyTxErr (ShelleyBlock (LedgerM.MaryEra StandardCrypto)))
  | TxSubmitEraMismatchError !EraMismatch
  deriving (Eq, Show)

renderTxSubmitError :: TxSubmitError -> Text
renderTxSubmitError tse =
  case tse of
    TxSubmitByronError err -> renderApplyMempoolPayloadErr err
    TxSubmitShelleyError err -> show err -- TODO: Better rendering for Shelley errors
    TxSubmitAllegraError err -> show err -- TODO: Better rendering for Allegra errors
    TxSubmitMaryError err -> show err -- TODO: Better rendering for Mary errors
    TxSubmitEraMismatchError err -> renderEraMismatch err

blast :: a -> b
blast = undefined

-- | Submit a transaction to a local node.
submitTx
  :: forall mode.
     LocalNodeConnectInfo mode
  -> Either (Tx ByronEra) (Tx ShelleyEra)
  -> IO (Either TxSubmitError TxId)
submitTx connectInfo byronOrShelleyTx =
  case (localConsensusModeParams connectInfo, byronOrShelleyTx) of
    (ByronModeParams {}, Left tx) -> do
      result <- liftIO $ submitTxToNodeLocal connectInfo (TxInMode tx ByronEraInByronMode) -- (TxForByronMode tx)


    --  ByronEraInByronMode     :: EraInMode ByronEra   ByronMode

    --  ShelleyEraInShelleyMode :: EraInMode ShelleyEra ShelleyMode

    --  ByronEraInCardanoMode   :: EraInMode ByronEra   CardanoMode
    --  ShelleyEraInCardanoMode :: EraInMode ShelleyEra CardanoMode
    --  AllegraEraInCardanoMode :: EraInMode AllegraEra CardanoMode
    --  MaryEraInCardanoMode    :: EraInMode MaryEra    CardanoMode

      pure $ case result of
        SubmitSuccess -> Right (getTxIdForTx tx)
        SubmitFail err -> Left (TxSubmitByronError (blast err))

    (ByronModeParams {}, Right{}) ->
      pure $ Left $ TxSubmitEraMismatchError EraMismatch {
                ledgerEraName = "Byron",
                otherEraName  = "Shelley"
              }

    (ShelleyModeParams {}, Right tx) -> do
      result <- liftIO $ submitTxToNodeLocal connectInfo undefined -- (TxForShelleyMode tx)
      case result of
        SubmitSuccess -> pure $ Right (getTxIdForTx tx)
        SubmitFail err ->
          pure $ Left (TxSubmitShelleyError undefined {-err-})

    (ShelleyModeParams {}, Left{}) ->
      pure $ Left $ TxSubmitEraMismatchError EraMismatch {
                ledgerEraName = "Shelley",
                otherEraName  = "Byron"
              }

    (CardanoModeParams {}, tx) -> do
      let txInEra = either (InAnyCardanoEra ByronEra) (InAnyCardanoEra ShelleyEra) tx
      result <- submitTxToNodeLocal connectInfo undefined {- (TxForCardanoMode txInEra)-}
      pure $ case result of
        SubmitSuccess -> Right (either getTxIdForTx getTxIdForTx tx)
        SubmitFail (TxValidationEraMismatch mismatch) ->
          Left (TxSubmitEraMismatchError mismatch)
        SubmitFail (TxValidationErrorInMode (ByronTxValidationError byronBlock) _eraInMode) ->
          Left (TxSubmitByronError byronBlock)
        SubmitFail (TxValidationErrorInMode (ShelleyTxValidationError era applyTxErr) _eraInMode) ->
          Left (TxSubmitShelleyError applyTxErr)
          

        SubmitFail _ {-(ApplyTxErrByron err)-} ->
          Left (TxSubmitByronError undefined {- err -})
        SubmitFail _ {-TxSubmitFailureCardanoMode (ApplyTxErrShelley err) -} ->
          Left (TxSubmitShelleyError undefined {- err -})
        SubmitFail _ {- TxSubmitFailureCardanoMode (ApplyTxErrAllegra err) -} ->
          Left (TxSubmitAllegraError undefined {- err -})
        SubmitFail _ {- TxSubmitFailureCardanoMode (ApplyTxErrMary err) -} ->
          Left (TxSubmitMaryError undefined {- err -})
    --     ByronTxValidationError
    --    :: Consensus.ApplyTxErr Consensus.ByronBlock
    --    -> TxValidationError ByronEra

    --  ShelleyTxValidationError
    --    :: ShelleyBasedEra era
    --    -> Consensus.ApplyTxErr (Consensus.ShelleyBlock (ShelleyLedgerEra era))
    --    -> TxValidationError era

-- TODO: This function should really be implemented in `Cardano.Api.Typed`.
-- The function, 'Cardano.Api.Typed.getTxId', accepts a 'TxBody' parameter.
getTxIdForTx :: Tx era -> TxId
getTxIdForTx = getTxId . getTxBody
