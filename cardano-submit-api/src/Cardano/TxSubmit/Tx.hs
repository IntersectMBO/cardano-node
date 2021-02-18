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

import Cardano.Api.TxSubmit hiding
    ( submitTx )
import Cardano.Api
    ( InAnyCardanoEra(InAnyCardanoEra),
      CardanoEra(ShelleyEra, ByronEra),
      ShelleyEra,
      ByronEra,
      TxId,
      getTxId,
      Tx,
      getTxBody,
      LocalNodeConnectInfo(localNodeConsensusMode) )
import Cardano.Api.Typed
    ( NodeConsensusMode (..)
    )
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

import qualified Cardano.Api.TxSubmit as Api
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

-- | Submit a transaction to a local node.
submitTx
  :: forall mode block.
     LocalNodeConnectInfo mode block
  -> Either (Tx ByronEra) (Tx ShelleyEra)
  -> IO (Either TxSubmitError TxId)
submitTx connectInfo byronOrShelleyTx =
  case (localNodeConsensusMode connectInfo, byronOrShelleyTx) of
    (ByronMode{}, Left tx) -> do
      result <- liftIO $ Api.submitTx connectInfo (TxForByronMode tx)
      pure $ case result of
        TxSubmitSuccess -> Right (getTxIdForTx tx)
        TxSubmitFailureByronMode err -> Left (TxSubmitByronError err)

    (ByronMode{}, Right{}) ->
      pure $ Left $ TxSubmitEraMismatchError EraMismatch {
                ledgerEraName = "Byron",
                otherEraName  = "Shelley"
              }

    (ShelleyMode{}, Right tx) -> do
      result <- liftIO $ Api.submitTx connectInfo (TxForShelleyMode tx)
      case result of
        TxSubmitSuccess -> pure $ Right (getTxIdForTx tx)
        TxSubmitFailureShelleyMode err ->
          pure $ Left (TxSubmitShelleyError err)

    (ShelleyMode{}, Left{}) ->
      pure $ Left $ TxSubmitEraMismatchError EraMismatch {
                ledgerEraName = "Shelley",
                otherEraName  = "Byron"
              }

    (CardanoMode{}, tx) -> do
      let txInEra = either (InAnyCardanoEra ByronEra) (InAnyCardanoEra ShelleyEra) tx
      result <- Api.submitTx connectInfo (TxForCardanoMode txInEra)
      pure $ case result of
        TxSubmitSuccess -> Right (either getTxIdForTx getTxIdForTx tx)
        TxSubmitFailureCardanoMode (ApplyTxErrByron err) ->
          Left (TxSubmitByronError err)
        TxSubmitFailureCardanoMode (ApplyTxErrShelley err) ->
          Left (TxSubmitShelleyError err)
        TxSubmitFailureCardanoMode (ApplyTxErrAllegra err) ->
          Left (TxSubmitAllegraError err)
        TxSubmitFailureCardanoMode (ApplyTxErrMary err) ->
          Left (TxSubmitMaryError err)
        TxSubmitFailureCardanoMode (ApplyTxErrWrongEra mismatch) ->
          Left (TxSubmitEraMismatchError mismatch)

-- TODO: This function should really be implemented in `Cardano.Api.Typed`.
-- The function, 'Cardano.Api.Typed.getTxId', accepts a 'TxBody' parameter.
getTxIdForTx :: Tx era -> TxId
getTxIdForTx = getTxId . getTxBody
