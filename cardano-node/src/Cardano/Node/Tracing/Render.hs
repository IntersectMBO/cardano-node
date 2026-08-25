{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Tracing.Render
  ( -- * Consensus-type rendering (re-exported from ouroboros-consensus:tracing)
    module Ouroboros.Consensus.Tracing.Render
    -- * cardano-api-dependent rendering (kept here, as cardano-api is
    -- downstream of ouroboros-consensus)
  , renderScriptHash
  , renderScriptIntegrityHash
  , renderScriptPurpose
  , renderMissingRedeemers
  , renderIncompleteWithdrawals
  ) where

import qualified Cardano.Api as Api

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..), AsItem (..),
                   PlutusPurpose)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import           Cardano.Ledger.BaseTypes (Mismatch (..), Relation (..))
import           Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Hashes as Hashes
import           Ouroboros.Consensus.Tracing.Render

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NonEmptyMap
import           Data.Text (Text)

renderScriptIntegrityHash :: Maybe Alonzo.ScriptIntegrityHash -> Aeson.Value
renderScriptIntegrityHash (Just witPPDataHash) =
  Aeson.String . Crypto.hashToTextAsHex $ Hashes.extractHash witPPDataHash
renderScriptIntegrityHash Nothing = Aeson.Null


renderMissingRedeemers :: forall era. ()
  => Api.ShelleyBasedEra era
  -> NonEmpty (PlutusPurpose AsItem (Api.ShelleyLedgerEra era), Ledger.ScriptHash)
  -> Aeson.Value
renderMissingRedeemers sbe scripts = Aeson.object $ NonEmpty.toList $ NonEmpty.map renderTuple scripts
  where
    renderTuple :: ()
      => (PlutusPurpose AsItem (Api.ShelleyLedgerEra era), Ledger.ScriptHash)
      -> Aeson.Pair
    renderTuple (scriptPurpose, sHash) =
      Aeson.fromText (renderScriptHash sHash) .= renderScriptPurpose sbe scriptPurpose

renderIncompleteWithdrawals :: forall payload. Show payload
  => NonEmptyMap Ledger.AccountAddress (Mismatch RelEQ payload)
  -> Aeson.Value
renderIncompleteWithdrawals payload =
  Aeson.object $ map renderTuple $ NonEmptyMap.toList payload
  where
    renderTuple :: (Ledger.AccountAddress, Mismatch RelEQ payload) -> Aeson.Pair
    renderTuple (address, mismatch) =
      Aeson.fromText (Api.serialiseAddress $ Api.fromShelleyStakeAddr address) .= show mismatch

renderScriptHash :: Ledger.ScriptHash -> Text
renderScriptHash = Api.serialiseToRawBytesHexText . Api.fromShelleyScriptHash

renderScriptPurpose :: ()
  => Api.ShelleyBasedEra era
  -> PlutusPurpose AsItem (Api.ShelleyLedgerEra era)
  -> Aeson.Value
renderScriptPurpose sbe =
  Api.forEraInEon
    (Api.toCardanoEra sbe)
    (const Aeson.Null)
    (\case
      Api.AlonzoEraOnwardsAlonzo -> renderAlonzoPlutusPurpose
      Api.AlonzoEraOnwardsBabbage -> renderAlonzoPlutusPurpose
      Api.AlonzoEraOnwardsConway -> renderConwayPlutusPurpose
      -- TODO: fix
      Api.AlonzoEraOnwardsDijkstra -> undefined
    )

renderAlonzoPlutusPurpose :: ()
  => Aeson.ToJSON (Ledger.TxCert era)
  => AlonzoPlutusPurpose AsItem era
  -> Aeson.Value
renderAlonzoPlutusPurpose = \case
  AlonzoSpending (AsItem txin) ->
    Aeson.object ["spending" .= Api.fromShelleyTxIn txin]
  AlonzoMinting pid ->
    Aeson.object ["minting" .= Aeson.toJSON pid]
  AlonzoWithdrawing (AsItem rwdAcct) ->
    Aeson.object ["rewarding" .= Aeson.String (Api.serialiseAddress $ Api.fromShelleyStakeAddr rwdAcct)]
  AlonzoCertifying cert ->
    Aeson.object ["certifying" .= Aeson.toJSON cert]

renderConwayPlutusPurpose :: ()
  => (Ledger.EraPParams era, Aeson.ToJSON (Ledger.TxCert era))
  => ConwayPlutusPurpose AsItem era
  -> Aeson.Value
renderConwayPlutusPurpose = \case
  ConwaySpending (AsItem txin) ->
    Aeson.object ["spending" .= Api.fromShelleyTxIn txin]
  ConwayMinting pid ->
    Aeson.object ["minting" .= Aeson.toJSON pid]
  ConwayWithdrawing (AsItem rwdAcct) ->
    Aeson.object ["rewarding" .= Aeson.String (Api.serialiseAddress $ Api.fromShelleyStakeAddr rwdAcct)]
  ConwayCertifying cert ->
    Aeson.object ["certifying" .= Aeson.toJSON cert]
  ConwayVoting voter ->
    Aeson.object ["voting" .= Aeson.toJSON voter]
  ConwayProposing proposal ->
    Aeson.object ["proposing" .= Aeson.toJSON proposal]
