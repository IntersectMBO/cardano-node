{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Api.InMode.Export
  ( renderScriptIntegrityHash
  , renderScriptHash
  , renderMissingRedeemers
  , renderScriptPurpose
  , renderBadInputsUTxOErr
  , renderValueNotConservedErr
  , renderTxId
  ) where

import           Cardano.Api.Orphans ()
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Shelley.API hiding (ShelleyBasedEra)
import           Cardano.Prelude
import           Data.Aeson (ToJSON(..), Value(..), (.=), object)
import           Data.Aeson.Types (Pair)
import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)
import           Prelude (error)

import qualified Cardano.Api.Address as Api
import qualified Cardano.Api.Certificate as Api
import qualified Cardano.Api.Script as Api
import qualified Cardano.Api.SerialiseRaw as Api
import qualified Cardano.Api.SerialiseTextEnvelope as Api
import qualified Cardano.Api.TxBody as Api
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Consensus

renderScriptIntegrityHash :: Maybe (Alonzo.ScriptIntegrityHash StandardCrypto) -> Value
renderScriptIntegrityHash (Just witPPDataHash) =
  String . Crypto.hashToTextAsHex $ SafeHash.extractHash witPPDataHash
renderScriptIntegrityHash Nothing = Aeson.Null


renderScriptHash :: ScriptHash StandardCrypto -> Text
renderScriptHash = Api.serialiseToRawBytesHexText . Api.fromShelleyScriptHash

renderMissingRedeemers :: [(Alonzo.ScriptPurpose StandardCrypto, ScriptHash StandardCrypto)] -> Value
renderMissingRedeemers scripts = object $ map renderTuple  scripts
 where
  renderTuple :: (Alonzo.ScriptPurpose StandardCrypto, ScriptHash StandardCrypto) -> Pair
  renderTuple (scriptPurpose, sHash) =
    Aeson.fromText (renderScriptHash sHash) .= renderScriptPurpose scriptPurpose

renderScriptPurpose :: Alonzo.ScriptPurpose StandardCrypto -> Value
renderScriptPurpose (Alonzo.Minting pid) = object
  [ "minting" .= toJSON pid
  ]
renderScriptPurpose (Alonzo.Spending txin) = object
  [ "spending" .= Api.fromShelleyTxIn txin
  ]
renderScriptPurpose (Alonzo.Rewarding rwdAcct) = object
  [ "rewarding" .= String (Api.serialiseAddress $ Api.fromShelleyStakeAddr rwdAcct)
  ]
renderScriptPurpose (Alonzo.Certifying cert) = object
  [ "certifying" .= toJSON (Api.textEnvelopeDefaultDescr $ Api.fromShelleyCertificate cert)
  ]

renderBadInputsUTxOErr ::  Set (TxIn era) -> Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Value
renderValueNotConservedErr consumed produced = String $
  "This transaction consumed " <> show consumed <> " but produced " <> show produced

renderTxId :: Consensus.TxId (GenTx (ShelleyBlock era)) -> Text
renderTxId = error "TODO implement"
