{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TracingOrphanInstances.HardFork () where

import           Prelude

import           Data.Text (Text)
import           Data.SOP.Strict

import           Cardano.Config.Orphanage ()
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.TracingOrphanInstances.Common
import           Cardano.TracingOrphanInstances.Consensus ()

import           Ouroboros.Consensus.Block (BlockProtocol, ForgeState)
import           Ouroboros.Consensus.Protocol.Abstract
                   (ValidationErr, CannotLead)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.SupportsMempool
                   (GenTx, TxId, ApplyTxErr)
import           Ouroboros.Consensus.HeaderValidation
                   (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
                   (OneEraValidationErr(..), OneEraLedgerError(..),
                    OneEraEnvelopeErr(..), OneEraCannotLead(..),
                    EraMismatch(..), mkEraMismatch)
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.Util.Condense (Condense(..))


--
-- instances for hashes
--

instance Condense (OneEraHash xs) where
    condense = condense . Crypto.UnsafeHash . getOneEraHash


--
-- instances for Header HardForkBlock
--

instance All (Condense `Compose` Header) xs => Condense (Header (HardForkBlock xs)) where
    condense =
          hcollapse
        . hcmap (Proxy @ (Condense `Compose` Header)) (K . condense)
        . getOneEraHeader
        . getHardForkHeader

instance All (ToObject `Compose` Header) xs => ToObject (Header (HardForkBlock xs)) where
    toObject verb =
          hcollapse
        . hcmap (Proxy @ (ToObject `Compose` Header)) (K . toObject verb)
        . getOneEraHeader
        . getHardForkHeader


--
-- instances for GenTx HardForkBlock
--

instance All (Compose ToObject GenTx) xs => ToObject (GenTx (HardForkBlock xs)) where
    toObject verb =
          hcollapse
        . hcmap (Proxy @ (ToObject `Compose` GenTx)) (K . toObject verb)
        . getOneEraGenTx
        . getHardForkGenTx

instance  All (Compose ToJSON WrapGenTxId) xs => ToJSON (TxId (GenTx (HardForkBlock xs))) where
    toJSON =
          hcollapse
        . hcmap (Proxy @ (ToJSON `Compose` WrapGenTxId)) (K . toJSON)
        . getOneEraGenTxId
        . getHardForkGenTxId

instance ToJSON (TxId (GenTx blk)) => ToJSON (WrapGenTxId blk) where
    toJSON = toJSON . unwrapGenTxId


--
-- instances for GenTx HardForkBlock
--

{-
instance HasKESMetricsData (HardForkBlock xs) where
    getKESMetricsData :: ProtocolInfo m (HardForkBlock xs)
                      -> ForgeState (HardForkBlock xs)
                      -> KESMetricsData
    getKESMetricsData protoInfo forgeState =
-}

--
-- instances for HardForkApplyTxErr
--

instance All (ToObject `Compose` WrapApplyTxErr) xs => ToObject (HardForkApplyTxErr xs) where
    toObject verb (HardForkApplyTxErrFromEra err) = toObject verb err
    toObject _verb (HardForkApplyTxErrWrongEra mismatch) =
      mkObject
        [ "kind"       .= String "HardForkApplyTxErrWrongEra"
        , "currentEra" .= ledgerEraName
        , "txEra"      .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (ToObject `Compose` WrapApplyTxErr) xs => ToObject (OneEraApplyTxErr xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @ (ToObject `Compose` WrapApplyTxErr)) (K . toObject verb)
      . getOneEraApplyTxErr

instance ToObject (ApplyTxErr blk) => ToObject (WrapApplyTxErr blk) where
    toObject verb = toObject verb . unwrapApplyTxErr


--
-- instances for HardForkLedgerError
--

instance All (ToObject `Compose` WrapLedgerErr) xs => ToObject (HardForkLedgerError xs) where
    toObject verb (HardForkLedgerErrorFromEra err) = toObject verb err

    toObject _verb (HardForkLedgerErrorWrongEra mismatch) =
      mkObject
        [ "kind"       .= String "HardForkLedgerErrorWrongEra"
        , "currentEra" .= ledgerEraName
        , "blockEra"   .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (ToObject `Compose` WrapLedgerErr) xs => ToObject (OneEraLedgerError xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @ (ToObject `Compose` WrapLedgerErr)) (K . toObject verb)
      . getOneEraLedgerError

instance ToObject (LedgerError blk) => ToObject (WrapLedgerErr blk) where
    toObject verb = toObject verb . unwrapLedgerErr


--
-- instances for HardForkEnvelopeErr
--

instance All (ToObject `Compose` WrapEnvelopeErr) xs => ToObject (HardForkEnvelopeErr xs) where
    toObject verb (HardForkEnvelopeErrFromEra err) = toObject verb err

    toObject _verb (HardForkEnvelopeErrWrongEra mismatch) =
      mkObject
        [ "kind"       .= String "HardForkEnvelopeErrWrongEra"
        , "currentEra" .= ledgerEraName
        , "blockEra"   .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (ToObject `Compose` WrapEnvelopeErr) xs => ToObject (OneEraEnvelopeErr xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @ (ToObject `Compose` WrapEnvelopeErr)) (K . toObject verb)
      . getOneEraEnvelopeErr

instance ToObject (OtherHeaderEnvelopeError blk) => ToObject (WrapEnvelopeErr blk) where
    toObject verb = toObject verb . unwrapEnvelopeErr


--
-- instances for HardForkValidationErr
--

instance All (ToObject `Compose` WrapValidationErr) xs => ToObject (HardForkValidationErr xs) where
    toObject verb (HardForkValidationErrFromEra err) = toObject verb err

    toObject _verb (HardForkValidationErrWrongEra mismatch) =
      mkObject
        [ "kind"       .= String "HardForkValidationErrWrongEra"
        , "currentEra" .= ledgerEraName
        , "blockEra"   .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (ToObject `Compose` WrapValidationErr) xs => ToObject (OneEraValidationErr xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @ (ToObject `Compose` WrapValidationErr)) (K . toObject verb)
      . getOneEraValidationErr

instance ToObject (ValidationErr (BlockProtocol blk)) => ToObject (WrapValidationErr blk) where
    toObject verb = toObject verb . unwrapValidationErr


--
-- instances for HardForkCannotLead
--

-- It's a type alias:
-- type HardForkCannotLead xs = OneEraCannotLead xs

instance All (ToObject `Compose` WrapCannotLead) xs => ToObject (OneEraCannotLead xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @ (ToObject `Compose` WrapCannotLead))
              (K . toObject verb)
      . getOneEraCannotLead

instance ToObject (CannotLead (BlockProtocol blk)) => ToObject (WrapCannotLead blk) where
    toObject verb = toObject verb . unwrapCannotLead


--
-- instances for PerEraForgeState
--

instance (All (HasPrivacyAnnotation `Compose` WrapForgeState) xs,
          All (HasSeverityAnnotation `Compose` WrapForgeState) xs)
      => Transformable Text IO (PerEraForgeState xs)

instance All (HasPrivacyAnnotation `Compose` WrapForgeState) xs
      => HasPrivacyAnnotation (PerEraForgeState xs) where
    getPrivacyAnnotation =
        minimum -- so Confidential dominates
      . hcollapse
      . hcmap (Proxy @ (HasPrivacyAnnotation `Compose` WrapForgeState))
              (K . getPrivacyAnnotation)
      . getPerEraForgeState

instance HasPrivacyAnnotation (ForgeState blk)
      => HasPrivacyAnnotation (WrapForgeState blk) where
    getPrivacyAnnotation = getPrivacyAnnotation . unwrapForgeState


instance All (HasSeverityAnnotation `Compose` WrapForgeState) xs
      => HasSeverityAnnotation (PerEraForgeState xs) where
    getSeverityAnnotation =
        maximum  -- So max severity dominates
      . hcollapse
      . hcmap (Proxy @ (HasSeverityAnnotation `Compose` WrapForgeState))
              (K . getSeverityAnnotation)
      . getPerEraForgeState

instance HasSeverityAnnotation (ForgeState blk)
      => HasSeverityAnnotation (WrapForgeState blk) where
    getSeverityAnnotation = getSeverityAnnotation . unwrapForgeState

