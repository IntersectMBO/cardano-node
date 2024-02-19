{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.OrphanInstances.HardFork () where

import           Cardano.Slotting.Slot (EpochSize (..))
import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge, ForgeStateInfo,
                   ForgeStateUpdateError)
import           Ouroboros.Consensus.BlockchainTime (getSlotLength)
import           Ouroboros.Consensus.Cardano.Condense ()
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..),
                   OneEraCannotForge (..), OneEraEnvelopeErr (..), OneEraForgeStateInfo (..),
                   OneEraForgeStateUpdateError (..), OneEraLedgerError (..),
                   OneEraLedgerUpdate (..), OneEraLedgerWarning (..), OneEraSelectView (..),
                   OneEraValidationErr (..), mkEraMismatch)
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
                   (EraNodeToClientVersion (..), HardForkNodeToClientVersion (..),
                   HardForkNodeToNodeVersion (..), HardForkSpecificNodeToClientVersion (..),
                   HardForkSpecificNodeToNodeVersion (..))
import           Ouroboros.Consensus.HardFork.History.EraParams (EraParams (..), SafeZone)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion,
                   BlockNodeToNodeVersion)
import           Ouroboros.Consensus.Protocol.Abstract (SelectView, ValidationErr)
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Condense (Condense (..))

import           Data.Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import           Data.Proxy (Proxy (..))
import           Data.SOP (All, Compose, K (..))
import           Data.SOP.Strict


--
-- instances for hashes
--

instance Condense (OneEraHash xs) where
    condense = condense . Base16.encode . SBS.fromShort . getOneEraHash

--
-- instances for Header HardForkBlock
--

instance All (ToObject `Compose` Header) xs => ToObject (Header (HardForkBlock xs)) where
    toObject verb =
          hcollapse
        . hcmap (Proxy @(ToObject `Compose` Header)) (K . toObject verb)
        . getOneEraHeader
        . getHardForkHeader


--
-- instances for GenTx HardForkBlock
--

instance All (Compose ToObject GenTx) xs => ToObject (GenTx (HardForkBlock xs)) where
    toObject verb =
          hcollapse
        . hcmap (Proxy @(ToObject `Compose` GenTx)) (K . toObject verb)
        . getOneEraGenTx
        . getHardForkGenTx

instance  All (Compose ToJSON WrapGenTxId) xs => ToJSON (TxId (GenTx (HardForkBlock xs))) where
    toJSON =
          hcollapse
        . hcmap (Proxy @(ToJSON `Compose` WrapGenTxId)) (K . toJSON)
        . getOneEraGenTxId
        . getHardForkGenTxId

instance ToJSON (TxId (GenTx blk)) => ToJSON (WrapGenTxId blk) where
    toJSON = toJSON . unwrapGenTxId


--
-- instances for HardForkApplyTxErr
--

instance All (ToObject `Compose` WrapApplyTxErr) xs => ToObject (HardForkApplyTxErr xs) where
    toObject verb (HardForkApplyTxErrFromEra err) = toObject verb err
    toObject _verb (HardForkApplyTxErrWrongEra mismatch) =
      mconcat
        [ "kind"       .= String "HardForkApplyTxErrWrongEra"
        , "currentEra" .= ledgerEraName
        , "txEra"      .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (ToObject `Compose` WrapApplyTxErr) xs => ToObject (OneEraApplyTxErr xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @(ToObject `Compose` WrapApplyTxErr)) (K . toObject verb)
      . getOneEraApplyTxErr

instance ToObject (ApplyTxErr blk) => ToObject (WrapApplyTxErr blk) where
    toObject verb = toObject verb . unwrapApplyTxErr


--
-- instances for HardForkLedgerError
--

instance All (ToObject `Compose` WrapLedgerErr) xs => ToObject (HardForkLedgerError xs) where
    toObject verb (HardForkLedgerErrorFromEra err) = toObject verb err

    toObject _verb (HardForkLedgerErrorWrongEra mismatch) =
      mconcat
        [ "kind"       .= String "HardForkLedgerErrorWrongEra"
        , "currentEra" .= ledgerEraName
        , "blockEra"   .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (ToObject `Compose` WrapLedgerErr) xs => ToObject (OneEraLedgerError xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @(ToObject `Compose` WrapLedgerErr)) (K . toObject verb)
      . getOneEraLedgerError

instance ToObject (LedgerError blk) => ToObject (WrapLedgerErr blk) where
    toObject verb = toObject verb . unwrapLedgerErr


--
-- instances for HardForkLedgerWarning
--

instance ( All (ToObject `Compose` WrapLedgerWarning) xs
         , All SingleEraBlock xs
         ) => ToObject (HardForkLedgerWarning xs) where
    toObject verb warning = case warning of
      HardForkWarningInEra err -> toObject verb err

      HardForkWarningTransitionMismatch toEra eraParams epoch ->
        mconcat
          [ "kind"            .= String "HardForkWarningTransitionMismatch"
          , "toEra"           .= condense toEra
          , "eraParams"       .= toObject verb eraParams
          , "transitionEpoch" .= epoch
          ]

      HardForkWarningTransitionInFinalEra fromEra epoch ->
        mconcat
          [ "kind"            .= String "HardForkWarningTransitionInFinalEra"
          , "fromEra"         .= condense fromEra
          , "transitionEpoch" .= epoch
          ]

      HardForkWarningTransitionUnconfirmed toEra ->
        mconcat
          [ "kind"  .= String "HardForkWarningTransitionUnconfirmed"
          , "toEra" .= condense toEra
          ]

      HardForkWarningTransitionReconfirmed fromEra toEra prevEpoch newEpoch ->
        mconcat
          [ "kind"                .= String "HardForkWarningTransitionReconfirmed"
          , "fromEra"             .= condense fromEra
          , "toEra"               .= condense toEra
          , "prevTransitionEpoch" .= prevEpoch
          , "newTransitionEpoch"  .= newEpoch
          ]

instance All (ToObject `Compose` WrapLedgerWarning) xs => ToObject (OneEraLedgerWarning xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @(ToObject `Compose` WrapLedgerWarning)) (K . toObject verb)
      . getOneEraLedgerWarning

instance ToObject (LedgerWarning blk) => ToObject (WrapLedgerWarning blk) where
    toObject verb = toObject verb . unwrapLedgerWarning

instance ToObject EraParams where
    toObject _verb EraParams{ eraEpochSize, eraSlotLength, eraSafeZone} =
      mconcat
        [ "epochSize"  .= unEpochSize eraEpochSize
        , "slotLength" .= getSlotLength eraSlotLength
        , "safeZone"   .= eraSafeZone
        ]

deriving instance ToJSON SafeZone


--
-- instances for HardForkLedgerUpdate
--

instance ( All (ToObject `Compose` WrapLedgerUpdate) xs
         , All SingleEraBlock xs
         ) => ToObject (HardForkLedgerUpdate xs) where
    toObject verb update = case update of
      HardForkUpdateInEra err -> toObject verb err

      HardForkUpdateTransitionConfirmed fromEra toEra epoch ->
        mconcat
          [ "kind"            .= String "HardForkUpdateTransitionConfirmed"
          , "fromEra"         .= condense fromEra
          , "toEra"           .= condense toEra
          , "transitionEpoch" .= epoch
          ]

      HardForkUpdateTransitionDone fromEra toEra epoch ->
        mconcat
          [ "kind"            .= String "HardForkUpdateTransitionDone"
          , "fromEra"         .= condense fromEra
          , "toEra"           .= condense toEra
          , "transitionEpoch" .= epoch
          ]

      HardForkUpdateTransitionRolledBack fromEra toEra ->
        mconcat
          [ "kind"    .= String "HardForkUpdateTransitionRolledBack"
          , "fromEra" .= condense fromEra
          , "toEra"   .= condense toEra
          ]

instance All (ToObject `Compose` WrapLedgerUpdate) xs => ToObject (OneEraLedgerUpdate xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @(ToObject `Compose` WrapLedgerUpdate)) (K . toObject verb)
      . getOneEraLedgerUpdate

instance ToObject (LedgerUpdate blk) => ToObject (WrapLedgerUpdate blk) where
    toObject verb = toObject verb . unwrapLedgerUpdate


--
-- instances for HardForkEnvelopeErr
--

instance All (ToObject `Compose` WrapEnvelopeErr) xs => ToObject (HardForkEnvelopeErr xs) where
    toObject verb (HardForkEnvelopeErrFromEra err) = toObject verb err

    toObject _verb (HardForkEnvelopeErrWrongEra mismatch) =
      mconcat
        [ "kind"       .= String "HardForkEnvelopeErrWrongEra"
        , "currentEra" .= ledgerEraName
        , "blockEra"   .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (ToObject `Compose` WrapEnvelopeErr) xs => ToObject (OneEraEnvelopeErr xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @(ToObject `Compose` WrapEnvelopeErr)) (K . toObject verb)
      . getOneEraEnvelopeErr

instance ToObject (OtherHeaderEnvelopeError blk) => ToObject (WrapEnvelopeErr blk) where
    toObject verb = toObject verb . unwrapEnvelopeErr


--
-- instances for HardForkValidationErr
--

instance All (ToObject `Compose` WrapValidationErr) xs => ToObject (HardForkValidationErr xs) where
    toObject verb (HardForkValidationErrFromEra err) = toObject verb err

    toObject _verb (HardForkValidationErrWrongEra mismatch) =
      mconcat
        [ "kind"       .= String "HardForkValidationErrWrongEra"
        , "currentEra" .= ledgerEraName
        , "blockEra"   .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (ToObject `Compose` WrapValidationErr) xs => ToObject (OneEraValidationErr xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @(ToObject `Compose` WrapValidationErr)) (K . toObject verb)
      . getOneEraValidationErr

instance ToObject (ValidationErr (BlockProtocol blk)) => ToObject (WrapValidationErr blk) where
    toObject verb = toObject verb . unwrapValidationErr


--
-- instances for HardForkCannotForge
--

-- It's a type alias:
-- type HardForkCannotForge xs = OneEraCannotForge xs

instance All (ToObject `Compose` WrapCannotForge) xs => ToObject (OneEraCannotForge xs) where
    toObject verb =
        hcollapse
      . hcmap (Proxy @(ToObject `Compose` WrapCannotForge))
              (K . toObject verb)
      . getOneEraCannotForge

instance ToObject (CannotForge blk) => ToObject (WrapCannotForge blk) where
    toObject verb = toObject verb . unwrapCannotForge


--
-- instances for HardForkForgeStateInfo
--

-- It's a type alias:
-- type HardForkForgeStateInfo xs = OneEraForgeStateInfo xs

instance All (ToObject `Compose` WrapForgeStateInfo) xs => ToObject (OneEraForgeStateInfo xs) where
    toObject verb forgeStateInfo =
        mconcat
          [ "kind" .= String "HardForkForgeStateInfo"
          , "forgeStateInfo" .= toJSON forgeStateInfo'
          ]
      where
        forgeStateInfo' :: Object
        forgeStateInfo' =
              hcollapse
            . hcmap (Proxy @(ToObject `Compose` WrapForgeStateInfo))
                    (K . toObject verb)
            . getOneEraForgeStateInfo
            $ forgeStateInfo

instance ToObject (ForgeStateInfo blk) => ToObject (WrapForgeStateInfo blk) where
    toObject verb = toObject verb . unwrapForgeStateInfo


--
-- instances for HardForkForgeStateUpdateError
--

-- It's a type alias:
-- type HardForkForgeStateUpdateError xs = OneEraForgeStateUpdateError xs

instance All (ToObject `Compose` WrapForgeStateUpdateError) xs => ToObject (OneEraForgeStateUpdateError xs) where
    toObject verb forgeStateUpdateError =
        mconcat
          [ "kind" .= String "HardForkForgeStateUpdateError"
          , "forgeStateUpdateError" .= toJSON forgeStateUpdateError'
          ]
      where
        forgeStateUpdateError' :: Object
        forgeStateUpdateError' =
              hcollapse
            . hcmap (Proxy @(ToObject `Compose` WrapForgeStateUpdateError))
                    (K . toObject verb)
            . getOneEraForgeStateUpdateError
            $ forgeStateUpdateError

instance ToObject (ForgeStateUpdateError blk) => ToObject (WrapForgeStateUpdateError blk) where
    toObject verb = toObject verb . unwrapForgeStateUpdateError

--
-- Instances for HardForkNodeToClientVersion
--

instance ( ToJSON (BlockNodeToClientVersion x)
         , All (ToJSON `Compose` EraNodeToClientVersion) (x ': xs)
         ) => ToJSON (HardForkNodeToClientVersion (x ': xs)) where
  toJSON (HardForkNodeToClientDisabled blockNodeToClientVersion) =
      object [ "tag"      .= String "HardForkNodeToClientDisabled"
             , "contents" .= toJSON blockNodeToClientVersion
             ]
  toJSON (HardForkNodeToClientEnabled hfNodeToClientVersion eraNodeToClientVersions) =
      object [ "tag"                                 .= String "HardForkNodeToClientEnabled"
             , "hardForkSpecificNodeToClientVersion" .= toJSON hfNodeToClientVersion
             , "eraNodeToClientVersions"             .= hcollapse eraNodeToClientVersionsAsJSON
             ]
    where
      eraNodeToClientVersionsAsJSON :: NP (K Value) (x ': xs)
      eraNodeToClientVersionsAsJSON = hcmap (Proxy @(ToJSON `Compose` EraNodeToClientVersion))
                                            (K . toJSON)
                                            eraNodeToClientVersions

instance ToJSON HardForkSpecificNodeToClientVersion where
    toJSON HardForkSpecificNodeToClientVersion1 = String "HardForkSpecificNodeToClientVersion1"
    toJSON HardForkSpecificNodeToClientVersion2 = String "HardForkSpecificNodeToClientVersion2"

instance (ToJSON (BlockNodeToClientVersion blk)) => ToJSON (EraNodeToClientVersion blk) where
    toJSON EraNodeToClientDisabled = String "EraNodeToClientDisabled"
    toJSON (EraNodeToClientEnabled blockNodeToClientVersion) = toJSON blockNodeToClientVersion

--
-- Instances for HardForkNodeToNodeVersion
--
instance ( ToJSON (BlockNodeToNodeVersion x)
         , All (ToJSON `Compose` WrapNodeToNodeVersion) (x ': xs)
         ) => ToJSON (HardForkNodeToNodeVersion (x ': xs)) where
    toJSON (HardForkNodeToNodeDisabled blockNodeToNodeVersion) =
        object [ "tag" .= String "HardForkNodeToNodeDisabled"
               , "contents" .= toJSON blockNodeToNodeVersion
               ]
    toJSON (HardForkNodeToNodeEnabled hfNodeToNodeVersion eraNodeToNodeVersions) =
        object [ "tag"                               .= String "HardForkNodeToNodeEnabled"
               , "hardForkSpecificNodeToNodeVersion" .= toJSON hfNodeToNodeVersion
               , "eraNodeToNodeVersions"             .= hcollapse eraNodeToNodeVersionsAsJSON
               ]
      where
        eraNodeToNodeVersionsAsJSON :: NP (K Value) (x ': xs)
        eraNodeToNodeVersionsAsJSON = hcmap (Proxy @(ToJSON `Compose` WrapNodeToNodeVersion))
                                            (K . toJSON)
                                            eraNodeToNodeVersions

instance ToJSON HardForkSpecificNodeToNodeVersion where
    toJSON HardForkSpecificNodeToNodeVersion1 = "HardForkSpecificNodeToNodeVersion1"

instance (ToJSON (BlockNodeToNodeVersion blk)) => ToJSON (WrapNodeToNodeVersion blk) where
    toJSON (WrapNodeToNodeVersion blockNodeToNodeVersion) = toJSON blockNodeToNodeVersion

--
-- instances for HardForkSelectView
--

instance All (ToObject `Compose` WrapSelectView) xs => ToObject (HardForkSelectView xs) where
    -- elide BlockNo as it is already contained in every per-era SelectView
    toObject verb = toObject verb . dropBlockNo . getHardForkSelectView

instance All (ToObject `Compose` WrapSelectView) xs => ToObject (OneEraSelectView xs) where
    toObject verb =
          hcollapse
        . hcmap (Proxy @(ToObject `Compose` WrapSelectView))
                (K . toObject verb)
        . getOneEraSelectView

instance ToObject (SelectView (BlockProtocol blk)) => ToObject (WrapSelectView blk) where
    toObject verb = toObject verb . unwrapSelectView
