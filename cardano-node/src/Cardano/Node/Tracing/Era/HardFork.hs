{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Cardano.Node.Tracing.Era.HardFork ()
  where

import           Cardano.Logging
import           Cardano.Slotting.Slot (EpochSize (..))
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
import           Ouroboros.Consensus.HardFork.History
                   (EraParams (eraEpochSize, eraSafeZone, eraSlotLength), SafeZone)
import           Ouroboros.Consensus.HardFork.History.EraParams (EraParams (EraParams))
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion (..),
                   BlockNodeToNodeVersion)
import           Ouroboros.Consensus.Protocol.Abstract (SelectView, ValidationErr)
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Condense (Condense (..))

import           Data.Aeson
import           Data.Proxy (Proxy (..))
import           Data.SOP (All, Compose, K (K))
import           Data.SOP.Strict



instance  All (Compose ToJSON WrapGenTxId) xs => ToJSON (TxId (GenTx (HardForkBlock xs))) where
    toJSON =
          hcollapse
        . hcmap (Proxy @(ToJSON `Compose` WrapGenTxId)) (K . toJSON)
        . getOneEraGenTxId
        . getHardForkGenTxId

instance ToJSON (TxId (GenTx blk)) => ToJSON (WrapGenTxId blk) where
    toJSON = toJSON . unwrapGenTxId

deriving instance ToJSON SafeZone



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
    toJSON HardForkSpecificNodeToClientVersion3 = String "HardForkSpecificNodeToClientVersion3"

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
-- instances for Header HardForkBlock
--

instance All (LogFormatting `Compose` Header) xs => LogFormatting (Header (HardForkBlock xs)) where
    forMachine dtal =
          hcollapse
        . hcmap (Proxy @(LogFormatting `Compose` Header)) (K . forMachine dtal)
        . getOneEraHeader
        . getHardForkHeader


--
-- instances for GenTx HardForkBlock
--

instance All (Compose LogFormatting GenTx) xs => LogFormatting (GenTx (HardForkBlock xs)) where
    forMachine dtal =
          hcollapse
        . hcmap (Proxy @(LogFormatting `Compose` GenTx)) (K . forMachine dtal)
        . getOneEraGenTx
        . getHardForkGenTx


--
-- instances for HardForkApplyTxErr
--

instance All (LogFormatting `Compose` WrapApplyTxErr) xs => LogFormatting (HardForkApplyTxErr xs) where
    forMachine dtal (HardForkApplyTxErrFromEra err) = forMachine dtal err
    forMachine _dtal (HardForkApplyTxErrWrongEra mismatch) =
      mconcat
        [ "kind"       .= String "HardForkApplyTxErrWrongEra"
        , "currentEra" .= ledgerEraName
        , "txEra"      .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (LogFormatting `Compose` WrapApplyTxErr) xs => LogFormatting (OneEraApplyTxErr xs) where
    forMachine dtal =
        hcollapse
      . hcmap (Proxy @(LogFormatting `Compose` WrapApplyTxErr)) (K . forMachine dtal)
      . getOneEraApplyTxErr

instance LogFormatting (ApplyTxErr blk) => LogFormatting (WrapApplyTxErr blk) where
    forMachine dtal = forMachine dtal . unwrapApplyTxErr


--
-- instances for HardForkLedgerError
--

instance All (LogFormatting `Compose` WrapLedgerErr) xs => LogFormatting (HardForkLedgerError xs) where
    forMachine dtal (HardForkLedgerErrorFromEra err) = forMachine dtal err

    forMachine _dtal (HardForkLedgerErrorWrongEra mismatch) =
      mconcat
        [ "kind"       .= String "HardForkLedgerErrorWrongEra"
        , "currentEra" .= ledgerEraName
        , "blockEra"   .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (LogFormatting `Compose` WrapLedgerErr) xs => LogFormatting (OneEraLedgerError xs) where
    forMachine dtal =
        hcollapse
      . hcmap (Proxy @(LogFormatting `Compose` WrapLedgerErr)) (K . forMachine dtal)
      . getOneEraLedgerError

instance LogFormatting (LedgerError blk) => LogFormatting (WrapLedgerErr blk) where
    forMachine dtal = forMachine dtal . unwrapLedgerErr


--
-- instances for HardForkLedgerWarning
--

instance ( All (LogFormatting `Compose` WrapLedgerWarning) xs
         , All SingleEraBlock xs
         ) => LogFormatting (HardForkLedgerWarning xs) where
    forMachine dtal warning = case warning of
      HardForkWarningInEra err -> forMachine dtal err

      HardForkWarningTransitionMismatch toEra eraParams epoch ->
        mconcat
          [ "kind"            .= String "HardForkWarningTransitionMismatch"
          , "toEra"           .= condense toEra
          , "eraParams"       .= forMachine dtal eraParams
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

instance All (LogFormatting `Compose` WrapLedgerWarning) xs => LogFormatting (OneEraLedgerWarning xs) where
    forMachine dtal =
        hcollapse
      . hcmap (Proxy @(LogFormatting `Compose` WrapLedgerWarning)) (K . forMachine dtal)
      . getOneEraLedgerWarning

instance LogFormatting (LedgerWarning blk) => LogFormatting (WrapLedgerWarning blk) where
    forMachine dtal = forMachine dtal . unwrapLedgerWarning

instance LogFormatting EraParams where
    forMachine _dtal EraParams{ eraEpochSize, eraSlotLength, eraSafeZone} =
      mconcat
        [ "epochSize"  .= unEpochSize eraEpochSize
        , "slotLength" .= getSlotLength eraSlotLength
        , "safeZone"   .= eraSafeZone
        ]

-- deriving instance ToJSON SafeZone


--
-- instances for HardForkLedgerUpdate
--

instance ( All (LogFormatting `Compose` WrapLedgerUpdate) xs
         , All SingleEraBlock xs
         ) => LogFormatting (HardForkLedgerUpdate xs) where
    forMachine dtal update = case update of
      HardForkUpdateInEra err -> forMachine dtal err

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

instance All (LogFormatting `Compose` WrapLedgerUpdate) xs => LogFormatting (OneEraLedgerUpdate xs) where
    forMachine dtal =
        hcollapse
      . hcmap (Proxy @(LogFormatting `Compose` WrapLedgerUpdate)) (K . forMachine dtal)
      . getOneEraLedgerUpdate

instance LogFormatting (LedgerUpdate blk) => LogFormatting (WrapLedgerUpdate blk) where
    forMachine dtal = forMachine dtal . unwrapLedgerUpdate


--
-- instances for HardForkEnvelopeErr
--

instance All (LogFormatting `Compose` WrapEnvelopeErr) xs => LogFormatting (HardForkEnvelopeErr xs) where
    forMachine dtal (HardForkEnvelopeErrFromEra err) = forMachine dtal err

    forMachine _dtal (HardForkEnvelopeErrWrongEra mismatch) =
      mconcat
        [ "kind"       .= String "HardForkEnvelopeErrWrongEra"
        , "currentEra" .= ledgerEraName
        , "blockEra"   .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (LogFormatting `Compose` WrapEnvelopeErr) xs => LogFormatting (OneEraEnvelopeErr xs) where
    forMachine dtal =
        hcollapse
      . hcmap (Proxy @(LogFormatting `Compose` WrapEnvelopeErr)) (K . forMachine dtal)
      . getOneEraEnvelopeErr

instance LogFormatting (OtherHeaderEnvelopeError blk) => LogFormatting (WrapEnvelopeErr blk) where
    forMachine dtal = forMachine dtal . unwrapEnvelopeErr


--
-- instances for HardForkValidationErr
--

instance All (LogFormatting `Compose` WrapValidationErr) xs => LogFormatting (HardForkValidationErr xs) where
    forMachine dtal (HardForkValidationErrFromEra err) = forMachine dtal err

    forMachine _dtal (HardForkValidationErrWrongEra mismatch) =
      mconcat
        [ "kind"       .= String "HardForkValidationErrWrongEra"
        , "currentEra" .= ledgerEraName
        , "blockEra"   .= otherEraName
        ]
      where
        EraMismatch {ledgerEraName, otherEraName} = mkEraMismatch mismatch

instance All (LogFormatting `Compose` WrapValidationErr) xs => LogFormatting (OneEraValidationErr xs) where
    forMachine dtal =
        hcollapse
      . hcmap (Proxy @(LogFormatting `Compose` WrapValidationErr)) (K . forMachine dtal)
      . getOneEraValidationErr

instance LogFormatting (ValidationErr (BlockProtocol blk)) => LogFormatting (WrapValidationErr blk) where
    forMachine dtal = forMachine dtal . unwrapValidationErr


--
-- instances for HardForkCannotForge
--

-- It's a type alias:
-- type HardForkCannotForge xs = OneEraCannotForge xs

instance All (LogFormatting `Compose` WrapCannotForge) xs => LogFormatting (OneEraCannotForge xs) where
    forMachine dtal =
        hcollapse
      . hcmap (Proxy @(LogFormatting `Compose` WrapCannotForge))
              (K . forMachine dtal)
      . getOneEraCannotForge

instance LogFormatting (CannotForge blk) => LogFormatting (WrapCannotForge blk) where
    forMachine dtal = forMachine dtal . unwrapCannotForge


--
-- instances for HardForkForgeStateInfo
--

-- It's a type alias:
-- type HardForkForgeStateInfo xs = OneEraForgeStateInfo xs

instance All (LogFormatting `Compose` WrapForgeStateInfo) xs => LogFormatting (OneEraForgeStateInfo xs) where
    forMachine dtal forgeStateInfo =
        mconcat
          [ "kind" .= String "HardForkForgeStateInfo"
          , "forgeStateInfo" .= toJSON forgeStateInfo'
          ]
      where
        forgeStateInfo' :: Object
        forgeStateInfo' =
              hcollapse
            . hcmap (Proxy @(LogFormatting `Compose` WrapForgeStateInfo))
                    (K . forMachine dtal)
            . getOneEraForgeStateInfo
            $ forgeStateInfo

instance LogFormatting (ForgeStateInfo blk) => LogFormatting (WrapForgeStateInfo blk) where
    forMachine dtal = forMachine dtal . unwrapForgeStateInfo


--
-- instances for HardForkForgeStateUpdateError
--

-- It's a type alias:
-- type HardForkForgeStateUpdateError xs = OneEraForgeStateUpdateError xs

instance All (LogFormatting `Compose` WrapForgeStateUpdateError) xs => LogFormatting (OneEraForgeStateUpdateError xs) where
    forMachine dtal forgeStateUpdateError =
        mconcat
          [ "kind" .= String "HardForkForgeStateUpdateError"
          , "forgeStateUpdateError" .= toJSON forgeStateUpdateError'
          ]
      where
        forgeStateUpdateError' :: Object
        forgeStateUpdateError' =
              hcollapse
            . hcmap (Proxy @(LogFormatting `Compose` WrapForgeStateUpdateError))
                    (K . forMachine dtal)
            . getOneEraForgeStateUpdateError
            $ forgeStateUpdateError

instance LogFormatting (ForgeStateUpdateError blk) => LogFormatting (WrapForgeStateUpdateError blk) where
    forMachine dtal = forMachine dtal . unwrapForgeStateUpdateError

--
-- instances for HardForkSelectView
--

instance All (LogFormatting `Compose` WrapSelectView) xs => LogFormatting (HardForkSelectView xs) where
    -- elide BlockNo as it is already contained in every per-era SelectView
    -- TODO: use level DMinimal for a textual representation without the block number,
    -- like this: `forMachine DMinimal . getHardForkSelectView`, and update the different SelectView instances
    -- to not print the blockNr
    forMachine dtal = forMachine dtal . dropBlockNo . getHardForkSelectView

instance All (LogFormatting `Compose` WrapSelectView) xs => LogFormatting (OneEraSelectView xs) where
    forMachine dtal =
          hcollapse
        . hcmap (Proxy @(LogFormatting `Compose` WrapSelectView))
                (K . forMachine dtal)
        . getOneEraSelectView

instance LogFormatting (SelectView (BlockProtocol blk)) => LogFormatting (WrapSelectView blk) where
    forMachine dtal = forMachine dtal . unwrapSelectView
