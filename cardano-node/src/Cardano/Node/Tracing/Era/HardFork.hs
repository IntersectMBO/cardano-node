{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Era.HardFork ()
  where

import           Cardano.Logging
import           Cardano.Slotting.Slot (EpochSize (..))
import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge, ForgeStateInfo,
                   ForgeStateUpdateError, PerasWeight (..))
import           Ouroboros.Consensus.BlockchainTime (getSlotLength)
import           Ouroboros.Consensus.Cardano.Condense ()
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..), OneEraTiebreakerView (..),
                   OneEraCannotForge (..), OneEraEnvelopeErr (..), OneEraForgeStateInfo (..),
                   OneEraForgeStateUpdateError (..), OneEraLedgerError (..),
                   OneEraLedgerUpdate (..), OneEraLedgerWarning (..),
                   OneEraValidationErr (..), mkEraMismatch)
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()
import           Ouroboros.Consensus.HardFork.History
                   (EraParams (eraEpochSize, eraSafeZone, eraSlotLength))
import           Ouroboros.Consensus.HardFork.History.EraParams (EraParams (EraParams))
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Peras.SelectView
import           Ouroboros.Consensus.Protocol.Abstract (TiebreakerView, ValidationErr)
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Condense (Condense (..))

import           Data.Aeson
import           Data.Proxy (Proxy (..))
import           Data.SOP (All, Compose, K (K))
import           Data.SOP.Strict


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

instance All (LogFormatting `Compose` WrapTiebreakerView) xs => LogFormatting (HardForkTiebreakerView xs) where
    forMachine dtal = forMachine dtal . getHardForkTiebreakerView

instance LogFormatting (TiebreakerView protocol) => LogFormatting (WeightedSelectView protocol) where
    forMachine dtal sv = mconcat
        [ "blockNo"  .= wsvBlockNo sv
        , "weightBoost" .= unPerasWeight (wsvWeightBoost sv)
        , forMachine dtal (wsvTiebreaker sv)
        ]

instance All (LogFormatting `Compose` WrapTiebreakerView) xs => LogFormatting (OneEraTiebreakerView xs) where
    forMachine dtal =
          hcollapse
        . hcmap (Proxy @(LogFormatting `Compose` WrapTiebreakerView))
                (K . forMachine dtal)
        . getOneEraTiebreakerView

instance LogFormatting (TiebreakerView (BlockProtocol blk)) => LogFormatting (WrapTiebreakerView blk) where
  forMachine dtal  = forMachine dtal  . unwrapTiebreakerView
