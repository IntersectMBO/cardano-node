{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}
{- HLINT ignore "Use head" -}
module Cardano.Analysis.Chain (module Cardano.Analysis.Chain) where

import Cardano.Prelude hiding (head)

import Data.Aeson
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock qualified as Time

import Cardano.Analysis.Run
import Cardano.Slotting.Slot (EpochNo (..),  SlotNo (..))


newtype EpochSlot = EpochSlot { unEpochSlot :: Word64 }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, NFData, ToJSON)
  deriving newtype (Num)

-- | A pretty obvious (and dangerously assumptious) interpretation of an absolute slot number.
--   This is wrong, if you consider the reasons why epochLength can change.
unsafeParseSlot :: Genesis -> SlotNo -> (EpochNo, EpochSlot)
unsafeParseSlot Genesis{..} slot =  (EpochNo epoch, EpochSlot epochSlot)
  where
    (epoch, epochSlot) = unSlotNo slot `divMod` epochLength

newtype EpochSafeInt = EpochSafeInt { unEpochSafeInt :: Word64 }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, NFData, ToJSON)
  deriving newtype (Num)

slotEpochSafeInt :: Genesis -> EpochSlot -> EpochSafeInt
slotEpochSafeInt Genesis{..} (EpochSlot relSlot) =
  EpochSafeInt . floor $
      fromIntegral relSlot
    / fromIntegral @Word64 @Double securityParam
    * activeSlotsCoeff

newtype SlotStart =
  SlotStart { unSlotStart :: UTCTime }
  deriving stock (Eq, Generic, Show)
  deriving newtype (FromJSON, NFData, ToJSON)

slotStart :: Genesis -> SlotNo -> SlotStart
slotStart Genesis{..} =
  SlotStart
  . flip Time.addUTCTime systemStart
  . (* slotLength)
  . fromIntegral
  . unSlotNo

sinceSlot :: UTCTime -> SlotStart -> NominalDiffTime
sinceSlot t (SlotStart start) = Time.diffUTCTime t start

afterSlot :: NominalDiffTime -> SlotStart -> UTCTime
afterSlot t (SlotStart start) = Time.addUTCTime t start
