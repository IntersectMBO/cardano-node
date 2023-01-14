{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- HLINT ignore "Use head" -}
module Cardano.Analysis.API.Chain (module Cardano.Analysis.API.Chain) where

import Cardano.Prelude hiding (head)

import Data.Aeson
import Data.Time.Clock qualified as Time

import Cardano.Analysis.API.Context
import Cardano.Analysis.API.Ground


-- | A pretty obvious (and dangerously assumptious) interpretation of an absolute slot number.
--   This is wrong, if you consider the reasons why epochLength can change.
unsafeParseSlot :: Genesis -> SlotNo -> (EpochNo, EpochSlot)
unsafeParseSlot Genesis{..} slot =  (EpochNo epoch, EpochSlot epochSlot)
  where
    (epoch, epochSlot) = unSlotNo slot `divMod` epochLength


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

impliedSlot :: Genesis -> UTCTime -> SlotNo
impliedSlot Genesis{..} =
  SlotNo
  . floor
  . (/ slotLength)
  . (`Time.diffUTCTime` systemStart)

sinceSlot :: UTCTime -> SlotStart -> NominalDiffTime
sinceSlot t (SlotStart start) = Time.diffUTCTime t start

afterSlot :: NominalDiffTime -> SlotStart -> UTCTime
afterSlot t (SlotStart start) = Time.addUTCTime t start
