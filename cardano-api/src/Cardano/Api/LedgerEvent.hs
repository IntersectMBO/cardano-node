{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Api.LedgerEvent
  ( LedgerEvent (..),
    MIRDistributionDetails(..),
    toLedgerEvent,
  )
where

import           Cardano.Api.Address (StakeCredential, fromShelleyStakeCredential)
import           Cardano.Api.Block (EpochNo)
import           Cardano.Api.Certificate (Certificate)
import           Cardano.Api.Value (Lovelace, fromShelleyDeltaLovelace, fromShelleyLovelace)
import qualified Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Credential
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Era (Crypto)
import           Control.State.Transition (Event)
import           Data.Function (($), (.))
import           Data.Functor (fmap)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (Maybe (Just, Nothing))
import           Data.SOP.Strict
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (HardForkBlock)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraLedgerEvent)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerState)
import           Ouroboros.Consensus.Ledger.Basics (AuxLedgerEvent)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                   ShelleyLedgerEvent (ShelleyLedgerEventTICK))
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Shelley.Spec.Ledger.API (InstantaneousRewards (InstantaneousRewards))
import           Shelley.Spec.Ledger.STS.Mir (MirEvent (..))
import           Shelley.Spec.Ledger.STS.NewEpoch (NewEpochEvent (MirEvent, SumRewards))
import           Shelley.Spec.Ledger.STS.Tick (TickEvent (NewEpochEvent))

data LedgerEvent
  = -- | The given pool is being registered for the first time on chain.
    PoolRegistration Certificate
  | -- | The given pool already exists and is being re-registered.
    PoolReRegistration Certificate
  | -- | Rewards are being distributed.
    RewardsDistribution EpochNo (Map StakeCredential Lovelace)
  | -- | MIR are being distributed.
    MIRDistribution MIRDistributionDetails

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe LedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ = Nothing

instance
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ TickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "MIR" ledgerera) ~ MirEvent ledgerera
  ) =>
  ConvertLedgerEvent (ShelleyBlock ledgerera)
  where
  toLedgerEvent evt = case unwrapLedgerEvent evt of
    LESumRewards e m -> Just $ RewardsDistribution e m
    LEMirTransfer rp rt rtt ttr ->
      Just $
        MIRDistribution $
          MIRDistributionDetails rp rt rtt ttr
    _ -> Nothing

instance All ConvertLedgerEvent xs => ConvertLedgerEvent (HardForkBlock xs) where
  toLedgerEvent =
    hcollapse
      . hcmap (Proxy @ ConvertLedgerEvent) (K . toLedgerEvent)
      . getOneEraLedgerEvent
      . unwrapLedgerEvent

--------------------------------------------------------------------------------
-- Event details
--------------------------------------------------------------------------------

-- | Details of fund transfers due to MIR certificates.
--
--   Note that the transfers from reserves to treasury and treasury to reserves
--   are inverse; a transfer of 100 ADA in either direction will result in a net
--   movement of 0, but we include both directions for assistance in debugging.
data MIRDistributionDetails = MIRDistributionDetails
  { reservePayouts :: Map StakeCredential Lovelace,
    treasuryPayouts :: Map StakeCredential Lovelace,
    reservesToTreasury :: Lovelace,
    treasuryToReserves :: Lovelace
  }

--------------------------------------------------------------------------------
-- Patterns for event access
--------------------------------------------------------------------------------

pattern LESumRewards ::
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ TickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
  ) =>
  EpochNo ->
  Map StakeCredential Lovelace ->
  AuxLedgerEvent (LedgerState (ShelleyBlock ledgerera))
pattern LESumRewards e m <-
  ShelleyLedgerEventTICK
    (NewEpochEvent (SumRewards e (convertSumRewardsMap -> m)))

pattern LEMirTransfer ::
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ TickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "MIR" ledgerera) ~ MirEvent ledgerera
  ) =>
  Map StakeCredential Lovelace ->
  Map StakeCredential Lovelace ->
  Lovelace ->
  Lovelace ->
  AuxLedgerEvent (LedgerState (ShelleyBlock ledgerera))
pattern LEMirTransfer rp tp rtt ttr <-
  ShelleyLedgerEventTICK
    ( NewEpochEvent
        ( MirEvent
            ( MirTransfer
                ( InstantaneousRewards
                    (convertSumRewardsMap -> rp)
                    (convertSumRewardsMap -> tp)
                    (fromShelleyDeltaLovelace -> rtt)
                    (fromShelleyDeltaLovelace -> ttr)
                  )
              )
          )
      )

convertSumRewardsMap ::
  Map
    ( Cardano.Ledger.Credential.StakeCredential
        Cardano.Ledger.Crypto.StandardCrypto
    )
    Cardano.Ledger.Coin.Coin ->
  Map StakeCredential Lovelace
convertSumRewardsMap =
  Map.mapKeys fromShelleyStakeCredential . fmap fromShelleyLovelace
