{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.LedgerEvent
  ( LedgerEvent (..),
    MIRDistributionDetails (..),
    PoolReapDetails (..),
    toLedgerEvent,
  )
where

import           Cardano.Api.Address (StakeCredential, fromShelleyStakeCredential)
import           Cardano.Api.Block (EpochNo)
import           Cardano.Api.Certificate (Certificate)
import           Cardano.Api.KeysShelley (Hash (StakePoolKeyHash), StakePoolKey)
import           Cardano.Api.Value (Lovelace, fromShelleyDeltaLovelace, fromShelleyLovelace)
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Era (Crypto)
import qualified Cardano.Ledger.Keys as Ledger
import           Cardano.Ledger.Shelley.API (InstantaneousRewards (InstantaneousRewards))
import           Cardano.Ledger.Shelley.Rewards
import           Control.State.Transition (Event)
import           Data.Function (($), (.))
import           Data.Functor (fmap)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (Maybe (Just, Nothing))
import           Data.SOP.Strict
import           Data.Set (Set)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (HardForkBlock)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraLedgerEvent)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerState)
import           Ouroboros.Consensus.Ledger.Basics (AuxLedgerEvent)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                   ShelleyLedgerEvent (ShelleyLedgerEventTICK))
import           Ouroboros.Consensus.TypeFamilyWrappers
import Cardano.Ledger.Shelley.Rules.Tick (ShelleyTickEvent (NewEpochEvent))
import Cardano.Ledger.Shelley.Rules.NewEpoch (ShelleyNewEpochEvent (..))
import Cardano.Ledger.Shelley.Rules.Epoch (ShelleyEpochEvent (..))
import Cardano.Ledger.Shelley.Rules.PoolReap (ShelleyPoolreapEvent (..))
import Cardano.Ledger.Shelley.Rules.Mir (ShelleyMirEvent (..))
import Cardano.Ledger.Shelley.Rules.Rupd (RupdEvent (..))

data LedgerEvent
  = -- | The given pool is being registered for the first time on chain.
    PoolRegistration Certificate
  | -- | The given pool already exists and is being re-registered.
    PoolReRegistration Certificate
  | -- | Incremental rewards are being computed.
    IncrementalRewardsDistribution EpochNo (Map StakeCredential (Set (Reward StandardCrypto)))
  | -- | Reward distribution has completed.
    RewardsDistribution EpochNo (Map StakeCredential (Set (Reward StandardCrypto)))
  | -- | MIR are being distributed.
    MIRDistribution MIRDistributionDetails
  | -- | Pools have been reaped and deposits refunded.
    PoolReap PoolReapDetails

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe LedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ = Nothing

instance
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "EPOCH" ledgerera) ~ ShelleyEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "POOLREAP" ledgerera) ~ ShelleyPoolreapEvent ledgerera,
    Event (Ledger.Core.EraRule "MIR" ledgerera) ~ ShelleyMirEvent ledgerera,
    Event (Ledger.Core.EraRule "RUPD" ledgerera) ~ RupdEvent (Crypto ledgerera)
  ) =>
  ConvertLedgerEvent (ShelleyBlock protocol ledgerera)
  where
  toLedgerEvent evt = case unwrapLedgerEvent evt of
    LEDeltaRewardEvent e m -> Just $ IncrementalRewardsDistribution e m
    LERewardEvent e m -> Just $ RewardsDistribution e m
    LEMirTransfer rp rt rtt ttr ->
      Just $
        MIRDistribution $
          MIRDistributionDetails rp rt rtt ttr
    LERetiredPools r u e -> Just $ PoolReap $ PoolReapDetails e r u
    _ -> Nothing

instance All ConvertLedgerEvent xs => ConvertLedgerEvent (HardForkBlock xs) where
  toLedgerEvent =
    hcollapse
      . hcmap (Proxy @ConvertLedgerEvent) (K . toLedgerEvent)
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
  { mirddReservePayouts :: Map StakeCredential Lovelace,
    mirddTreasuryPayouts :: Map StakeCredential Lovelace,
    mirddReservesToTreasury :: Lovelace,
    mirddTreasuryToReserves :: Lovelace
  }

data PoolReapDetails = PoolReapDetails
  { prdEpochNo :: EpochNo,
    -- | Refunded deposits. The pools referenced are now retired, and the
    --   'StakeCredential' accounts are credited with the deposits.
    prdRefunded :: Map StakeCredential (Map (Hash StakePoolKey) Lovelace),
    -- | Unclaimed deposits. The 'StakeCredential' referenced in this map is not
    -- actively registered at the time of the pool reaping, and as such the
    -- funds are returned to the treasury.
    prdUnclaimed :: Map StakeCredential (Map (Hash StakePoolKey) Lovelace)
  }

--------------------------------------------------------------------------------
-- Patterns for event access
--------------------------------------------------------------------------------

pattern LERewardEvent ::
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  ) =>
  EpochNo ->
  Map StakeCredential (Set (Reward StandardCrypto)) ->
  AuxLedgerEvent (LedgerState (ShelleyBlock protocol ledgerera))
pattern LERewardEvent e m <-
  ShelleyLedgerEventTICK
    (NewEpochEvent (TotalRewardEvent e (Map.mapKeys fromShelleyStakeCredential -> m)))

pattern LEDeltaRewardEvent ::
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "RUPD" ledgerera) ~ RupdEvent (Crypto ledgerera)
  ) =>
  EpochNo ->
  Map StakeCredential (Set (Reward StandardCrypto)) ->
  AuxLedgerEvent (LedgerState (ShelleyBlock protocol ledgerera))
pattern LEDeltaRewardEvent e m <-
  ShelleyLedgerEventTICK
    (NewEpochEvent (DeltaRewardEvent (RupdEvent e (Map.mapKeys fromShelleyStakeCredential -> m))))

pattern LEMirTransfer ::
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "MIR" ledgerera) ~ ShelleyMirEvent ledgerera
  ) =>
  Map StakeCredential Lovelace ->
  Map StakeCredential Lovelace ->
  Lovelace ->
  Lovelace ->
  AuxLedgerEvent (LedgerState (ShelleyBlock protocol ledgerera))
pattern LEMirTransfer rp tp rtt ttr <-
  ShelleyLedgerEventTICK
    ( NewEpochEvent
        ( MirEvent
            ( MirTransfer
                ( InstantaneousRewards
                    (Map.mapKeys fromShelleyStakeCredential . fmap fromShelleyLovelace -> rp)
                    (Map.mapKeys fromShelleyStakeCredential . fmap fromShelleyLovelace -> tp)
                    (fromShelleyDeltaLovelace -> rtt)
                    (fromShelleyDeltaLovelace -> ttr)
                  )
              )
          )
      )

pattern LERetiredPools ::
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "EPOCH" ledgerera) ~ ShelleyEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "POOLREAP" ledgerera) ~ ShelleyPoolreapEvent ledgerera
  ) =>
  Map StakeCredential (Map (Hash StakePoolKey) Lovelace) ->
  Map StakeCredential (Map (Hash StakePoolKey) Lovelace) ->
  EpochNo ->
  AuxLedgerEvent (LedgerState (ShelleyBlock protocol ledgerera))
pattern LERetiredPools r u e <-
  ShelleyLedgerEventTICK
    ( NewEpochEvent
        ( EpochEvent
            ( PoolReapEvent
                ( RetiredPools
                    (convertRetiredPoolsMap -> r)
                    (convertRetiredPoolsMap -> u)
                    e
                  )
              )
          )
      )

convertRetiredPoolsMap ::
     Map (Ledger.StakeCredential StandardCrypto) (Map (Ledger.KeyHash Ledger.StakePool StandardCrypto) Ledger.Coin)
  -> Map StakeCredential (Map (Hash StakePoolKey) Lovelace)
convertRetiredPoolsMap =
  Map.mapKeys fromShelleyStakeCredential
    . fmap (Map.mapKeys StakePoolKeyHash . fmap fromShelleyLovelace)
