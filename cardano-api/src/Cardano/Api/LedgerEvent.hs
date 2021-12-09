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
import qualified Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Credential
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Shelley.Rewards (Reward (..))
import qualified Cardano.Ledger.Keys
import           Control.State.Transition (Event)
import           Data.Function (($), (.))
import           Data.Functor (fmap)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
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
import           Cardano.Ledger.Shelley.API (InstantaneousRewards (InstantaneousRewards))
import           Cardano.Ledger.Shelley.Rules.Epoch (EpochEvent (PoolReapEvent))
import           Cardano.Ledger.Shelley.Rules.Mir (MirEvent (..))
import           Cardano.Ledger.Shelley.Rules.NewEpoch (NewEpochEvent (EpochEvent, MirEvent, RewardEvent))
import           Cardano.Ledger.Shelley.Rules.PoolReap (PoolreapEvent (RetiredPools))
import           Cardano.Ledger.Shelley.Rules.Tick (TickEvent (NewEpochEvent))

data LedgerEvent
  = -- | The given pool is being registered for the first time on chain.
    PoolRegistration Certificate
  | -- | The given pool already exists and is being re-registered.
    PoolReRegistration Certificate
  | -- | Rewards are being distributed.
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
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ TickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "EPOCH" ledgerera) ~ EpochEvent ledgerera,
    Event (Ledger.Core.EraRule "POOLREAP" ledgerera) ~ PoolreapEvent ledgerera,
    Event (Ledger.Core.EraRule "MIR" ledgerera) ~ MirEvent ledgerera
  ) =>
  ConvertLedgerEvent (ShelleyBlock ledgerera)
  where
  toLedgerEvent evt = case unwrapLedgerEvent evt of
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
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ TickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
  ) =>
  EpochNo ->
  Map StakeCredential (Set (Reward StandardCrypto)) ->
  AuxLedgerEvent (LedgerState (ShelleyBlock ledgerera))
pattern LERewardEvent e m <-
  ShelleyLedgerEventTICK
    (NewEpochEvent (RewardEvent e (convertRewardEventMapNew -> m)))

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
                    (convertSumRewardsMapSimple -> rp)
                    (convertSumRewardsMapSimple -> tp)
                    (fromShelleyDeltaLovelace -> rtt)
                    (fromShelleyDeltaLovelace -> ttr)
                  )
              )
          )
      )

convertRewardEventMapNew ::
  Map
    ( Cardano.Ledger.Credential.StakeCredential
        Cardano.Ledger.Crypto.StandardCrypto
    )
    (Set (Reward StandardCrypto)) ->
  Map StakeCredential (Set (Reward StandardCrypto))
convertRewardEventMapNew =
  Map.mapKeys fromShelleyStakeCredential

convertSumRewardsMapSimple ::
  Map
    ( Cardano.Ledger.Credential.StakeCredential
        Cardano.Ledger.Crypto.StandardCrypto
    )
    Cardano.Ledger.Coin.Coin ->
  Map StakeCredential Lovelace
convertSumRewardsMapSimple =
  Map.mapKeys fromShelleyStakeCredential . fmap fromShelleyLovelace

pattern LERetiredPools ::
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ TickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "EPOCH" ledgerera) ~ EpochEvent ledgerera,
    Event (Ledger.Core.EraRule "POOLREAP" ledgerera) ~ PoolreapEvent ledgerera
  ) =>
  Map StakeCredential (Map (Hash StakePoolKey) Lovelace) ->
  Map StakeCredential (Map (Hash StakePoolKey) Lovelace) ->
  EpochNo ->
  AuxLedgerEvent (LedgerState (ShelleyBlock ledgerera))
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
  Map
    ( Cardano.Ledger.Credential.StakeCredential
        Cardano.Ledger.Crypto.StandardCrypto
    )
    ( Map
        (Cardano.Ledger.Keys.KeyHash Cardano.Ledger.Keys.StakePool StandardCrypto)
        Cardano.Ledger.Coin.Coin
    ) ->
  Map StakeCredential (Map (Hash StakePoolKey) Lovelace)
convertRetiredPoolsMap =
  Map.mapKeys fromShelleyStakeCredential
    . fmap (Map.mapKeys StakePoolKeyHash . fmap fromShelleyLovelace)
