
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Tracing.Metrics
  ( KESMetricsData (..)
  , MaxKESEvolutions (..)
  , OperationalCertStartKESPeriod (..)
  , HasKESMetricsData (..)
  ) where

import           Cardano.Prelude

import           Cardano.Crypto.KES.Class (Period)
import           Ouroboros.Consensus.Block (ForgeState (..))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Unary
import           Ouroboros.Consensus.Mock.Ledger.Block (SimpleBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool ()
import           Ouroboros.Consensus.Shelley.Protocol.Crypto.HotKey (HotKey (..))
import           Shelley.Spec.Ledger.OCert (KESPeriod (..))


-- | KES-related data to be traced as metrics.
data KESMetricsData
  = NoKESMetricsData
  -- ^ The current protocol does not support KES.
  | TPraosKESMetricsData
      !Period
      -- ^ The current KES period of the hot key, relative to the start KES
      -- period of the operational certificate.
      !MaxKESEvolutions
      -- ^ The configured max KES evolutions.
      !OperationalCertStartKESPeriod
      -- ^ The start KES period of the configured operational certificate.

-- | The maximum number of evolutions that a KES key can undergo before it is
-- considered expired.
newtype MaxKESEvolutions = MaxKESEvolutions Word64

-- | The start KES period of the configured operational certificate.
newtype OperationalCertStartKESPeriod = OperationalCertStartKESPeriod Period

class HasKESMetricsData blk where
  getKESMetricsData :: ForgeState blk -> KESMetricsData

  -- Default to 'NoKESMetricsData'
  getKESMetricsData _ = NoKESMetricsData

instance HasKESMetricsData (ShelleyBlock c) where
  getKESMetricsData forgeState =
      TPraosKESMetricsData currKesPeriod maxKesEvos oCertStartKesPeriod
    where
      HotKey
        { hkStart     = KESPeriod startKesPeriod
        , hkEvolution = currKesPeriod
        , hkEnd       = KESPeriod endKesPeriod
        } = chainIndepState forgeState

      maxKesEvos = MaxKESEvolutions $
          fromIntegral $ endKesPeriod - startKesPeriod

      oCertStartKesPeriod = OperationalCertStartKESPeriod startKesPeriod

instance HasKESMetricsData ByronBlock where

instance HasKESMetricsData (SimpleBlock a b) where

instance (HasKESMetricsData x, NoHardForks x)
      => HasKESMetricsData (HardForkBlock '[x]) where
  getKESMetricsData forgeState =
    getKESMetricsData (project forgeState)
