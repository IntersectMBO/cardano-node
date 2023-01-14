{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.KESInfo
  ( severityKESInfo
  , namesForKESInfo
  , traceAsKESInfo
  , docForgeKESInfo
  ) where

import           Data.SOP.Strict

import           Cardano.Logging
import           Cardano.Prelude hiding (All, Show, show)

import           Cardano.Node.Queries (GetKESInfo (..))
import           Cardano.Protocol.TPraos.OCert (KESPeriod (..))

import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.Node.Tracers (TraceLabelCreds (..))
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey

traceAsKESInfo
  :: forall m blk . (GetKESInfo blk, MonadIO m)
  => Proxy blk
  -> Trace m (TraceLabelCreds HotKey.KESInfo)
  -> Trace m (TraceLabelCreds (ForgeStateInfo blk))
traceAsKESInfo pr tr = traceAsMaybeKESInfo pr (filterTraceMaybe tr)

traceAsMaybeKESInfo
  :: forall m blk . (GetKESInfo blk, MonadIO m)
  => Proxy blk
  -> Trace m (Maybe (TraceLabelCreds HotKey.KESInfo))
  -> Trace m (TraceLabelCreds (ForgeStateInfo blk))
traceAsMaybeKESInfo pr (Trace tr) = Trace $
  contramap
        (\case
          (lc, Right (TraceLabelCreds c e)) ->
            case getKESInfoFromStateInfo pr e of
              Just kesi -> (lc, Right (Just (TraceLabelCreds c kesi)))
              Nothing   -> (lc, Right Nothing)
          (lc, Left ctrl) -> (lc, Left ctrl))
        tr

--------------------------------------------------------------------------------
-- KESInfo Tracer
--------------------------------------------------------------------------------

severityKESInfo :: TraceLabelCreds HotKey.KESInfo -> SeverityS
severityKESInfo (TraceLabelCreds _creds a) = severityKESInfo'  a

severityKESInfo' :: HotKey.KESInfo -> SeverityS
severityKESInfo' forgeStateInfo =
    let maxKesEvos = endKesPeriod - startKesPeriod
        oCertExpiryKesPeriod = startKesPeriod + maxKesEvos
        kesPeriodsUntilExpiry = max 0 (oCertExpiryKesPeriod - currKesPeriod)
    in if kesPeriodsUntilExpiry > 7
      then Info
      else if kesPeriodsUntilExpiry <= 1
        then Alert
        else Warning
    where
    HotKey.KESInfo
      { HotKey.kesStartPeriod = KESPeriod startKesPeriod
      , HotKey.kesEvolution = currKesPeriod
      , HotKey.kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo

namesForKESInfo :: TraceLabelCreds HotKey.KESInfo -> [Text]
namesForKESInfo (TraceLabelCreds _creds a) = namesForKESInfo' a

namesForKESInfo' :: HotKey.KESInfo -> [Text]
namesForKESInfo' _fsi = []

docForgeKESInfo :: Documented (TraceLabelCreds HotKey.KESInfo)
docForgeKESInfo = Documented [
    DocMsg
      []
      []
      "kesStartPeriod \
      \\nkesEndPeriod is kesStartPeriod + tpraosMaxKESEvo\
      \\nkesEvolution is the current evolution or /relative period/."
    ]
