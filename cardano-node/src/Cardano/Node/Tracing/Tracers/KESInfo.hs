{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.KESInfo
   (
      traceAsKESInfo
   ) where

import           Cardano.Logging
import           Cardano.Node.Queries (GetKESInfo (..))
import           Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod))
import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.Node.Tracers (TraceLabelCreds (..))
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson (ToJSON (..), Value (..), (.=))
import           Data.Proxy (Proxy)
import qualified Data.Text as Text

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

-- --------------------------------------------------------------------------------
-- -- KESInfo Tracer
-- --------------------------------------------------------------------------------

deriving newtype instance ToJSON KESPeriod


instance LogFormatting HotKey.KESInfo where
  forMachine _dtal forgeStateInfo =
    let currKesPeriod' = currKesPeriod + startKesPeriod
        maxKesEvos = endKesPeriod - startKesPeriod
        expiryKesPeriod = startKesPeriod + maxKesEvos
        kesPeriodsUntilExpiry = max 0 (expiryKesPeriod - currKesPeriod')
    in
      if kesPeriodsUntilExpiry > 7
        then mconcat
              [ "kind" .= String "KESInfo"
              , "startPeriod" .= startKesPeriod
              , "endPeriod" .= currKesPeriod'
              , "evolution" .= endKesPeriod
              ]
        else mconcat
              [ "kind" .= String "ExpiryLogMessage"
              , "keyExpiresIn" .= kesPeriodsUntilExpiry
              , "startPeriod" .= startKesPeriod
              , "endPeriod" .= currKesPeriod'
              , "evolution" .= endKesPeriod
              ]
    where
    HotKey.KESInfo
      { HotKey.kesStartPeriod = KESPeriod startKesPeriod
      , HotKey.kesEvolution = currKesPeriod
      , HotKey.kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo

  forHuman forgeStateInfo =
    let currKesPeriod' = currKesPeriod + startKesPeriod
        maxKesEvos = endKesPeriod - startKesPeriod
        expiryKesPeriod = startKesPeriod + maxKesEvos
        kesPeriodsUntilExpiry = max 0 (expiryKesPeriod - currKesPeriod')
    in if kesPeriodsUntilExpiry > 7
      then "KES info startPeriod  " <> (Text.pack . show) startKesPeriod
            <> " currPeriod " <> (Text.pack . show) currKesPeriod'
            <> " endPeriod " <> (Text.pack . show) endKesPeriod
            <> ", "
            <> (Text.pack . show) kesPeriodsUntilExpiry
            <> " KES periods until expiry."
      else "Operational key will expire in "
             <> (Text.pack . show) kesPeriodsUntilExpiry
             <> " KES periods."
    where
    HotKey.KESInfo
      { HotKey.kesStartPeriod = KESPeriod startKesPeriod
      , HotKey.kesEvolution = currKesPeriod
      , HotKey.kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo

  asMetrics forgeStateInfo =
      let currKesPeriod' = currKesPeriod + startKesPeriod
          maxKesEvos = endKesPeriod - startKesPeriod
          expiryKesPeriod = startKesPeriod + maxKesEvos
          kesPeriodsUntilExpiry = max 0 (expiryKesPeriod - currKesPeriod')
      in  [
            IntM "operationalCertificateStartKESPeriod" (fromIntegral startKesPeriod)
          , IntM "operationalCertificateExpiryKESPeriod" (fromIntegral expiryKesPeriod)
          , IntM "currentKESPeriod" (fromIntegral currKesPeriod')
          , IntM "remainingKESPeriods" (fromIntegral kesPeriodsUntilExpiry)
          ]
    where
    HotKey.KESInfo
      { HotKey.kesStartPeriod = KESPeriod startKesPeriod
      , HotKey.kesEvolution = currKesPeriod
      , HotKey.kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo


instance MetaTrace HotKey.KESInfo where
    namespaceFor HotKey.KESInfo {} = Namespace [] ["StateInfo"]

    severityFor (Namespace _ _) (Just forgeStateInfo) = Just $
      let currKesPeriod' = currKesPeriod + startKesPeriod
          maxKesEvos = endKesPeriod - startKesPeriod
          expiryKesPeriod = startKesPeriod + maxKesEvos
          kesPeriodsUntilExpiry = max 0 (expiryKesPeriod - currKesPeriod')
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

    severityFor (Namespace _ ["StateInfo"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["StateInfo"]) = Just
      "kesStartPeriod \
       \\nkesEndPeriod is kesStartPeriod + tpraosMaxKESEvo\
       \\nkesEvolution is the current evolution or /relative period/."
    documentFor _ = Nothing

    metricsDocFor (Namespace _ ["StateInfo"]) =
        [ ("operationalCertificateStartKESPeriod", "")
        , ("operationalCertificateExpiryKESPeriod", "")
        , ("currentKESPeriod", "")
        , ("remainingKESPeriods", "")]
    metricsDocFor _ = []

    allNamespaces = [Namespace [] ["StateInfo"]]




instance LogFormatting HotKey.KESEvolutionError where
  forMachine dtal (HotKey.KESCouldNotEvolve kesInfo targetPeriod) =
    mconcat
      [ "kind" .= String "KESCouldNotEvolve"
      , "kesInfo" .= forMachine dtal kesInfo
      , "targetPeriod" .= targetPeriod
      ]
  forMachine dtal (HotKey.KESKeyAlreadyPoisoned kesInfo targetPeriod) =
    mconcat
      [ "kind" .= String "KESKeyAlreadyPoisoned"
      , "kesInfo" .= forMachine dtal kesInfo
      , "targetPeriod" .= targetPeriod
      ]




