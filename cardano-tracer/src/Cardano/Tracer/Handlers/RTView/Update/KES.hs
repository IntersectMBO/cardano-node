{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.KES
  ( updateKESInfo
  ) where

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.EraSettings
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as M
import           Data.Text (pack)
import           Data.Text.Read (decimal)
import           Text.Printf (printf)

import           Graphics.UI.Threepenny.Core (UI, liftIO)

updateKESInfo
  :: TracerEnv
  -> ErasSettings
  -> DisplayedElements
  -> UI ()
updateKESInfo tracerEnv settings displayed =
  forAcceptedMetricsUI_ tracerEnv $ \(nodeId@(NodeId anId), (ekgStore, _)) ->
    forMM_ (liftIO $ getListOfMetrics ekgStore) $ \(metricName, metricValue) ->
      case metricName of
        "Forge.CurrentKESPeriod" ->
          setDisplayedValue nodeId displayed (anId <> "__node-current-kes-period") metricValue
        "Forge.OperationalCertificateExpiryKESPeriod" ->
          setDisplayedValue nodeId displayed (anId <> "__node-op-cert-expiry-kes-period") metricValue
        "Forge.OperationalCertificateStartKESPeriod" ->
          setDisplayedValue nodeId displayed (anId <> "__node-op-cert-start-kes-period") metricValue
        "Forge.RemainingKESPeriods" -> do
          setDisplayedValue nodeId displayed (anId <> "__node-remaining-kes-periods") metricValue
          allSettings <- liftIO $ readTVarIO settings
          whenJust (M.lookup nodeId allSettings) $
            setDaysUntilRenew nodeId metricValue
        _ -> return ()
 where
  setDaysUntilRenew nodeId@(NodeId anId) metricValue EraSettings{esKESPeriodLength, esSlotLengthInS} = do
    case decimal metricValue of
      Left _ -> return ()
      Right (remainingKesPeriods :: Int, _) -> do
        let secondsUntilRenew = remainingKesPeriods * esKESPeriodLength * esSlotLengthInS
            daysUntilRenew :: Double
            !daysUntilRenew = fromIntegral secondsUntilRenew / 3600 / 24
        setDisplayedValue nodeId displayed (anId <> "__node-days-until-op-cert-renew") $
                          pack $ printf "%.1f" daysUntilRenew
