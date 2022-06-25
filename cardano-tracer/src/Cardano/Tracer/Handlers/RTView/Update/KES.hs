{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.KES
  ( updateKESInfo
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_)
import           Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as M
import           Data.Text (pack)
import           Data.Text.Read (decimal)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (UI, liftIO)
import           Text.Printf (printf)

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.EraSettings
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

updateKESInfo
  :: UI.Window
  -> AcceptedMetrics
  -> ErasSettings
  -> DisplayedElements
  -> UI ()
updateKESInfo _window acceptedMetrics settings displayed = do
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ (M.toList allMetrics) $ \(nodeId@(NodeId anId), (ekgStore, _)) -> do
    metrics <- liftIO $ getListOfMetrics ekgStore
    forM_ metrics $ \(metricName, metricValue) ->
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
            daysUntilRenew = fromIntegral secondsUntilRenew / 3600 / 24
        setDisplayedValue nodeId displayed (anId <> "__node-days-until-op-cert-renew") $
                          pack $ printf "%.1f" daysUntilRenew
