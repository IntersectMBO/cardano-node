{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.EraSettings
  ( runEraSettingsUpdater
  ) where

import           Control.Concurrent.Extra (Lock)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_, forever)
import           Control.Monad.Extra (whenJustM)
import           Data.Set (Set)
import           Data.Time.Clock (nominalDiffTimeToSeconds)
import           System.Time.Extra (sleep)

import           Cardano.Node.Startup (NodeStartupInfo (..))

import           Cardano.Tracer.Handlers.RTView.State.EraSettings
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

runEraSettingsUpdater
  :: ConnectedNodes
  -> ErasSettings
  -> DataPointRequestors
  -> Lock
  -> IO ()
runEraSettingsUpdater connectedNodes settings dpRequestors currentDPLock = forever $ do
  connected <- readTVarIO connectedNodes
  updateErasSettings connected settings dpRequestors currentDPLock
  sleep 5.0

updateErasSettings
  :: Set NodeId
  -> ErasSettings
  -> DataPointRequestors
  -> Lock
  -> IO ()
updateErasSettings connected settings dpRequestors currentDPLock =
  forM_ connected $ \nodeId ->
    whenJustM (askDataPoint dpRequestors currentDPLock nodeId "NodeStartupInfo") $ \nsi ->
      addEraSettings settings nodeId $
        EraSettings
          { esEra             = suiEra nsi
          , esSlotLengthInS   = floor . nominalDiffTimeToSeconds $ suiSlotLength nsi
          , esEpochLength     = fromIntegral $ suiEpochLength nsi
          , esKESPeriodLength = fromIntegral $ suiSlotsPerKESPeriod nsi
          }
