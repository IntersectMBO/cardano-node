{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.EraSettings
  ( runEraSettingsUpdater
  ) where

import           Cardano.Node.Startup (NodeStartupInfo (..))
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.EraSettings
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Handlers.RTView.Utils

import           Control.Monad (forever)
import           Control.Monad.Extra (whenJustM)
import           Data.Time.Clock (nominalDiffTimeToSeconds)
import           System.Time.Extra (sleep)

runEraSettingsUpdater
  :: TracerEnv
  -> ErasSettings
  -> IO ()
runEraSettingsUpdater tracerEnv settings = forever $ do
  updateErasSettings
  sleep 5.0
 where
  TracerEnv{teDPRequestors, teCurrentDPLock} = tracerEnv

  updateErasSettings =
    forConnected_ tracerEnv $ \nodeId ->
      whenJustM (askDataPoint teDPRequestors teCurrentDPLock nodeId "NodeStartupInfo") $ \nsi ->
        addEraSettings settings nodeId $
          EraSettings
            { esEra             = suiEra nsi
            , esSlotLengthInS   = floor . nominalDiffTimeToSeconds $ suiSlotLength nsi
            , esEpochLength     = fromIntegral $ suiEpochLength nsi
            , esKESPeriodLength = fromIntegral $ suiSlotsPerKESPeriod nsi
            }
