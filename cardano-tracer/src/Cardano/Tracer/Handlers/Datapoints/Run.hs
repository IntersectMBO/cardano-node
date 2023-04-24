{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Handlers.Datapoints.Run
  ( runDatapoints
  ) where

import Control.Monad.Extra (whenJust)

import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.NodeToClient (withIOManager)

import Cardano.Logging.Forwarding
import Cardano.Logging.Tracer.DataPoint
import Cardano.Logging.Types qualified as Log
import Trace.Forward.Utils.DataPoint

import Cardano.Tracer.Configuration
import Cardano.Tracer.MetaTrace


runDatapoints :: TracerConfig -> Log.Trace IO TracerTrace -> IO ()
runDatapoints TracerConfig{networkMagic, hasForwarding} teTracer =
  whenJust hasForwarding $ \case
    (ConnectTo{}, _) ->
      error "runDatapoints:  unsupported mode of operation:  ConnectTo.  Use AcceptAt."
    (AcceptAt (LocalSocket socket), forwConf) -> do
      (_, dpStore :: DataPointStore) <- withIOManager $ \iomgr -> do
        traceWith teTracer TracerStartedDataPoints
        initForwarding iomgr forwConf (NetworkMagic networkMagic) Nothing (Just (socket, Log.Responder))
      let _tr = dataPointTracer @IO dpStore
      pure ()
