{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
module Cardano.TraceDispatcher.Tracers.Shutdown
  ( ppShutdownTrace
  , namesShutdown
  , docShutdown
  ) where

import Prelude (show)
import Data.Monoid ((<>))
import Data.Aeson (ToJSON (..), Value (..), (.=))
import Data.Text (Text, pack)
import Cardano.Logging
import Cardano.Node.Handlers.Shutdown

--------------------------------------------------------------------------------
-- ShutdownTrace Tracer
--------------------------------------------------------------------------------


namesShutdown :: ShutdownTrace -> [Text]
namesShutdown = \case
  ShutdownRequested{}         -> ["ShutdownRequested"]
  RequestingShutdown{}        -> ["RequestingShutdown"]
  ShutdownArmedAtSlot{}       -> ["ShutdownArmedAtSlot"]

ppShutdownTrace :: ShutdownTrace -> Text
ppShutdownTrace =
  \case
    ShutdownRequested         -> "Received shutdown request"
    RequestingShutdown reason -> "Ringing the node shutdown doorbell:  " <> reason
    ShutdownArmedAtSlot slot  -> "Will terminate upon reaching " <> pack (show slot)

instance LogFormatting ShutdownTrace where
  forHuman = ppShutdownTrace

  forMachine _ = \case
    ShutdownRequested         ->
          mkObject [ "kind"   .= String "ShutdownRequested" ]
    RequestingShutdown reason ->
          mkObject [ "kind"   .= String "RequestingShutdown"
                   , "reason" .= String reason ]
    ShutdownArmedAtSlot slot  ->
          mkObject [ "kind"   .= String "ShutdownArmedAtSlot"
                   , "slot"   .= toJSON slot ]

docShutdown :: Documented ShutdownTrace
docShutdown = Documented
  [ DocMsg
      ShutdownRequested
      []
      "Node shutdown was requested."
  , DocMsg
      (RequestingShutdown anyProto)
      []
      "Requesting node shutdown."
  , DocMsg
      (ShutdownArmedAtSlot anyProto)
      []
      "Setting up node shutdown at given slot."
  ]
