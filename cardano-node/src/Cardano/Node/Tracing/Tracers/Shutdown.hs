{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}

module Cardano.Node.Tracing.Tracers.Shutdown
  ( namesForShutdown
  , severityShutdown
  , ppShutdownTrace
  , docShutdown
  ) where

import           Cardano.Logging
import           Cardano.Node.Handlers.Shutdown
import           Data.Aeson (ToJSON (..), Value (..), (.=))
import           Data.Monoid (mconcat, (<>))
import           Data.Text (Text, pack)
import           Prelude (show)

--------------------------------------------------------------------------------
-- ShutdownTrace Tracer
--------------------------------------------------------------------------------

namesForShutdown :: ShutdownTrace -> [Text]
namesForShutdown = \case
  ShutdownRequested{}         -> ["ShutdownRequested"]
  AbnormalShutdown{}          -> ["AbnormalShutdown"]
  ShutdownUnexpectedInput{}   -> ["ShutdownUnexpectedInput"]
  RequestingShutdown{}        -> ["RequestingShutdown"]
  ShutdownArmedAtSlot{}       -> ["ShutdownArmedAtSlot"]

severityShutdown :: ShutdownTrace -> SeverityS
severityShutdown = \case
  ShutdownRequested{}         -> Warning
  AbnormalShutdown{}          -> Error
  ShutdownUnexpectedInput{}   -> Error
  RequestingShutdown{}        -> Warning
  ShutdownArmedAtSlot{}       -> Warning

ppShutdownTrace :: ShutdownTrace -> Text
ppShutdownTrace = \case
  ShutdownRequested             -> "Received shutdown request"
  AbnormalShutdown              -> "non-isEOFerror shutdown request"
  ShutdownUnexpectedInput text  ->
    "Received shutdown request but found unexpected input in --shutdown-ipc FD: " <> text
  RequestingShutdown reason     -> "Ringing the node shutdown doorbell:  " <> reason
  ShutdownArmedAtSlot slot      -> "Will terminate upon reaching " <> pack (show slot)

instance LogFormatting ShutdownTrace where
  forHuman = ppShutdownTrace

  forMachine _ = \case
    ShutdownRequested ->
          mconcat [ "kind"   .= String "ShutdownRequested" ]
    AbnormalShutdown  ->
          mconcat [ "kind"   .= String "AbnormalShutdown" ]
    ShutdownUnexpectedInput text ->
          mconcat [ "kind"   .= String "AbnormalShutdown"
                   , "unexpected" .= String text ]
    RequestingShutdown reason ->
          mconcat [ "kind"   .= String "RequestingShutdown"
                   , "reason" .= String reason ]
    ShutdownArmedAtSlot slot  ->
          mconcat [ "kind"   .= String "ShutdownArmedAtSlot"
                   , "slot"   .= toJSON slot ]

docShutdown :: Documented ShutdownTrace
docShutdown = Documented
  [ DocMsg
      ShutdownRequested
      []
      "Node shutdown was requested."
  ,  DocMsg
      AbnormalShutdown
      []
      "non-isEOFerror shutdown request"
  ,  DocMsg
      (ShutdownUnexpectedInput anyProto)
      []
      "Received shutdown request but found unexpected input in --shutdown-ipc FD: "
  , DocMsg
      (RequestingShutdown anyProto)
      []
      "Ringing the node shutdown doorbell"
  , DocMsg
      (ShutdownArmedAtSlot anyProto)
      []
      "Setting up node shutdown at given slot."
  ]
