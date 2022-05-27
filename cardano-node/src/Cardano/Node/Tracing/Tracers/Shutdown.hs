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
import           Data.Aeson (Value (..), (.=))
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
  ShutdownArmedAtSlotBlock{}  -> ["ShutdownArmedAtSlotBlock"]

severityShutdown :: ShutdownTrace -> SeverityS
severityShutdown = \case
  ShutdownRequested{}         -> Warning
  AbnormalShutdown{}          -> Error
  ShutdownUnexpectedInput{}   -> Error
  RequestingShutdown{}        -> Warning
  ShutdownArmedAtSlotBlock{}  -> Warning

ppShutdownTrace :: ShutdownTrace -> Text
ppShutdownTrace = \case
  ShutdownRequested             -> "Received shutdown request"
  AbnormalShutdown              -> "non-isEOFerror shutdown request"
  ShutdownUnexpectedInput text  ->
    "Received shutdown request but found unexpected input in --shutdown-ipc FD: " <> text
  RequestingShutdown reason     -> "Ringing the node shutdown doorbell:  " <> reason
  ShutdownArmedAtSlotBlock s b  -> "Will terminate upon reaching " <> pack (show s) <> " / " <> pack (show b)

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
    ShutdownArmedAtSlotBlock s b ->
          mconcat [ "kind"   .= String "ShutdownArmedAtSlotBlock"
                  , "slot"   .= s
                  , "block"  .= b ]

docShutdown :: Documented ShutdownTrace
docShutdown = addDocumentedNamespace  [] docShutdown'

docShutdown' :: Documented ShutdownTrace
docShutdown' = Documented
  [ DocMsg
      ["ShutdownRequested"]
      []
      "Node shutdown was requested."
  ,  DocMsg
      ["AbnormalShutdown"]
      []
      "non-isEOFerror shutdown request"
  ,  DocMsg
      ["ShutdownUnexpectedInput"]
      []
      "Received shutdown request but found unexpected input in --shutdown-ipc FD: "
  , DocMsg
      ["RequestingShutdown"]
      []
      "Ringing the node shutdown doorbell"
  , DocMsg
      ["ShutdownArmedAtSlotBlock"]
      []
      "Setting up node shutdown at given slot / block."
  ]
