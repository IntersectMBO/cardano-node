{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}

module Cardano.Node.Tracing.Tracers.Shutdown
  ( ppShutdownTrace
  ) where

import           Cardano.Logging
import           Cardano.Node.Handlers.Shutdown

import           Prelude (Maybe (..), show)

import           Data.Aeson (Value (..), (.=))
import           Data.Monoid (mconcat, (<>))
import           Data.Text (Text, pack)

-- --------------------------------------------------------------------------------
-- -- ShutdownTrace Tracer
-- --------------------------------------------------------------------------------

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
    ShutdownArmedAt lim ->
          mconcat [ "kind"   .= String "ShutdownArmedAt"
                  , "limit"  .= lim ]

ppShutdownTrace :: ShutdownTrace -> Text
ppShutdownTrace = \case
  ShutdownRequested             -> "Received shutdown request"
  AbnormalShutdown              -> "non-isEOFerror shutdown request"
  ShutdownUnexpectedInput text  ->
    "Received shutdown request but found unexpected input in --shutdown-ipc FD: " <> text
  RequestingShutdown reason     -> "Ringing the node shutdown doorbell:  " <> reason
  ShutdownArmedAt lim           -> "Will terminate upon reaching " <> pack (show lim)

instance MetaTrace ShutdownTrace where
  namespaceFor ShutdownRequested {} =
    Namespace [] ["Requested"]
  namespaceFor AbnormalShutdown {} =
    Namespace [] ["Abnormal"]
  namespaceFor ShutdownUnexpectedInput {} =
    Namespace [] ["UnexpectedInput"]
  namespaceFor RequestingShutdown {} =
    Namespace [] ["Requesting"]
  namespaceFor ShutdownArmedAt {} =
    Namespace [] ["ArmedAt"]

  severityFor  (Namespace _ ["Requested"]) _ =
    Just Warning
  severityFor  (Namespace _ ["Abnormal"]) _ =
    Just Error
  severityFor  (Namespace _ ["UnexpectedInput"]) _ =
    Just Error
  severityFor  (Namespace _ ["Requesting"]) _ =
    Just Warning
  severityFor  (Namespace _ ["ArmedAt"]) _ =
    Just Warning
  severityFor _ns _ =
    Nothing

  documentFor  (Namespace _ ["Requested"]) =
    Just "Node shutdown was requested."
  documentFor  (Namespace _ ["Abnormal"]) =
    Just "non-isEOFerror shutdown request"
  documentFor  (Namespace _ ["UnexpectedInput"]) =
    Just "Received shutdown request but found unexpected input in --shutdown-ipc FD: "
  documentFor  (Namespace _ ["Requesting"]) =
    Just "Ringing the node shutdown doorbell"
  documentFor  (Namespace _ ["ArmedAt"])  =
    Just "Setting up node shutdown at given slot / block."
  documentFor _ns =
    Nothing

  allNamespaces =
    [ Namespace [] ["Requested"]
    , Namespace [] ["Abnormal"]
    , Namespace [] ["UnexpectedInput"]
    , Namespace [] ["Requesting"]
    , Namespace [] ["ArmedAt"]
    ]

