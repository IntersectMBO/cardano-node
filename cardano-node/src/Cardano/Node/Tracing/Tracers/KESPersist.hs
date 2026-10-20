{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tracing of the KES key file writer ("Cardano.Node.KES.Persist") under
-- the namespace @Forge.KESPersist@. Only periods, reasons and file names
-- are traced; never any key material.
module Cardano.Node.Tracing.Tracers.KESPersist () where

import           Cardano.Logging
import           Cardano.Node.KES.Persist (TraceKESPersist (..))

import           Data.Aeson (Value (..), (.=))
import           Data.Text (pack)

instance LogFormatting TraceKESPersist where
  forMachine _ = \case
    KESPersisted period ->
      mconcat [ "kind" .= String "KESPersisted"
              , "period" .= period ]
    KESPersistSkipped period reason ->
      mconcat [ "kind" .= String "KESPersistSkipped"
              , "period" .= period
              , "reason" .= String reason ]
    KESPersistFailed period reason ->
      mconcat [ "kind" .= String "KESPersistFailed"
              , "period" .= period
              , "reason" .= String reason ]
    KESFilePeriodWentBackwards onDisk lastWritten ->
      mconcat [ "kind" .= String "KESFilePeriodWentBackwards"
              , "periodOnDisk" .= onDisk
              , "periodLastWritten" .= lastWritten ]
    KESOrphanNotZeroed reason ->
      mconcat [ "kind" .= String "KESOrphanNotZeroed"
              , "reason" .= String reason ]
    KESStaleTempRemoved path ->
      mconcat [ "kind" .= String "KESStaleTempRemoved"
              , "path" .= String (pack path) ]

  forHuman = \case
    KESPersisted period ->
      "Persisted the KES signing key evolved to period " <> showT period <> " to its file"
    KESPersistSkipped period reason ->
      "Did not persist the KES signing key at period " <> showT period <> ": " <> reason
    KESPersistFailed period reason ->
      "Failed to persist the KES signing key at period " <> showT period
        <> " (forging continues): " <> reason
    KESFilePeriodWentBackwards onDisk lastWritten ->
      "The KES signing key file went back to period "
        <> (if onDisk == 0 then "0 (the evolution-zero key)" else showT onDisk)
        <> " after period " <> showT lastWritten
        <> " was written; something restored an older copy of the file, overwriting it again"
    KESOrphanNotZeroed reason ->
      "The replaced KES signing key file was not overwritten with zeros: " <> reason
    KESStaleTempRemoved path ->
      "Removed a stale temporary KES signing key file: " <> pack path

instance MetaTrace TraceKESPersist where
  namespaceFor = \case
    KESPersisted {}               -> Namespace [] ["Persisted"]
    KESPersistSkipped {}          -> Namespace [] ["Skipped"]
    KESPersistFailed {}           -> Namespace [] ["Failed"]
    KESFilePeriodWentBackwards {} -> Namespace [] ["PeriodWentBackwards"]
    KESOrphanNotZeroed {}         -> Namespace [] ["OrphanNotZeroed"]
    KESStaleTempRemoved {}        -> Namespace [] ["StaleTempRemoved"]

  severityFor (Namespace _ ["Persisted"]) _ = Just Info
  severityFor (Namespace _ ["Skipped"]) _ = Just Warning
  severityFor (Namespace _ ["Failed"]) _ = Just Warning
  severityFor (Namespace _ ["PeriodWentBackwards"]) _ = Just Warning
  severityFor (Namespace _ ["OrphanNotZeroed"]) _ = Just Warning
  severityFor (Namespace _ ["StaleTempRemoved"]) _ = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["Persisted"]) = Just
    "The KES signing key, evolved to the given absolute KES period, was \
    \written to its file and read back successfully. Only emitted when \
    \PersistEvolvedKESKey is enabled."
  documentFor (Namespace _ ["Skipped"]) = Just
    "The evolved KES signing key was not written because of what was found \
    \at the key file path: no file, a symbolic link, a mount point, a file \
    \that does not parse, a file holding another key, or a file at a higher \
    \period. The file is left as it is; forging continues."
  documentFor (Namespace _ ["Failed"]) = Just
    "Writing the evolved KES signing key failed (an IO error, or the file \
    \did not read back as written). Forging continues; the file still holds \
    \an earlier evolution."
  documentFor (Namespace _ ["PeriodWentBackwards"]) = Just
    "The KES signing key file held a lower period than the one last written \
    \(period 0 stands for the evolution-zero envelope). Something, such as \
    \configuration management, restored an older copy of the key; it is \
    \overwritten again."
  documentFor (Namespace _ ["OrphanNotZeroed"]) = Just
    "After replacing the KES signing key file, the old file's contents were \
    \not overwritten with zeros: it still has another hard link, or it could \
    \not be opened or written. The new file is in place regardless."
  documentFor (Namespace _ ["StaleTempRemoved"]) = Just
    "A temporary file left behind by an interrupted write of the KES signing \
    \key was removed at startup."
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["Persisted"]
    , Namespace [] ["Skipped"]
    , Namespace [] ["Failed"]
    , Namespace [] ["PeriodWentBackwards"]
    , Namespace [] ["OrphanNotZeroed"]
    , Namespace [] ["StaleTempRemoved"]
    ]
