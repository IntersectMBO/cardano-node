{-# LANGUAGE LambdaCase #-}
module Cardano.Node.Tracing.Tracers.ConsensusSanityCheckWarning
  ( ConsensusSanityCheckWarning(..)
  ) where

import Ouroboros.Consensus.Block.SupportsSanityCheck
import Cardano.Logging
import qualified Data.Text as Text
import Data.Aeson
import Ouroboros.Consensus.Config.SecurityParam
import Control.Exception

newtype ConsensusSanityCheckWarning =
  ConsensusSanityCheckWarning SanityCheckIssue
  deriving (Show)

instance LogFormatting ConsensusSanityCheckWarning where
  forMachine _ (ConsensusSanityCheckWarning s) =
    case s of
      InconsistentSecurityParam ps ->
        mconcat [ "kind" .= String "InconsistentSecurityParam"
                , "securityParams" .= fmap maxRollbacks ps
                ]
  forHuman (ConsensusSanityCheckWarning s) =
    Text.pack $ displayException s

instance MetaTrace ConsensusSanityCheckWarning where
  namespaceFor (ConsensusSanityCheckWarning s) = case s of
    InconsistentSecurityParam _ ->
      Namespace [] ["InconsistentSecurityParam"]

  severityFor ns _ = case ns of
    Namespace [] ["InconsistentSecurityParam"] -> Just Warning
    Namespace _ _ -> Nothing

  documentFor = \case
    Namespace [] ["InconsistentSecurityParam"] ->
      Just "SecurityParams (k) were found to be inconsistent between eras"
    Namespace _ _ -> Nothing

  allNamespaces =
    [ Namespace [] ["InconsistentSecurityParam"]
    ]
