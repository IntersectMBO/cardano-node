module Cardano.Node.Peras.Trace
  ( PerasVoteForgingUpdateResult (..)
  ) where

data PerasVoteForgingUpdateResult
  = PerasVoteForgingInstalled
  | PerasVoteForgingDisabledUnsupported
  | PerasVoteForgingCleared
  | PerasVoteForgingRetainedAfterError String
  | PerasVoteForgingAmbiguousCredentials Int
  deriving (Eq, Show)
