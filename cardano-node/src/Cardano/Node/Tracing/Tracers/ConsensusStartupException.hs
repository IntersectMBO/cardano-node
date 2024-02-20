-- | Consensus startup exception
--
-- TODO: once the legacy logging framework is removed it can be moved to
-- `Cardano.Node.Tracing.Tracers.Consensus`.
module Cardano.Node.Tracing.Tracers.ConsensusStartupException
  ( ConsensusStartupException (..)
  ) where

import           Cardano.Logging.Types

import           Control.Exception (SomeException)
import           Data.Aeson (Value (String), (.=))
import qualified Data.Text as Text

-- | Exceptions logged when the consensus is initialising.
--
newtype ConsensusStartupException = ConsensusStartupException SomeException
  deriving Show

instance LogFormatting ConsensusStartupException where
  forMachine _ (ConsensusStartupException err) =
    mconcat [ "kind" .= String "ConsensusStartupException"
            , "error" .= String (Text.pack . show $ err)
            ]
  forHuman = Text.pack . show

instance MetaTrace ConsensusStartupException where
  namespaceFor ConsensusStartupException {} = Namespace [] ["ConsensusStartupException"]

  severityFor (Namespace _ ["ConsensusStartupException"]) Nothing = Just Error
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["ConsensusStartupException"]) = Just
    ""
  documentFor _ = Just ""

  allNamespaces = [Namespace [] ["ConsensusStartupException"]]
