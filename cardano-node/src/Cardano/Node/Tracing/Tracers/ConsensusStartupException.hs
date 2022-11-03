-- | Consensus startup exception
--
-- TODO: once the legacy logging framework is removed it can be moved to
-- `Cardano.Node.Tracing.Tracers.Consensus`.
module Cardano.Node.Tracing.Tracers.ConsensusStartupException
  ( ConsensusStartupException (..)
  ) where

import           Cardano.Prelude hiding (Show, show)

import           Data.Aeson (Value (String), (.=))
import qualified Data.Text as Text
import           Text.Show

import           Cardano.Logging.Types

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
