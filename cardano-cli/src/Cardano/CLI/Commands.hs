module Cardano.CLI.Commands
  ( ClientCommand (..)
  ) where

import           Cardano.Prelude

import           Cardano.CLI.Byron.Commands (ByronCommand (..))
import           Cardano.CLI.Shelley.Commands (ShelleyCommand (..))


-- | Sub-commands of 'cardano-cli'.
data ClientCommand =

    -- | Byron Related Commands
    ByronCommand ByronCommand

    -- | Shelley Related Commands
  | ShelleyCommand ShelleyCommand

  | DisplayVersion
  deriving Show

