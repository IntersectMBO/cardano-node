module Cardano.Chairman.Commands where

import           Cardano.Chairman.Commands.Run
import           Cardano.Chairman.Commands.Version
import           Options.Applicative

{- HLINT ignore "Monoid law, left identity" -}

commands :: Parser (IO ())
commands = subparser $ mempty
  <>  cmdRun
  <>  cmdVersion
