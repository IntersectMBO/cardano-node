module Testnet.Commands where

import           Data.Function
import           Data.Monoid
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Commands.ByronShelley
import           Testnet.Commands.Shelley

{- HLINT ignore "Monoid law, left identity" -}

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdByronShelley
  <>  cmdShelley
