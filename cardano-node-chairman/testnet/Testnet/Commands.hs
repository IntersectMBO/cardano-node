module Testnet.Commands where

import           Data.Function
import           Data.Monoid
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Commands.Byron
import           Testnet.Commands.Cardano
import           Testnet.Commands.Shelley
import           Testnet.Commands.Version

{- HLINT ignore "Monoid law, left identity" -}

commands :: Parser (IO ())
commands = commandsTestnet <|> commandsGeneral

commandsTestnet :: Parser (IO ())
commandsTestnet = hsubparser $ mempty
  <>  commandGroup "Testnets:"
  <>  cmdByron
  <>  cmdCardano
  <>  cmdShelley

commandsGeneral :: Parser (IO ())
commandsGeneral = hsubparser $ mempty
  <>  commandGroup "General:"
  <>  cmdVersion
