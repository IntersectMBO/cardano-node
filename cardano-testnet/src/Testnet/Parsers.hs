module Testnet.Parsers
  ( commands
  ) where

import           Options.Applicative
import           Parsers.Babbage
import           Parsers.Byron
import           Parsers.Cardano
import           Parsers.Shelley
import           Parsers.Version

{- HLINT ignore "Monoid law, left identity" -}

commands :: Parser (IO ())
commands = commandsTestnet <|> commandsGeneral

commandsTestnet :: Parser (IO ())
commandsTestnet = hsubparser $ mempty
  <>  commandGroup "Testnets:"
  <>  cmdBabbage
  <>  cmdByron
  <>  cmdCardano
  <>  cmdShelley

commandsGeneral :: Parser (IO ())
commandsGeneral = hsubparser $ mempty
  <>  commandGroup "General:"
  <>  cmdVersion
