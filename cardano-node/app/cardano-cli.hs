{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import           Cardano.Binary (Annotated (..))
import           Cardano.Chain.Common
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochNumber(..))
import           Cardano.CLI.Run (ClientCommand(..), SystemVersion(..), decideKeyMaterialOps, runCommand)
import           Cardano.Common.CommonCLI (command')
import           Cardano.Crypto (AProtocolMagic(..), ProtocolMagic, ProtocolMagicId(..), RequiresNetworkMagic(..))

import           Options.Applicative
import           Control.Exception.Safe (catchIO)
import           System.Exit (ExitCode(..), exitWith)

main :: IO ()
main = do
  CLI{mainCommand, protocol} <- execParser opts
  ops <- decideCLIOps protocol
  catchIO (runCommand ops mainCommand) $
    \err-> do
      hPutStrLn stderr ("Error:\n" <> show err :: String)
      exitWith $ ExitFailure 1

opts :: ParserInfo CLI
opts = info (parseClient <**> helper)
  ( fullDesc
    <> progDesc "Cardano genesis tool."
    <> header "Cardano genesis tool."
  )

data CLI = CLI
  { protocol    :: Protocol
  , mainCommand :: ClientCommand
  }

parseClient :: Parser CLI
parseClient = CLI
    <$> parseProtocolAsCommand
    <*> parseClientCommand
