module Testnet.Status.Parsers
  ( cmdCheckStatus
  ) where

import           Cardano.CLI.EraBased.Common.Option (command')
import           Cardano.Prelude (Nat)

import           Prelude

import           Control.Monad (when)
import           Data.Default.Class (def)
import           Options.Applicative (CommandFields, Mod, Parser)
import qualified Options.Applicative as OA

import           Testnet.Status.Types (CheckStatusOptions (..), OutputFormat (..))

cmdCheckStatus :: Mod CommandFields CheckStatusOptions
cmdCheckStatus = command' "status" "Check the status of a running testnet" optsCheckStatus

optsCheckStatus :: Parser CheckStatusOptions
optsCheckStatus = CheckStatusOptions
  <$> pOutputDir
  <*> pStatusCheckTimeout
  <*> pOutputFormat

pOutputDir :: Parser (Maybe FilePath)
pOutputDir = OA.optional (OA.strOption
      (  OA.long "testnet-dir"
      <> OA.metavar "FILEPATH"
      <> OA.help "Path to the environment folder of the testnet whose status to check, by default \"./testnet\"."
      ))


pStatusCheckTimeout :: Parser Nat
pStatusCheckTimeout = OA.option timeoutReader
  (   OA.long "timeout"
  <>  OA.metavar "SECONDS"
  <>  OA.help "How many seconds to wait for each node to answer (1 - 3600)."
  <>  OA.value 5
  <>  OA.showDefault
  )
  where
    timeoutReader = OA.auto >>= \n -> do
      when (n < 1 || n > 3600) $
        OA.readerError $ "Timeout out of range (1 - 3600): " <> show n
      pure n

pOutputFormat :: Parser OutputFormat
pOutputFormat =
  OA.flag def OutputFormatJson (OA.long "json" <> OA.help "Output status as JSON instead of a table.")
