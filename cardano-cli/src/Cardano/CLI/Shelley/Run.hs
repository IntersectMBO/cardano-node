{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Shelley.Run
  ( runShelleyClientCommand
  ) where

import           Cardano.Prelude hiding (option, trace)

import           Control.Monad.Trans.Except (ExceptT)

import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), ShelleyCommand(..))
import           Cardano.CLI.Shelley.Run.Genesis (runGenesisCreate)


runShelleyClientCommand :: ShelleyCommand -> ExceptT CliError IO ()
runShelleyClientCommand cc =
  case cc of
    ShelleyCreateGenesis genSyr mstart amount -> runGenesisCreate genSyr mstart amount
    ShelleyKeyGenerate fpath title -> runShelleyKeyGenerate fpath title
    _ -> liftIO . putStrLn $ "runShelleyClientCommand: " ++ show cc


runShelleyKeyGenerate :: OutputFile -> ByteString -> ExceptT CliError IO ()
runShelleyKeyGenerate fpath title =
  liftIO . putStrLn $ "runShelleyKeyGenerate (" ++ show fpath ++ ") " ++ show title
