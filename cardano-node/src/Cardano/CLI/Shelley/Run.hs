{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Shelley.Run
  ( runShelleyCommand
  ) where

import           Cardano.Prelude hiding (option, trace)

import           Control.Monad.Trans.Except (ExceptT)

import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), ShelleyCommand(..))


runShelleyCommand :: ShelleyCommand -> ExceptT CliError IO ()
runShelleyCommand cc =
  case cc of
    ShelleyKeyGenerate fpath title -> runShelleyKeyGenerate fpath title
    _ -> liftIO . putStrLn $ "runShelleyCommand: " ++ show cc


runShelleyKeyGenerate :: OutputFile -> ByteString -> ExceptT CliError IO ()
runShelleyKeyGenerate fpath title =
  liftIO . putStrLn $ "runShelleyKeyGenerate (" ++ show fpath ++ ") " ++ show title
