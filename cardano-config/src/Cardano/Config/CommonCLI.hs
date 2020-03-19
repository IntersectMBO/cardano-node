{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Config.CommonCLI
  ( parseDelegationCert
  , parseGenesisPath
  , parseSigningKey
  ) where

import           Cardano.Prelude hiding (option)

import           Options.Applicative hiding (command)


{-------------------------------------------------------------------------------
  Common CLI
-------------------------------------------------------------------------------}

parseGenesisPath :: Parser FilePath
parseGenesisPath =
  strOption
    ( long "genesis-file"
        <> metavar "FILEPATH"
        <> help "The filepath to the genesis file."
    )

parseDelegationCert :: Parser FilePath
parseDelegationCert =
  strOption
    ( long "delegation-certificate"
        <> metavar "FILEPATH"
        <> help "Path to the delegation certificate."
    )

parseSigningKey :: Parser FilePath
parseSigningKey =
  strOption
    ( long "signing-key"
        <> metavar "FILEPATH"
        <> help "Path to the signing key."
    )
