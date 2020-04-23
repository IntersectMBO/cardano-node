module Cardano.Config.Byron.Parsers
  ( parseDelegationCert
  , parseSigningKey
  ) where

import           Cardano.Prelude hiding (option)

import           Options.Applicative hiding (command)


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
