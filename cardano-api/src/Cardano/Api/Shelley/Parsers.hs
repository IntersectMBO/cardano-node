module Cardano.Api.Shelley.Parsers
  ( parseOperationalCertFilePath
  , parseKesKeyFilePath
  , parseVrfKeyFilePath
  ) where

import           Cardano.Prelude hiding (option)

import           Options.Applicative hiding (command)


parseOperationalCertFilePath :: Parser FilePath
parseOperationalCertFilePath =
  strOption
    ( long "shelley-operational-certificate"
        <> metavar "FILEPATH"
        <> help "Path to the delegation certificate."
    )

--TODO: pass the current KES evolution, not the KES_0
parseKesKeyFilePath :: Parser FilePath
parseKesKeyFilePath =
  strOption
    ( long "shelley-kes-key"
        <> metavar "FILEPATH"
        <> help "Path to the KES signing key."
    )

parseVrfKeyFilePath :: Parser FilePath
parseVrfKeyFilePath =
  strOption
    ( long "shelley-vrf-key"
        <> metavar "FILEPATH"
        <> help "Path to the VRF signing key."
    )
