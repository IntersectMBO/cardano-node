{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Rest.Parsers
  ( pWebserverConfig
  ) where

import Cardano.TxSubmit.Rest.Types
    ( WebserverConfig (..) )
import Data.String
    ( fromString )
import Network.Wai.Handler.Warp
    ( HostPreference, Port )
import Options.Applicative
    ( Parser
    , auto
    , help
    , long
    , metavar
    , option
    , showDefault
    , strOption
    , switch
    , value
    )

pWebserverConfig :: Port -> Parser WebserverConfig
pWebserverConfig defaultPort = do
  wcHost <- pHostPreferenceOption
  isRandom <- pRandomPortOption
  wcPort <- pPortOption defaultPort
  pure
    WebserverConfig
      { wcHost
      , wcPort =
          if isRandom
            then 0
            else wcPort
      }

pHostPreferenceOption :: Parser HostPreference
pHostPreferenceOption =
  fromString <$>
  strOption
    (long "listen-address" <>
     metavar "HOST" <>
     help
       ("Specification of which host to the bind API server to. " <>
        "Can be an IPv[46] address, hostname, or '*'.") <>
     value "127.0.0.1" <> showDefault)

pPortOption :: Port -> Parser Port
pPortOption defaultPort =
  option auto $
  long "port" <>
  metavar "INT" <>
  help "Port used for the API server." <> value defaultPort <> showDefault

pRandomPortOption :: Parser Bool
pRandomPortOption =
  switch $
  long "random-port" <>
  help "Serve API on any available port (overrides --port)"
