{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.CLI.Parsers
  ( opts
  , pTxSubmit
  , pConfigFile
  , pSocketPath
  )
where

import           Cardano.Api (File (..), SocketPath)

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Common.Option
import           Cardano.TxSubmit.CLI.Types (ConfigFile (..), TxSubmitCommand (..),
                   TxSubmitNodeParams (..))
import           Cardano.TxSubmit.Rest.Parsers (pWebserverConfig)

import           Control.Applicative
import           Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative as Opt

opts :: EnvCli -> ParserInfo TxSubmitCommand
opts envCli =
  Opt.info (pTxSubmit envCli <**> Opt.helper) $
    mconcat
      [ Opt.fullDesc
      , Opt.progDesc "Cardano transaction submission web API."
      ]

pTxSubmit :: EnvCli -> Parser TxSubmitCommand
pTxSubmit envCli =
  asum
    [ TxSubmitRun
        <$> ( TxSubmitNodeParams
                <$> pConfigFile
                <*> pConsensusModeParams
                <*> pNetworkId envCli
                <*> pSocketPath'
                <*> pWebserverConfig 8090
                <*> pMetricsPort 8081
            )
    , pVersion
    ]

pVersion :: Parser TxSubmitCommand
pVersion =
  Opt.flag'
    TxSubmitVersion
    ( Opt.long "version"
        <> Opt.help "Show the cardano-submit-api version"
        <> Opt.hidden
    )

pConfigFile :: Parser ConfigFile
pConfigFile =
  ConfigFile
    <$> Opt.strOption
      ( Opt.long "config"
          <> Opt.help "Path to the tx-submit web API configuration file"
          <> Opt.completer (Opt.bashCompleter "file")
          <> Opt.metavar "FILEPATH"
      )

pSocketPath' :: Parser SocketPath
pSocketPath' =
  fmap File $
    Opt.strOption $
      mconcat
        [ Opt.long "socket-path"
        , Opt.help "Path to a cardano-node socket"
        , Opt.completer (Opt.bashCompleter "file")
        , Opt.metavar "FILEPATH"
        ]

pMetricsPort :: Int -> Parser Int
pMetricsPort defaultValue =
  Opt.option
    Opt.auto
    ( Opt.long "metrics-port"
        <> Opt.help
          ( "Port for exposing metrics. If unavailable, the next free port is used. If no port can be allocated, "
              <> "the transaction submission API starts without metrics endpoint."
          )
        <> Opt.metavar "PORT"
        <> Opt.value defaultValue
        <> Opt.showDefault
    )
