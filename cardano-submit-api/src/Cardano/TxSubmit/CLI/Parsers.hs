{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.CLI.Parsers
  ( opts
  , pTxSubmitNodeParams
  , pConfigFile
  , pSocketPath
  ) where

import           Cardano.Api (NetworkId, SocketPath (..))

import           Cardano.CLI.Parsers (pConsensusModeParams, pNetworkId)

import           Cardano.TxSubmit.CLI.Types (ConfigFile (..), TxSubmitNodeParams (..))
import           Cardano.TxSubmit.Rest.Parsers (pWebserverConfig)

import           Control.Applicative ((<**>))
import           Options.Applicative (Parser, ParserInfo)

import qualified Options.Applicative as Opt

opts :: Maybe NetworkId -> ParserInfo TxSubmitNodeParams
opts mNetworkId =
  Opt.info (pTxSubmitNodeParams mNetworkId <**> Opt.helper) $ mconcat
    [ Opt.fullDesc
    , Opt.progDesc "Cardano transaction submission web API."
    ]

pTxSubmitNodeParams :: Maybe NetworkId -> Parser TxSubmitNodeParams
pTxSubmitNodeParams mNetworkId = TxSubmitNodeParams
  <$> pConfigFile
  <*> pConsensusModeParams
  <*> pNetworkId mNetworkId
  <*> pSocketPath
  <*> pWebserverConfig 8090
  <*> pMetricsPort 8081

pConfigFile :: Parser ConfigFile
pConfigFile = ConfigFile <$> Opt.strOption
  (   Opt.long "config"
  <>  Opt.help "Path to the tx-submit web API configuration file"
  <>  Opt.completer (Opt.bashCompleter "file")
  <>  Opt.metavar "FILEPATH"
  )

pSocketPath :: Parser SocketPath
pSocketPath = SocketPath <$> Opt.strOption
  (   Opt.long "socket-path"
  <>  Opt.help "Path to a cardano-node socket"
  <>  Opt.completer (Opt.bashCompleter "file")
  <>  Opt.metavar "FILEPATH"
  )

pMetricsPort :: Int -> Parser Int
pMetricsPort defaultValue = Opt.option Opt.auto
  (   Opt.long "metrics-port"
  <>  Opt.help "Metrics port"
  <>  Opt.metavar "PORT"
  <>  Opt.value defaultValue
  )
