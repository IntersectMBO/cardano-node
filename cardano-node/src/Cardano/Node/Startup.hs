{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Node.Startup
  ( module Cardano.Node.Startup
  , module Cardano.Node.Startup.Types
  , module Cardano.Logging.Types.NodeInfo
  , module Cardano.Logging.Types.NodeStartupInfo
  ) where

import qualified Cardano.Api as Api

import           Cardano.Git.Rev (gitRev)
import           Cardano.Ledger.Shelley.Genesis (sgSystemStart)
import           Cardano.Logging
import           Cardano.Logging.Types.NodeInfo (NodeInfo (..))
import           Cardano.Logging.Types.NodeStartupInfo (NodeStartupInfo (..))
import           Cardano.Node.Configuration.POM (NodeConfiguration (..), ncProtocol)
import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import           Cardano.Node.Startup.Types
import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as WCT
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork (shelleyLedgerConfig)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
import           Ouroboros.Consensus.Ledger.Query (getSystemStart)
import           Ouroboros.Consensus.Node (pInfoConfig)
import           Ouroboros.Consensus.Shelley.Ledger.Ledger (shelleyLedgerGenesis)

import           Prelude

import           Data.Monoid (Last (..))
import           Data.Text (pack)
import           Data.Time.Clock (UTCTime)
import           Data.Version (showVersion)
import           Network.HostName (getHostName)

import           Paths_cardano_node (version)

-- | Prepare basic info about the node. This info will be sent to 'cardano-tracer'.
prepareNodeInfo
  :: NodeConfiguration
  -> SomeConsensusProtocol
  -> TraceConfig
  -> UTCTime
  -> IO NodeInfo
prepareNodeInfo nc (SomeConsensusProtocol whichP pForInfo) tc nodeStartTime = do
  nodeName <- prepareNodeName
  return $ NodeInfo
    { niName            = nodeName
    , niProtocol        = pack . show . ncProtocol $ nc
    , niVersion         = pack . showVersion $ version
    , niCommit          = $(gitRev)
    , niStartTime       = nodeStartTime
    , niSystemStartTime = systemStartTime
    }
 where
  cfg = pInfoConfig $ fst $ Api.protocolInfo @IO pForInfo

  systemStartTime :: UTCTime
  systemStartTime =
    case whichP of
      Api.ByronBlockType ->
        getSystemStartByron
      Api.ShelleyBlockType ->
        let DegenLedgerConfig cfgShelley = configLedger cfg
        in getSystemStartShelley cfgShelley
      Api.CardanoBlockType ->
        let CardanoLedgerConfig _ cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage cfgConway cfgDijkstra = configLedger cfg
        in minimum [ getSystemStartByron
                   , getSystemStartShelley cfgShelley
                   , getSystemStartShelley cfgAllegra
                   , getSystemStartShelley cfgMary
                   , getSystemStartShelley cfgAlonzo
                   , getSystemStartShelley cfgBabbage
                   , getSystemStartShelley cfgConway
                   , getSystemStartShelley cfgDijkstra
                   ]

  getSystemStartByron = WCT.getSystemStart . getSystemStart . configBlock $ cfg
  getSystemStartShelley = sgSystemStart . shelleyLedgerGenesis . shelleyLedgerConfig

  prepareNodeName =
    case tcNodeName tc of
      Just aName -> return aName
      Nothing -> do
        -- The user didn't specify node's name in the configuration.
        -- In this case we should form node's name as "host_port",
        -- where 'host' is the machine's host name and 'port' is taken
        -- from the '--port' CLI-parameter.

        let suffix :: String
            suffix
              | SocketConfig{ncNodePortNumber = Last (Just port)} <- ncSocketConfig nc
              = "_" <> show port
              | otherwise
              = ""

        hostName <- getHostName
        return (pack (hostName <> suffix))
