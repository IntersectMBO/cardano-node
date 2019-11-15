{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.Run
  ( WalletCLI(..)
  , runClient
  ) where

import           Cardano.Prelude

import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text, pack)

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (Trace, appendName)
import           Cardano.Config.Protocol(SomeProtocol(..), fromProtocol)

import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Cardano.Config.CommonCLI
import           Cardano.Config.Types (ConfigYamlFilePath(..), MiscellaneousFilepaths(..),
                                       NodeCLI(..), SocketFile(..), parseNodeConfiguration)
import           Cardano.Wallet.Client

runClient :: WalletCLI -> Trace IO Text -> IO ()
runClient WalletCLI{..} tracer = do
    let CoreNodeId nid = cliCoreNodeId
    let tracer' = contramap pack . toLogObject $
          appendName ("Wallet " <> pack (show nid)) tracer
    nc <- parseNodeConfiguration . unConfigPath $ configFp cliNodeCLI
    -- TODO
    SomeProtocol p <- fromProtocol nc cliNodeCLI
    let socketDir = unSocket . socketFile $ mscFp cliNodeCLI
    runWalletClient p socketDir cliCoreNodeId tracer'

data WalletCLI = WalletCLI {
    cliCoreNodeId   :: CoreNodeId,
    cliCommon       :: CommonCLI,
    cliCommonAdv    :: CommonCLIAdvanced,
    cliNodeCLI      :: NodeCLI
  }
