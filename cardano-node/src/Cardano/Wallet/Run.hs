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
import           Cardano.Config.Protocol(Protocol, SomeProtocol(..), fromProtocol)

import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract (NumCoreNodes (..))
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
    SomeProtocol p <- fromProtocol nc cliNodeCLI cliProtocol
    let socketDir = unSocket . socketFile $ mscFp cliNodeCLI
    runWalletClient p socketDir cliCoreNodeId cliNumCoreNodes tracer'

data WalletCLI = WalletCLI {
    cliCoreNodeId   :: CoreNodeId,
    cliNumCoreNodes :: NumCoreNodes,
    cliProtocol     :: Protocol,
    cliCommon       :: CommonCLI,
    cliCommonAdv    :: CommonCLIAdvanced,
    cliNodeCLI      :: NodeCLI
  }
