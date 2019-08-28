{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.Run
  ( CLI(..)
  , runClient
  ) where

import           Cardano.Prelude

import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text, pack)

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (Trace, appendName)
import           Cardano.Common.Protocol(Protocol, SomeProtocol(..), fromProtocol)
import           Cardano.Config.CommonCLI(CommonCLI)

import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Cardano.Config.Types (CardanoConfiguration (..))
import           Cardano.Wallet.Client

runClient :: CLI -> Trace IO Text -> CardanoConfiguration -> IO ()
runClient CLI{..} tracer cc = do
    let CoreNodeId nid = cliCoreNodeId
    let tracer' = contramap pack . toLogObject $
          appendName ("Wallet " <> pack (show nid)) tracer

    SomeProtocol p <- fromProtocol cc cliProtocol
    runWalletClient p cliCoreNodeId cliNumCoreNodes tracer'

data CLI = CLI {
    cliCoreNodeId   :: CoreNodeId,
    cliNumCoreNodes :: NumCoreNodes,
    cliProtocol     :: Protocol,
    cliCommon       :: CommonCLI
  }
