{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.Run
  ( runClient
  ) where

import           Cardano.Prelude

import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text, pack)

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (Trace, appendName)
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Cardano.Node.Configuration.Types (CardanoConfiguration (..))

import           Cardano.Wallet.CLI
import           Cardano.Wallet.Client

runClient :: CLI -> Trace IO Text -> CardanoConfiguration -> IO ()
runClient CLI{..} tracer cc = do
    let CoreNodeId nid = cliCoreNodeId
    let tracer' = contramap pack . toLogObject $
          appendName ("Wallet " <> pack (show nid)) tracer

    SomeProtocol p <- fromProtocol cc cliProtocol
    runWalletClient p cliCoreNodeId cliNumCoreNodes tracer'
