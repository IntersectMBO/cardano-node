{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Byron.Query
  ( runGetLocalNodeTip
  ) where

import           Prelude (show, unlines)
import           Cardano.Prelude hiding (show, unlines)

import qualified Data.Text as T
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Ouroboros.Consensus.Block (BlockProtocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Config (configCodec)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo(..))
import           Ouroboros.Consensus.Node.Run
                   (RunNode(..))
import           Ouroboros.Consensus.Util.Condense (Condense(..))
import           Ouroboros.Network.Block
import           Ouroboros.Network.NodeToClient (withIOManager)

import           Cardano.Common.LocalSocket (chooseSocketPath)
import           Cardano.Config.Protocol
                   (SomeConsensusProtocol(..), mkConsensusProtocol)
import           Cardano.Config.Types

import           Cardano.Api (Network(..), getLocalTip)
import           Cardano.CLI.Errors


--------------------------------------------------------------------------------
-- Query local node's chain tip
--------------------------------------------------------------------------------

runGetLocalNodeTip :: ConfigYamlFilePath -> Maybe SocketPath -> ExceptT e IO ()
runGetLocalNodeTip configFp mSockPath = liftIO $ do
    nc <- parseNodeConfigurationFP configFp
    sockPath <- return $ chooseSocketPath (ncSocketPath nc) mSockPath
    frmPtclRes <- runExceptT $ firstExceptT ProtocolError $
                    mkConsensusProtocol nc Nothing

    --TODO: simplify using the Consensus.ProtocolClient
    SomeConsensusProtocol (p :: Consensus.Protocol blk (BlockProtocol blk))
                     <- case frmPtclRes of
                          Right p -> pure p
                          Left err -> do putTextLn . toS $ show err
                                         exitFailure

    let ProtocolInfo{pInfoConfig = ptclcfg} = Consensus.protocolInfo p
        cfg = configCodec ptclcfg
        --FIXME: this works, but we should get the magic properly:
        nm  = Testnet (nodeNetworkMagic (Proxy :: Proxy blk) ptclcfg)

    tip <- withIOManager $ \iomgr -> getLocalTip iomgr cfg nm sockPath
    putTextLn (getTipOutput tip)
  where
    getTipOutput :: forall blk. Condense (HeaderHash blk) => Tip blk -> Text
    getTipOutput (TipGenesis) = "Current tip: genesis (origin)"
    getTipOutput (Tip slotNo headerHash blkNo) =
      T.pack $ unlines [ "\n"
                        , "Current tip: "
                        , "Block hash: " <> condense headerHash
                        , "Slot: " <> condense slotNo
                        , "Block number: " <> condense blkNo
                        ]

