{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Byron.Query
  ( ByronQueryError(..)
  , renderByronQueryError
  , runGetLocalNodeTip
  ) where

import           Prelude (unlines)
import           Cardano.Prelude hiding (unlines)

import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as T

import           Cardano.Api.Typed
import           Cardano.Chain.Slotting (EpochSlots(..))
import           Ouroboros.Consensus.Cardano (SecurityParam(..))
import           Ouroboros.Consensus.Util.Condense (Condense(..))
import           Ouroboros.Network.Block

import           Cardano.Config.Types (SocketPath (..))
import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.CLI.Environment
                   (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
-- TODO @intricate: This forces us to import "cardano-node". Fix this.
import           Cardano.Tracing.OrphanInstances.HardFork ()


data ByronQueryError
  = ByronQueryEnvVarSocketErr !EnvSocketError
  deriving Show

renderByronQueryError :: ByronQueryError -> Text
renderByronQueryError err =
  case err of
    ByronQueryEnvVarSocketErr sockEnvErr -> renderEnvSocketError sockEnvErr

--------------------------------------------------------------------------------
-- Query local node's chain tip
--------------------------------------------------------------------------------

runGetLocalNodeTip :: NetworkId -> ExceptT ByronQueryError IO ()
runGetLocalNodeTip networkId = do
    SocketPath sockPath <- firstExceptT ByronQueryEnvVarSocketErr $
                           readEnvSocketPath
    let connctInfo =
          LocalNodeConnectInfo {
            localNodeSocketPath    = sockPath,
            localNodeNetworkId     = networkId,
            localNodeConsensusMode = ByronMode
                                       (EpochSlots 21600)
                                       (SecurityParam 2160)
          }
    liftIO $ do
      tip <- getLocalTip connctInfo
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
