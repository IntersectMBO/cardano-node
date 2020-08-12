{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Byron.Query
  ( ByronQueryError(..)
  , renderByronQueryError
  , runGetLocalNodeTip
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as Text

import           Cardano.Api.Typed
import           Cardano.Chain.Slotting (EpochSlots (..))
import           Ouroboros.Consensus.Block (ConvertRawHash (..))
import           Ouroboros.Consensus.Cardano (SecurityParam (..))
import           Ouroboros.Network.Block

import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Types (SocketPath (..))
import           Cardano.Tracing.Render (renderHeaderHash, renderSlotNo) -- TODO: This forces us to import "cardano-node". Fix this.


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
    getTipOutput :: forall blk. ConvertRawHash blk => Tip blk -> Text
    getTipOutput (TipGenesis) = "Current tip: genesis (origin)"
    getTipOutput (Tip slotNo headerHash (BlockNo blkNo)) =
      Text.unlines
        [ "\n"
        , "Current tip: "
        , "Block hash: " <> renderHeaderHash (Proxy @blk) headerHash
        , "Slot: " <> renderSlotNo slotNo
        , "Block number: " <> show blkNo
        ]
