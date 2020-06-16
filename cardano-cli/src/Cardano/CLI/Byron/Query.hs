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

import           Cardano.Chain.Slotting (EpochSlots(..))
import           Ouroboros.Consensus.Cardano
                   (protocolClientInfo, SecurityParam(..))
import           Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import           Ouroboros.Consensus.Util.Condense (Condense(..))
import           Ouroboros.Network.Block
import           Ouroboros.Network.NodeToClient (withIOManager)

import           Cardano.Api (Network(..), getLocalTip)
import           Cardano.Api.Protocol.Byron (mkNodeClientProtocolByron)
import           Cardano.CLI.Environment
                   (EnvSocketError, readEnvSocketPath, renderEnvSocketError)

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

runGetLocalNodeTip :: Network -> ExceptT ByronQueryError IO ()
runGetLocalNodeTip network = do
    sockPath <- firstExceptT ByronQueryEnvVarSocketErr $ readEnvSocketPath
    let ptclClientInfo = pClientInfoCodecConfig . protocolClientInfo $
          mkNodeClientProtocolByron
            (EpochSlots 21600)
            (SecurityParam 2160)

    liftIO $ do
      tip <- withIOManager $ \iomgr ->
               getLocalTip iomgr ptclClientInfo network sockPath
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
