
module Cardano.TraceDispatcher.Network.Combinators
  (
    severityTChainSync
  , namesTChainSync

  ) where


import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Network.Block (Serialised, Tip, Point)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

severityTChainSync :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Serialised blk) (Point blk) (Tip blk))) -> SeverityS
severityTChainSync _  = undefined


namesTChainSync :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Serialised blk) (Point blk) (Tip blk))) -> [Text]
namesTChainSync _ = undefined
