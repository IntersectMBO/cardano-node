{-# LANGUAGE GADTSyntax                #-}
{-# LANGUAGE ExistentialQuantification #-}

module Cardano.Api.Protocol.Types
  ( SomeNodeClientProtocol(..)
  )
where

import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Node.Run   ( RunNode )
import           Ouroboros.Consensus.Block      ( BlockProtocol )

-- Most uses of the client side rely on network protocol tracers
import           Cardano.TracingOrphanInstances.Network
                                                ( )


data SomeNodeClientProtocol where

     SomeNodeClientProtocol
       ::RunNode blk
       => ProtocolClient blk (BlockProtocol blk)
       -> SomeNodeClientProtocol
