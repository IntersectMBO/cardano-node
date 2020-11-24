{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}

module Cardano.Api.Protocol.Types
  ( SomeNodeClientProtocol(..)
  ) where

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Node.Run (RunNode)

data SomeNodeClientProtocol where

     SomeNodeClientProtocol
       :: RunNode blk
       => ProtocolClient blk (BlockProtocol blk)
       -> SomeNodeClientProtocol
