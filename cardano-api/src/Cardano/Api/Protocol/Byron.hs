-- | Node client support for the Byron protocol
--
module Cardano.Api.Protocol.Byron
  ( -- * Client support
    mkNodeClientProtocolByron
  , mkSomeNodeClientProtocolByron
  ) where

import           Cardano.Chain.Slotting (EpochSlots)

import           Ouroboros.Consensus.Cardano
                   (ProtocolClient(ProtocolClientRealPBFT), ProtocolRealPBFT,
                    SecurityParam)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol(..))


mkNodeClientProtocolByron :: EpochSlots
                          -> SecurityParam
                          -> ProtocolClient ByronBlock ProtocolRealPBFT
mkNodeClientProtocolByron epochSlots securityParam =
    ProtocolClientRealPBFT epochSlots securityParam


mkSomeNodeClientProtocolByron :: EpochSlots
                              -> SecurityParam
                              -> SomeNodeClientProtocol
mkSomeNodeClientProtocolByron epochSlots securityParam =
    SomeNodeClientProtocol
      (mkNodeClientProtocolByron epochSlots securityParam)
