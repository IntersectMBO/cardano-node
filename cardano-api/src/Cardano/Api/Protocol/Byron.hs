-- | Node client support for the Byron protocol
--
module Cardano.Api.Protocol.Byron
  ( -- * Client support
    mkNodeClientProtocolByron
  , mkSomeNodeClientProtocolByron
  ) where

import           Cardano.Chain.Slotting (EpochSlots)

import           Ouroboros.Consensus.Cardano
                   (ProtocolClient(ProtocolClientByron), ProtocolByron,
                    SecurityParam)
import           Ouroboros.Consensus.Cardano.ByronHFC

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol(..))


mkNodeClientProtocolByron :: EpochSlots
                          -> SecurityParam
                          -> ProtocolClient ByronBlockHFC ProtocolByron
mkNodeClientProtocolByron epochSlots securityParam =
    ProtocolClientByron epochSlots securityParam


mkSomeNodeClientProtocolByron :: EpochSlots
                              -> SecurityParam
                              -> SomeNodeClientProtocol
mkSomeNodeClientProtocolByron epochSlots securityParam =
    SomeNodeClientProtocol
      (mkNodeClientProtocolByron epochSlots securityParam)
