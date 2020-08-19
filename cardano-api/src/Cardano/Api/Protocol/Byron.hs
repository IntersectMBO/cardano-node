-- | Node client support for the Byron protocol
--
module Cardano.Api.Protocol.Byron
  ( -- * Client support
    mkNodeClientProtocolByron
  , mkSomeNodeClientProtocolByron
  ) where

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol (..))
import           Cardano.Chain.Slotting (EpochSlots)
import           Ouroboros.Consensus.Cardano (ProtocolByron, ProtocolClient (ProtocolClientByron),
                     SecurityParam)
import           Ouroboros.Consensus.Cardano.ByronHFC

mkNodeClientProtocolByron :: EpochSlots
                          -> SecurityParam
                          -> ProtocolClient ByronBlockHFC ProtocolByron
mkNodeClientProtocolByron = ProtocolClientByron

mkSomeNodeClientProtocolByron :: EpochSlots
                              -> SecurityParam
                              -> SomeNodeClientProtocol
mkSomeNodeClientProtocolByron epochSlots securityParam =
    SomeNodeClientProtocol
      (mkNodeClientProtocolByron epochSlots securityParam)
