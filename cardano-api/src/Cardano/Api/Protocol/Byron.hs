-- | Node client support for the Byron protocol
--
module Cardano.Api.Protocol.Byron
  ( -- * Client support
    mkNodeClientProtocolByron
  , mkSomeNodeClientProtocolByron
  ) where

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol (..))
import           Cardano.Chain.Slotting (EpochSlots)
import           Ouroboros.Consensus.Cardano (ProtocolByron, ProtocolClient (ProtocolClientByron))
import           Ouroboros.Consensus.Cardano.ByronHFC

mkNodeClientProtocolByron :: EpochSlots
                          -> ProtocolClient ByronBlockHFC ProtocolByron
mkNodeClientProtocolByron = ProtocolClientByron

mkSomeNodeClientProtocolByron :: EpochSlots
                              -> SomeNodeClientProtocol
mkSomeNodeClientProtocolByron epochSlots =
    SomeNodeClientProtocol
      (mkNodeClientProtocolByron epochSlots)
