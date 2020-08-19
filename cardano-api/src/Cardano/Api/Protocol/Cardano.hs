-- | Node client support for the Cardano protocol
--
module Cardano.Api.Protocol.Cardano
  ( -- * Client support
    mkNodeClientProtocolCardano
  , mkSomeNodeClientProtocolCardano
  ) where

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol (..))
import           Cardano.Chain.Slotting (EpochSlots)
import           Ouroboros.Consensus.Cardano (ProtocolCardano,
                     ProtocolClient (ProtocolClientCardano), SecurityParam)
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

mkNodeClientProtocolCardano :: EpochSlots
                            -> SecurityParam
                            -> ProtocolClient (CardanoBlock TPraosStandardCrypto)
                                              ProtocolCardano
mkNodeClientProtocolCardano = ProtocolClientCardano

mkSomeNodeClientProtocolCardano :: EpochSlots
                                -> SecurityParam
                                -> SomeNodeClientProtocol
mkSomeNodeClientProtocolCardano epochSlots securityParam =
    SomeNodeClientProtocol
      (mkNodeClientProtocolCardano epochSlots securityParam)
