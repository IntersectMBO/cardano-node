-- | Node client support for the Cardano protocol
--
module Cardano.Api.Protocol.Cardano
  ( -- * Client support
    mkNodeClientProtocolCardano
  , mkSomeNodeClientProtocolCardano
  ) where

import           Cardano.Chain.Slotting (EpochSlots)

import           Ouroboros.Consensus.Cardano
                   (ProtocolClient(ProtocolClientCardano), ProtocolCardano,
                    SecurityParam)
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol(..))


mkNodeClientProtocolCardano :: EpochSlots
                            -> SecurityParam
                            -> ProtocolClient (CardanoBlock TPraosStandardCrypto)
                                              ProtocolCardano
mkNodeClientProtocolCardano epochSlots securityParam =
    ProtocolClientCardano epochSlots securityParam


mkSomeNodeClientProtocolCardano :: EpochSlots
                                -> SecurityParam
                                -> SomeNodeClientProtocol
mkSomeNodeClientProtocolCardano epochSlots securityParam =
    SomeNodeClientProtocol
      (mkNodeClientProtocolCardano epochSlots securityParam)
