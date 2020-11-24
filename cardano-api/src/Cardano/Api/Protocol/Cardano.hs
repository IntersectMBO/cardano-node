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
                     ProtocolClient (ProtocolClientCardano))
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

mkNodeClientProtocolCardano :: EpochSlots
                            -> ProtocolClient (CardanoBlock StandardCrypto)
                                              ProtocolCardano
mkNodeClientProtocolCardano = ProtocolClientCardano

mkSomeNodeClientProtocolCardano :: EpochSlots
                                -> SomeNodeClientProtocol
mkSomeNodeClientProtocolCardano epochSlots =
    SomeNodeClientProtocol
      (mkNodeClientProtocolCardano epochSlots)
