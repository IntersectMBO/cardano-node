-- | Node client support for the Shelley protocol
--
module Cardano.Api.Protocol.Shelley
  ( -- * Client support
    mkNodeClientProtocolShelley
  , mkSomeNodeClientProtocolShelley
  ) where


import           Ouroboros.Consensus.Cardano
                   (ProtocolClient(ProtocolClientRealTPraos), ProtocolRealTPraos)

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol(..))


mkNodeClientProtocolShelley :: ProtocolClient
                                 (ShelleyBlock TPraosStandardCrypto)
                                 ProtocolRealTPraos
mkNodeClientProtocolShelley = ProtocolClientRealTPraos


mkSomeNodeClientProtocolShelley :: SomeNodeClientProtocol
mkSomeNodeClientProtocolShelley =
    SomeNodeClientProtocol mkNodeClientProtocolShelley
