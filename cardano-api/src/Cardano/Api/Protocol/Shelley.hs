-- | Node client support for the Shelley protocol
--
module Cardano.Api.Protocol.Shelley
  ( -- * Client support
    mkNodeClientProtocolShelley
  , mkSomeNodeClientProtocolShelley
  ) where


import           Ouroboros.Consensus.Cardano
                   (ProtocolClient(ProtocolClientShelley), ProtocolShelley)
import           Ouroboros.Consensus.Cardano.ShelleyHFC

import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol(..))


mkNodeClientProtocolShelley :: ProtocolClient
                                 (ShelleyBlockHFC TPraosStandardCrypto)
                                 ProtocolShelley
mkNodeClientProtocolShelley = ProtocolClientShelley


mkSomeNodeClientProtocolShelley :: SomeNodeClientProtocol
mkSomeNodeClientProtocolShelley =
    SomeNodeClientProtocol mkNodeClientProtocolShelley
