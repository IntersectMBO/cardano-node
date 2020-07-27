-- | Node client support for the Shelley protocol
module Cardano.Api.Protocol.Shelley
  ( -- * Client support
    mkNodeClientProtocolShelley,
    mkSomeNodeClientProtocolShelley,
  )
where

import Cardano.Api.Protocol.Types (SomeNodeClientProtocol (..))
import Ouroboros.Consensus.Cardano
  ( ProtocolClient (ProtocolClientShelley),
    ProtocolShelley,
  )
import Ouroboros.Consensus.Cardano.ShelleyHFC
import Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

mkNodeClientProtocolShelley ::
  ProtocolClient
    (ShelleyBlockHFC TPraosStandardCrypto)
    ProtocolShelley
mkNodeClientProtocolShelley = ProtocolClientShelley

mkSomeNodeClientProtocolShelley :: SomeNodeClientProtocol
mkSomeNodeClientProtocolShelley =
  SomeNodeClientProtocol mkNodeClientProtocolShelley
