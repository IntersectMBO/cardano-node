{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Parameters fixed in the genesis file: 'GenesisParameters'
--
module Cardano.Api.GenesisParameters (

    -- * Protocol parameters fixed in the genesis file
    GenesisParameters(..),
    EpochSize(..),

    -- * Internal conversion functions
    fromShelleyGenesis,

  ) where

import           Prelude

import           Data.Time (NominalDiffTime, UTCTime)

import           Cardano.Slotting.Slot (EpochSize (..))

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Shelley

import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Value


-- ----------------------------------------------------------------------------
-- Genesis parameters
--

data GenesisParameters =
     GenesisParameters {

       -- | The reference time the system started. The time of slot zero.
       -- The time epoch against which all Ouroboros time slots are measured.
       --
       protocolParamSystemStart :: UTCTime,

       -- | The network identifier for this blockchain instance. This
       -- distinguishes the mainnet from testnets, and different testnets from
       -- each other.
       --
       protocolParamNetworkId :: NetworkId,

       -- | The Ouroboros Praos active slot coefficient, aka @f@.
       --
       protocolParamActiveSlotsCoefficient :: Rational,

       -- | The Ouroboros security parameters, aka @k@. This is the maximum
       -- number of blocks the node would ever be prepared to roll back by.
       --
       -- Clients of the node following the chain should be prepared to handle
       -- the node switching forks up to this long.
       --
       protocolParamSecurity :: Int,

       -- | The number of Ouroboros time slots in an Ouroboros epoch.
       --
       protocolParamEpochLength :: EpochSize,

       -- | The time duration of a slot.
       --
       protocolParamSlotLength :: NominalDiffTime,

       -- | For Ouroboros Praos, the length of a KES period as a number of time
       -- slots. The KES keys get evolved once per KES period.
       --
       protocolParamSlotsPerKESPeriod :: Int,

       -- | The maximum number of times a KES key can be evolved before it is
       -- no longer considered valid. This can be less than the maximum number
       -- of times given the KES key size. For example the mainnet KES key size
       -- would allow 64 evolutions, but the max KES evolutions param is 62.
       --
       protocolParamMaxKESEvolutions ::  Int,

       -- | In the Shelley era, prior to decentralised governance, this is the
       -- number of genesis key delegates that need to agree for an update
       -- proposal to be enacted.
       --
       protocolParamUpdateQuorum ::  Int,

       -- | The maximum supply for Lovelace. This determines the initial value
       -- of the reserves.
       --
       protocolParamMaxLovelaceSupply :: Lovelace,

       -- | The initial values of the updateable 'ProtocolParameters'.
       --
       protocolInitialUpdateableProtocolParameters :: ProtocolParameters
     }


-- ----------------------------------------------------------------------------
-- Conversion functions
--

fromShelleyGenesis :: Shelley.ShelleyGenesis era -> GenesisParameters
fromShelleyGenesis
    Shelley.ShelleyGenesis {
      Shelley.sgSystemStart
    , Shelley.sgNetworkMagic
    , Shelley.sgNetworkId
    , Shelley.sgActiveSlotsCoeff
    , Shelley.sgSecurityParam
    , Shelley.sgEpochLength
    , Shelley.sgSlotsPerKESPeriod
    , Shelley.sgMaxKESEvolutions
    , Shelley.sgSlotLength
    , Shelley.sgUpdateQuorum
    , Shelley.sgMaxLovelaceSupply
    , Shelley.sgProtocolParams
    , Shelley.sgGenDelegs    = _  -- unused, might be of interest
    , Shelley.sgInitialFunds = _  -- unused, not retained by the node
    , Shelley.sgStaking      = _  -- unused, not retained by the node
    } =
    GenesisParameters {
      protocolParamSystemStart            = sgSystemStart
    , protocolParamNetworkId              = fromShelleyNetwork sgNetworkId
                                              (NetworkMagic sgNetworkMagic)
    , protocolParamActiveSlotsCoefficient = Ledger.unboundRational
                                              sgActiveSlotsCoeff
    , protocolParamSecurity               = fromIntegral sgSecurityParam
    , protocolParamEpochLength            = sgEpochLength
    , protocolParamSlotLength             = sgSlotLength
    , protocolParamSlotsPerKESPeriod      = fromIntegral sgSlotsPerKESPeriod
    , protocolParamMaxKESEvolutions       = fromIntegral sgMaxKESEvolutions
    , protocolParamUpdateQuorum           = fromIntegral sgUpdateQuorum
    , protocolParamMaxLovelaceSupply      = Lovelace
                                              (fromIntegral sgMaxLovelaceSupply)
    , protocolInitialUpdateableProtocolParameters = fromShelleyPParams
                                                      sgProtocolParams
    }
