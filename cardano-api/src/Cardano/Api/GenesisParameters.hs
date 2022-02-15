{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Parameters fixed in the genesis file: 'GenesisParameters'
--
module Cardano.Api.GenesisParameters (

    -- * Protocol paramaters fixed in the genesis file
    GenesisParameters(..),
    EpochSize(..),

    -- * Internal conversion functions
    fromShelleyGenesis,
    toShelleyGenesis,

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

       -- | The Ouroboros security paramaters, aka @k@. This is the maximum
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

-- | GenesisParameters is obtained via the 'QueryGenesisParameters' query and is
-- constructed via 'getCompactGenesis' which lives in ouroboros-network. The following
-- fields are ignored for the following reasons:
--   The 'sgInitialFunds' field is erased. It is only used to set up the initial
--   UTxO in tests and testnets.
--
--   The 'sgStaking' field is erased. It is only used to register initial stake
--   pools in tests and benchmarks.
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

toShelleyGenesis :: GenesisParameters -> Either String (Shelley.ShelleyGenesis era)
toShelleyGenesis gParams = do
   case Ledger.boundRational $ protocolParamActiveSlotsCoefficient gParams of
     Nothing -> Left $ "Active slot coefficient is not within the bounds: " <> show (protocolParamActiveSlotsCoefficient gParams)
     Just aSlotCoeff ->
      Right $ Shelley.ShelleyGenesis {
                 Shelley.sgSystemStart = protocolParamSystemStart gParams
               , Shelley.sgNetworkMagic = unNetworkMagic . toNetworkMagic $ protocolParamNetworkId gParams
               , Shelley.sgNetworkId = toShelleyNetwork $ protocolParamNetworkId gParams
               , Shelley.sgActiveSlotsCoeff = aSlotCoeff
               , Shelley.sgSecurityParam = fromIntegral $ protocolParamSecurity gParams
               , Shelley.sgEpochLength = protocolParamEpochLength gParams
               , Shelley.sgSlotsPerKESPeriod = fromIntegral $ protocolParamSlotsPerKESPeriod gParams
               , Shelley.sgMaxKESEvolutions = fromIntegral $ protocolParamMaxKESEvolutions gParams
               , Shelley.sgSlotLength = protocolParamSlotLength gParams
               , Shelley.sgUpdateQuorum = fromIntegral $ protocolParamUpdateQuorum gParams
               , Shelley.sgMaxLovelaceSupply = let Lovelace i = protocolParamMaxLovelaceSupply gParams
                                               in fromIntegral i
               , Shelley.sgProtocolParams = toShelleyPParams . protocolInitialUpdateableProtocolParameters $ gParams
               , Shelley.sgGenDelegs    = mempty
               , Shelley.sgInitialFunds = mempty
               , Shelley.sgStaking      = Shelley.ShelleyGenesisStaking mempty mempty
                }
