{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Certificates embedded in transactions
--
module Cardano.Api.Certificate (
    Certificate(..),

    -- * Registering stake address and delegating
    makeStakeAddressRegistrationCertificate,
    makeStakeAddressDeregistrationCertificate,
    makeStakeAddressDelegationCertificate,
    PoolId,

    -- * Registering stake pools
    makeStakePoolRegistrationCertificate,
    makeStakePoolRetirementCertificate,
    StakePoolParameters(..),
    StakePoolRelay(..),
    StakePoolMetadataReference(..),

    -- * Special certificates
    makeMIRCertificate,
    makeGenesisKeyDelegationCertificate,

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import           Data.Maybe
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence.Strict as Seq

import           Data.IP (IPv4, IPv6)
import           Network.Socket (PortNumber)

import           Cardano.Slotting.Slot (EpochNo (..))
import qualified Cardano.Crypto.Hash.Class as Crypto

import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Shelley.Spec.Ledger.BaseTypes (maybeToStrictMaybe)
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import           Shelley.Spec.Ledger.TxBody (MIRPot (..))

import           Cardano.Api.Address
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysPraos
import           Cardano.Api.KeysShelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.Value


-- ----------------------------------------------------------------------------
-- Certificates embedded in transactions
--

newtype Certificate = Certificate (Shelley.DCert StandardShelley)
  deriving stock (Eq, Show)
  deriving newtype (ToCBOR, FromCBOR)
  deriving anyclass SerialiseAsCBOR

instance HasTypeProxy Certificate where
    data AsType Certificate = AsCertificate
    proxyToAsType _ = AsCertificate

instance HasTextEnvelope Certificate where
    textEnvelopeType _ = "CertificateShelley"
    textEnvelopeDefaultDescr (Certificate cert) = case cert of
      Shelley.DCertDeleg Shelley.RegKey {}    -> "Stake address registration"
      Shelley.DCertDeleg Shelley.DeRegKey {}  -> "Stake address de-registration"
      Shelley.DCertDeleg Shelley.Delegate {}  -> "Stake address delegation"
      Shelley.DCertPool Shelley.RegPool {}    -> "Pool registration"
      Shelley.DCertPool Shelley.RetirePool {} -> "Pool retirement"
      Shelley.DCertGenesis{}                  -> "Genesis key delegation"
      Shelley.DCertMir{}                      -> "MIR"


-- ----------------------------------------------------------------------------
-- Stake address certificates
--

makeStakeAddressRegistrationCertificate
  :: StakeCredential
  -> Certificate
makeStakeAddressRegistrationCertificate stakecred =
    Certificate
  . Shelley.DCertDeleg
  $ Shelley.RegKey
      (toShelleyStakeCredential stakecred)

makeStakeAddressDeregistrationCertificate
  :: StakeCredential
  -> Certificate
makeStakeAddressDeregistrationCertificate stakecred =
    Certificate
  . Shelley.DCertDeleg
  $ Shelley.DeRegKey
      (toShelleyStakeCredential stakecred)

makeStakeAddressDelegationCertificate
  :: StakeCredential
  -> PoolId
  -> Certificate
makeStakeAddressDelegationCertificate stakecred (StakePoolKeyHash poolid) =
    Certificate
  . Shelley.DCertDeleg
  . Shelley.Delegate
  $ Shelley.Delegation
      (toShelleyStakeCredential stakecred)
      poolid


-- ----------------------------------------------------------------------------
-- Stake pool certificates
--

makeStakePoolRegistrationCertificate
  :: StakePoolParameters
  -> Certificate
makeStakePoolRegistrationCertificate poolparams =
    Certificate
  . Shelley.DCertPool
  $ Shelley.RegPool
      (toShelleyPoolParams poolparams)

makeStakePoolRetirementCertificate
  :: PoolId
  -> EpochNo
  -> Certificate
makeStakePoolRetirementCertificate (StakePoolKeyHash poolid) epochno =
    Certificate
  . Shelley.DCertPool
  $ Shelley.RetirePool
      poolid
      epochno

type PoolId = Hash StakePoolKey

data StakePoolParameters =
     StakePoolParameters {
       stakePoolId            :: PoolId,
       stakePoolVRF           :: Hash VrfKey,
       stakePoolCost          :: Lovelace,
       stakePoolMargin        :: Rational,
       stakePoolRewardAccount :: StakeAddress,
       stakePoolPledge        :: Lovelace,
       stakePoolOwners        :: [Hash StakeKey],
       stakePoolRelays        :: [StakePoolRelay],
       stakePoolMetadata      :: Maybe StakePoolMetadataReference
     }
  deriving (Eq, Show)

data StakePoolRelay =

       -- | One or both of IPv4 & IPv6
       StakePoolRelayIp
          (Maybe IPv4) (Maybe IPv6) (Maybe PortNumber)

       -- | An DNS name pointing to a @A@ or @AAAA@ record.
     | StakePoolRelayDnsARecord
          ByteString (Maybe PortNumber)

       -- | A DNS name pointing to a @SRV@ record.
     | StakePoolRelayDnsSrvRecord
          ByteString

  deriving (Eq, Show)

data StakePoolMetadataReference =
     StakePoolMetadataReference {
       stakePoolMetadataURL  :: Text,
       stakePoolMetadataHash :: Hash StakePoolMetadata
     }
  deriving (Eq, Show)

toShelleyPoolParams :: StakePoolParameters -> Shelley.PoolParams StandardShelley
toShelleyPoolParams StakePoolParameters {
                      stakePoolId            = StakePoolKeyHash poolkh
                    , stakePoolVRF           = VrfKeyHash vrfkh
                    , stakePoolCost
                    , stakePoolMargin
                    , stakePoolRewardAccount
                    , stakePoolPledge
                    , stakePoolOwners
                    , stakePoolRelays
                    , stakePoolMetadata
                    } =
    --TODO: validate pool parameters
    Shelley.PoolParams {
      Shelley._poolId     = poolkh
    , Shelley._poolVrf    = vrfkh
    , Shelley._poolPledge = toShelleyLovelace stakePoolPledge
    , Shelley._poolCost   = toShelleyLovelace stakePoolCost
    , Shelley._poolMargin = Shelley.truncateUnitInterval
                              (fromRational stakePoolMargin)
    , Shelley._poolRAcnt  = toShelleyStakeAddr stakePoolRewardAccount
    , Shelley._poolOwners = Set.fromList
                              [ kh | StakeKeyHash kh <- stakePoolOwners ]
    , Shelley._poolRelays = Seq.fromList
                              (map toShelleyStakePoolRelay stakePoolRelays)
    , Shelley._poolMD     = toShelleyPoolMetaData <$>
                              maybeToStrictMaybe stakePoolMetadata
    }
  where
    toShelleyStakePoolRelay :: StakePoolRelay -> Shelley.StakePoolRelay
    toShelleyStakePoolRelay (StakePoolRelayIp mipv4 mipv6 mport) =
      Shelley.SingleHostAddr
        (fromIntegral <$> maybeToStrictMaybe mport)
        (maybeToStrictMaybe mipv4)
        (maybeToStrictMaybe mipv6)

    toShelleyStakePoolRelay (StakePoolRelayDnsARecord dnsname mport) =
      Shelley.SingleHostName
        (fromIntegral <$> maybeToStrictMaybe mport)
        (toShelleyDnsName dnsname)

    toShelleyStakePoolRelay (StakePoolRelayDnsSrvRecord dnsname) =
      Shelley.MultiHostName
        (toShelleyDnsName dnsname)

    toShelleyPoolMetaData :: StakePoolMetadataReference -> Shelley.PoolMetaData
    toShelleyPoolMetaData StakePoolMetadataReference {
                            stakePoolMetadataURL
                          , stakePoolMetadataHash = StakePoolMetadataHash mdh
                          } =
      Shelley.PoolMetaData {
        Shelley._poolMDUrl  = toShelleyUrl stakePoolMetadataURL
      , Shelley._poolMDHash = Crypto.hashToBytes mdh
      }

    toShelleyDnsName :: ByteString -> Shelley.DnsName
    toShelleyDnsName = fromMaybe (error "toShelleyDnsName: invalid dns name. TODO: proper validation")
                     . Shelley.textToDns
                     . Text.decodeLatin1

    toShelleyUrl :: Text -> Shelley.Url
    toShelleyUrl = fromMaybe (error "toShelleyUrl: invalid url. TODO: proper validation")
                 . Shelley.textToUrl


-- ----------------------------------------------------------------------------
-- Special certificates
--

makeGenesisKeyDelegationCertificate
  :: Hash GenesisKey
  -> Hash GenesisDelegateKey
  -> Hash VrfKey
  -> Certificate
makeGenesisKeyDelegationCertificate (GenesisKeyHash         genesiskh)
                                    (GenesisDelegateKeyHash delegatekh)
                                    (VrfKeyHash             vrfkh) =
    Certificate
  . Shelley.DCertGenesis
  $ Shelley.GenesisDelegCert
      genesiskh
      delegatekh
      vrfkh

makeMIRCertificate
  :: MIRPot
  -> [(StakeCredential, Lovelace)]
  -> Certificate
makeMIRCertificate mirpot amounts =
    Certificate
  . Shelley.DCertMir
  $ Shelley.MIRCert
      mirpot
      (Map.fromListWith (<>)
         [ (toShelleyStakeCredential sc, toShelleyLovelace v)
         | (sc, v) <- amounts ])

