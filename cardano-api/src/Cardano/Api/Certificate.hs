{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
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

    -- * Internal conversion functions
    toShelleyCertificate,
    fromShelleyCertificate,
    toShelleyPoolParams,

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
import qualified Data.Foldable as Foldable

import           Data.IP (IPv4, IPv6)
import           Network.Socket (PortNumber)

import           Cardano.Slotting.Slot (EpochNo (..))
import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Cardano.Ledger.Era as Ledger
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import           Shelley.Spec.Ledger.BaseTypes
                   (maybeToStrictMaybe, strictMaybeToMaybe)
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

data Certificate =

     -- Stake address certificates
     StakeAddressRegistrationCertificate   StakeCredential
   | StakeAddressDeregistrationCertificate StakeCredential
   | StakeAddressDelegationCertificate     StakeCredential PoolId

     -- Stake pool certificates
   | StakePoolRegistrationCertificate StakePoolParameters
   | StakePoolRetirementCertificate   PoolId EpochNo

     -- Special certificates
   | GenesisKeyDelegationCertificate (Hash GenesisKey)
                                     (Hash GenesisDelegateKey)
                                     (Hash VrfKey)
   | MIRCertificate MIRPot [(StakeCredential, Lovelace)]

  deriving stock (Eq, Show)
  deriving anyclass SerialiseAsCBOR

instance HasTypeProxy Certificate where
    data AsType Certificate = AsCertificate
    proxyToAsType _ = AsCertificate

instance ToCBOR Certificate where
    toCBOR = toCBOR . toShelleyCertificate @StandardShelley

instance FromCBOR Certificate where
    fromCBOR = fromShelleyCertificate @StandardShelley <$> fromCBOR

instance HasTextEnvelope Certificate where
    textEnvelopeType _ = "CertificateShelley"
    textEnvelopeDefaultDescr cert = case cert of
      StakeAddressRegistrationCertificate{}   -> "Stake address registration"
      StakeAddressDeregistrationCertificate{} -> "Stake address de-registration"
      StakeAddressDelegationCertificate{}     -> "Stake address delegation"
      StakePoolRegistrationCertificate{}      -> "Pool registration"
      StakePoolRetirementCertificate{}        -> "Pool retirement"
      GenesisKeyDelegationCertificate{}       -> "Genesis key delegation"
      MIRCertificate{}                        -> "MIR"


-- ----------------------------------------------------------------------------
-- Stake pool parameters
--

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


-- ----------------------------------------------------------------------------
-- Constructor functions
--

makeStakeAddressRegistrationCertificate :: StakeCredential -> Certificate
makeStakeAddressRegistrationCertificate = StakeAddressRegistrationCertificate

makeStakeAddressDeregistrationCertificate :: StakeCredential -> Certificate
makeStakeAddressDeregistrationCertificate = StakeAddressDeregistrationCertificate

makeStakeAddressDelegationCertificate :: StakeCredential -> PoolId -> Certificate
makeStakeAddressDelegationCertificate = StakeAddressDelegationCertificate

makeStakePoolRegistrationCertificate :: StakePoolParameters -> Certificate
makeStakePoolRegistrationCertificate = StakePoolRegistrationCertificate

makeStakePoolRetirementCertificate :: PoolId -> EpochNo -> Certificate
makeStakePoolRetirementCertificate = StakePoolRetirementCertificate

makeGenesisKeyDelegationCertificate :: Hash GenesisKey
                                    -> Hash GenesisDelegateKey
                                    -> Hash VrfKey
                                    -> Certificate
makeGenesisKeyDelegationCertificate = GenesisKeyDelegationCertificate

makeMIRCertificate :: MIRPot -> [(StakeCredential, Lovelace)] -> Certificate
makeMIRCertificate = MIRCertificate


-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

toShelleyCertificate :: Ledger.Crypto ledgerera ~ StandardCrypto
                     => Certificate -> Shelley.DCert ledgerera
toShelleyCertificate (StakeAddressRegistrationCertificate stakecred) =
    Shelley.DCertDeleg $
      Shelley.RegKey
        (toShelleyStakeCredential stakecred)

toShelleyCertificate (StakeAddressDeregistrationCertificate stakecred) =
    Shelley.DCertDeleg $
      Shelley.DeRegKey
        (toShelleyStakeCredential stakecred)

toShelleyCertificate (StakeAddressDelegationCertificate
                        stakecred (StakePoolKeyHash poolid)) =
    Shelley.DCertDeleg $
    Shelley.Delegate $
      Shelley.Delegation
        (toShelleyStakeCredential stakecred)
        poolid

toShelleyCertificate (StakePoolRegistrationCertificate poolparams) =
    Shelley.DCertPool $
      Shelley.RegPool
        (toShelleyPoolParams poolparams)

toShelleyCertificate (StakePoolRetirementCertificate
                       (StakePoolKeyHash poolid) epochno) =
    Shelley.DCertPool $
      Shelley.RetirePool
        poolid
        epochno

toShelleyCertificate (GenesisKeyDelegationCertificate
                       (GenesisKeyHash         genesiskh)
                       (GenesisDelegateKeyHash delegatekh)
                       (VrfKeyHash             vrfkh)) =
    Shelley.DCertGenesis $
      Shelley.GenesisDelegCert
        genesiskh
        delegatekh
        vrfkh

toShelleyCertificate (MIRCertificate mirpot amounts) =
    Shelley.DCertMir $
      Shelley.MIRCert
        mirpot
        (Map.fromListWith (<>)
           [ (toShelleyStakeCredential sc, toShelleyLovelace v)
           | (sc, v) <- amounts ])


fromShelleyCertificate :: Ledger.Crypto ledgerera ~ StandardCrypto
                       => Shelley.DCert ledgerera -> Certificate
fromShelleyCertificate (Shelley.DCertDeleg (Shelley.RegKey stakecred)) =
    StakeAddressRegistrationCertificate
      (fromShelleyStakeCredential stakecred)

fromShelleyCertificate (Shelley.DCertDeleg (Shelley.DeRegKey stakecred)) =
    StakeAddressDeregistrationCertificate
      (fromShelleyStakeCredential stakecred)

fromShelleyCertificate (Shelley.DCertDeleg
                         (Shelley.Delegate (Shelley.Delegation stakecred poolid))) =
    StakeAddressDelegationCertificate
      (fromShelleyStakeCredential stakecred)
      (StakePoolKeyHash poolid)

fromShelleyCertificate (Shelley.DCertPool (Shelley.RegPool poolparams)) =
    StakePoolRegistrationCertificate
      (fromShelleyPoolParams poolparams)

fromShelleyCertificate (Shelley.DCertPool (Shelley.RetirePool poolid epochno)) =
    StakePoolRetirementCertificate
      (StakePoolKeyHash poolid)
      epochno

fromShelleyCertificate (Shelley.DCertGenesis
                         (Shelley.GenesisDelegCert genesiskh delegatekh vrfkh)) =
    GenesisKeyDelegationCertificate
      (GenesisKeyHash         genesiskh)
      (GenesisDelegateKeyHash delegatekh)
      (VrfKeyHash             vrfkh)

fromShelleyCertificate (Shelley.DCertMir (Shelley.MIRCert mirpot amounts)) =
    MIRCertificate
      mirpot
      [ (fromShelleyStakeCredential sc, fromShelleyLovelace v)
      | (sc, v) <- Map.toList amounts ]


toShelleyPoolParams :: Ledger.Crypto ledgerera ~ StandardCrypto
                    => StakePoolParameters -> Shelley.PoolParams ledgerera
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
    , Shelley._poolMargin = Shelley.unitIntervalFromRational stakePoolMargin
    , Shelley._poolRAcnt  = toShelleyStakeAddr stakePoolRewardAccount
    , Shelley._poolOwners = Set.fromList
                              [ kh | StakeKeyHash kh <- stakePoolOwners ]
    , Shelley._poolRelays = Seq.fromList
                              (map toShelleyStakePoolRelay stakePoolRelays)
    , Shelley._poolMD     = toShelleyPoolMetadata <$>
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

    toShelleyPoolMetadata :: StakePoolMetadataReference -> Shelley.PoolMetadata
    toShelleyPoolMetadata StakePoolMetadataReference {
                            stakePoolMetadataURL
                          , stakePoolMetadataHash = StakePoolMetadataHash mdh
                          } =
      Shelley.PoolMetadata {
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


fromShelleyPoolParams :: Ledger.Crypto ledgerera ~ StandardCrypto
                      => Shelley.PoolParams ledgerera
                      -> StakePoolParameters
fromShelleyPoolParams
    Shelley.PoolParams {
      Shelley._poolId
    , Shelley._poolVrf
    , Shelley._poolPledge
    , Shelley._poolCost
    , Shelley._poolMargin
    , Shelley._poolRAcnt
    , Shelley._poolOwners
    , Shelley._poolRelays
    , Shelley._poolMD
    } =
    StakePoolParameters {
      stakePoolId            = StakePoolKeyHash _poolId
    , stakePoolVRF           = VrfKeyHash _poolVrf
    , stakePoolCost          = fromShelleyLovelace _poolCost
    , stakePoolMargin        = Shelley.unitIntervalToRational _poolMargin
    , stakePoolRewardAccount = fromShelleyStakeAddr _poolRAcnt
    , stakePoolPledge        = fromShelleyLovelace _poolPledge
    , stakePoolOwners        = map StakeKeyHash (Set.toList _poolOwners)
    , stakePoolRelays        = map fromShelleyStakePoolRelay
                                   (Foldable.toList _poolRelays)
    , stakePoolMetadata      = fromShelleyPoolMetadata <$>
                                 strictMaybeToMaybe _poolMD
    }
  where
    fromShelleyStakePoolRelay :: Shelley.StakePoolRelay -> StakePoolRelay
    fromShelleyStakePoolRelay (Shelley.SingleHostAddr mport mipv4 mipv6) =
      StakePoolRelayIp
        (strictMaybeToMaybe mipv4)
        (strictMaybeToMaybe mipv6)
        (fromIntegral . Shelley.portToWord16 <$> strictMaybeToMaybe mport)

    fromShelleyStakePoolRelay (Shelley.SingleHostName mport dnsname) =
      StakePoolRelayDnsARecord
        (fromShelleyDnsName dnsname)
        (fromIntegral . Shelley.portToWord16 <$> strictMaybeToMaybe mport)

    fromShelleyStakePoolRelay (Shelley.MultiHostName dnsname) =
      StakePoolRelayDnsSrvRecord
        (fromShelleyDnsName dnsname)

    fromShelleyPoolMetadata :: Shelley.PoolMetadata -> StakePoolMetadataReference
    fromShelleyPoolMetadata Shelley.PoolMetadata {
                              Shelley._poolMDUrl
                            , Shelley._poolMDHash
                            } =
      StakePoolMetadataReference {
        stakePoolMetadataURL  = Shelley.urlToText _poolMDUrl
      , stakePoolMetadataHash = StakePoolMetadataHash
                              . fromMaybe (error "fromShelleyPoolMetadata: invalid hash. TODO: proper validation")
                              . Crypto.hashFromBytes
                              $ _poolMDHash
      }

    --TODO: change the ledger rep of the DNS name to use ShortByteString
    fromShelleyDnsName :: Shelley.DnsName -> ByteString
    fromShelleyDnsName = Text.encodeUtf8
                       . Shelley.dnsToText
