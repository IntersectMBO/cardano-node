{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Shelley.Orphans () where

import           Cardano.Prelude

import           Control.Monad.Fail (fail)
import           Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..),
                    ToJSONKey (..), ToJSONKeyFunction (..), Value (..), (.=), (.:), (.!=), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import           Data.Aeson.Types (Parser)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Char8 as BS
import           Data.Char (isPrint)
import           Data.IP (IPv4, IPv6)
import           Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Slotting.Slot (EpochSize (..))
import           Cardano.TracingOrphanInstances.Common () -- For ToJSON EpochNo
import           Ouroboros.Consensus.BlockchainTime (SlotLength (..), SystemStart (..))
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..), emptyGenesisStaking)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)

import           Shelley.Spec.Ledger.Address (Addr(..), serialiseAddr, deserialiseAddr)
import           Shelley.Spec.Ledger.BaseTypes (DnsName, Nonce (..), Port, StrictMaybe,
                    UnitInterval (..), truncateUnitInterval)
import           Shelley.Spec.Ledger.Coin (Coin(..))
import           Shelley.Spec.Ledger.Credential (RewardAcnt (..), StakeCredential, Credential (..))
import qualified Shelley.Spec.Ledger.Credential as Ledger
import           Shelley.Spec.Ledger.Crypto (Crypto)
import           Shelley.Spec.Ledger.Keys (KeyHash(..))
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.LedgerState as Ledger
import           Shelley.Spec.Ledger.MetaData (MetaDataHash(..))
import           Shelley.Spec.Ledger.PParams (PParams, PParams' (..), ProtVer (..))
import qualified Shelley.Spec.Ledger.PParams as Ledger
import           Shelley.Spec.Ledger.Scripts (ScriptHash (..))
import           Shelley.Spec.Ledger.TxData (TxId(..), TxIn(..), TxOut(..))
import qualified Shelley.Spec.Ledger.TxData as Ledger
import           Shelley.Spec.Ledger.UTxO (UTxO(..))



instance Crypto crypto => ToJSON (ShelleyGenesis crypto) where
  toJSON sg =
    Aeson.object
      [ "startTime"             .= sgStartTime sg
        --TODO: this should not have both network magic and protocol magic
        -- they are different names for the same thing used in two ways.
      , "networkMagic"          .= sgNetworkMagic sg
      , "protocolMagicId"       .= sgProtocolMagicId sg
      , "activeSlotsCoeff"      .= sgActiveSlotsCoeff sg
      , "securityParam"         .= sgSecurityParam sg
      , "epochLength"           .= sgEpochLength sg
      , "slotsPerKESPeriod"     .= sgSlotsPerKESPeriod sg
      , "maxKESEvolutions"      .= sgMaxKESEvolutions sg
      , "slotLength"            .= sgSlotLength sg
      , "updateQuorum"          .= sgUpdateQuorum sg
      , "maxMajorPV"            .= sgMaxMajorPV sg
      , "maxLovelaceSupply"     .= sgMaxLovelaceSupply sg
      , "protocolParams"        .= sgProtocolParams sg
      , "genDelegs"             .= sgGenDelegs sg
      , "initialFunds"          .= sgInitialFunds sg
      , "staking"               .= Null
      ]

instance Crypto crypto => FromJSON (ShelleyGenesis crypto) where
  parseJSON =
    Aeson.withObject "ShelleyGenesis" $ \ obj ->
      ShelleyGenesis
        <$> obj .: "startTime"
        <*> obj .: "networkMagic"
        <*> obj .: "protocolMagicId"
        <*> obj .: "activeSlotsCoeff"
        <*> obj .: "securityParam"
        <*> obj .: "epochLength"
        <*> obj .: "slotsPerKESPeriod"
        <*> obj .: "maxKESEvolutions"
        <*> obj .: "slotLength"
        <*> obj .: "updateQuorum"
        <*> obj .: "maxMajorPV"
        <*> obj .: "maxLovelaceSupply"
        <*> obj .: "protocolParams"
        <*> obj .: "genDelegs"
        <*> obj .: "initialFunds"
        <*> pure emptyGenesisStaking  --TODO

instance ToJSON PParams where
  toJSON pp =
    Aeson.object
      [ "minFeeA" .= _minfeeA pp
      , "minFeeB" .= _minfeeB pp
      , "maxBlockBodySize" .= _maxBBSize pp
      , "maxTxSize" .= _maxTxSize pp
      , "maxBlockHeaderSize" .= _maxBHSize pp
      , "keyDeposit" .= _keyDeposit pp
      , "keyMinRefund" .= _keyMinRefund pp
      , "keyDecayRate" .= (fromRational $ _keyDecayRate pp :: Double)
      , "poolDeposit" .= _poolDeposit pp
      , "poolMinRefund" .= _poolMinRefund pp
      , "poolDecayRate" .= (fromRational $ _poolDecayRate pp :: Double)
      , "eMax" .= _eMax pp
      , "nOpt" .= _nOpt pp
      , "a0" .= (fromRational $ _a0 pp :: Double)
      , "rho" .= _rho pp
      , "tau" .= _tau pp
      , "decentralisationParam" .= _d pp
      , "extraEntropy" .= _extraEntropy pp
      , "protocolVersion" .= _protocolVersion pp
      , "minUTxOValue" .= _minUTxOValue pp
      ]

instance FromJSON PParams where
  parseJSON =
      Aeson.withObject "PParams" $ \ obj ->
        PParams
          <$> obj .: "minFeeA"
          <*> obj .: "minFeeB"
          <*> obj .: "maxBlockBodySize"
          <*> obj .: "maxTxSize"
          <*> obj .: "maxBlockHeaderSize"
          <*> obj .: "keyDeposit"
          <*> obj .: "keyMinRefund"
          <*> parseRationalFromDouble (obj .: "keyDecayRate")
          <*> obj .: "poolDeposit"
          <*> obj .: "poolMinRefund"
          <*> parseRationalFromDouble (obj .: "poolDecayRate")
          <*> obj .: "eMax"
          <*> obj .: "nOpt"
          <*> parseRationalFromDouble (obj .: "a0")
          <*> obj .: "rho"
          <*> obj .: "tau"
          <*> obj .: "decentralisationParam"
          <*> obj .: "extraEntropy"
          <*> obj .: "protocolVersion"
          <*> obj .:? "minUTxOValue" .!= 0
      where
        parseRationalFromDouble :: Parser Double -> Parser Rational
        parseRationalFromDouble p = realToFrac <$> p

instance ToJSON UnitInterval where
  toJSON (UnsafeUnitInterval r) = toJSON (fromRational r :: Double)

instance FromJSON UnitInterval where
  parseJSON v =
    truncateUnitInterval . realToFrac
      <$> (parseJSON v :: Parser Double)

instance ToJSON ProtVer where
  toJSON (ProtVer major minor) =
    Aeson.object
      [ "major" .= major
      , "minor" .= minor
      ]

instance FromJSON ProtVer where
  parseJSON =
    Aeson.withObject "ProtVer" $ \ obj ->
      ProtVer
        <$> obj .: "major"
        <*> obj .: "minor"

deriving instance ToJSON Nonce
deriving instance FromJSON Nonce

instance Crypto c => ToJSONKey (TxIn c) where
  toJSONKey = ToJSONKeyText txInToText (Aeson.text . txInToText)

txInToText :: TxIn c -> Text
txInToText (TxIn (TxId txidHash) ix) =
  hashToText txidHash
    <> Text.pack "#"
    <> Text.pack (show ix)

deriving instance Crypto c => ToJSON (TxIn c)

instance Crypto c => ToJSON (TxOut c) where
  toJSON (TxOut addr amount) =
    Aeson.object
      [ "address" .= addr
      , "amount" .= amount
      ]

deriving instance ToJSONKey (StakeCredential c)

deriving instance ToJSON (StakeCredential c)


--
-- Simple newtype wrappers JSON conversion
--

deriving newtype instance ToJSON (TxId c)

deriving newtype instance Crypto c => ToJSON (UTxO c)

-- These are for ShelleyGenesis.
-- These ones are all just newtype wrappers of numbers,
-- so newtype deriving for the JSON format is ok.

deriving newtype instance ToJSON   Coin
deriving newtype instance FromJSON Coin

deriving newtype instance ToJSON   EpochSize
deriving newtype instance FromJSON EpochSize

deriving newtype instance ToJSON   NetworkMagic
deriving newtype instance FromJSON NetworkMagic

deriving newtype instance ToJSON   SecurityParam
deriving newtype instance FromJSON SecurityParam

-- A 'NominalDiffTime' time
instance ToJSON   SlotLength where
  toJSON = toJSON . getSlotLength

instance FromJSON SlotLength where
  parseJSON = fmap mkSlotLength . parseJSON

-- A UTCTime, with format like "2020-04-15 11:44:07"
deriving newtype instance ToJSON   SystemStart
deriving newtype instance FromJSON SystemStart

deriving newtype instance ToJSONKey (RewardAcnt c)

deriving newtype instance ToJSON (RewardAcnt c)

deriving newtype instance ToJSON (ScriptHash c)

deriving newtype instance ToJSON (MetaDataHash c)

--
-- Genesis key hashes JSON conversion, including as map keys
--

deriving newtype instance ToJSONKey (KeyHash disc crypto)
deriving newtype instance Crypto crypto =>
                          FromJSONKey (KeyHash disc crypto)

deriving newtype instance ToJSON (KeyHash disc crypto)
deriving newtype instance Crypto crypto =>
                          FromJSON (KeyHash disc crypto)

instance ToJSONKey (Hash crypto a) where
  toJSONKey = ToJSONKeyText hashToText (Aeson.text . hashToText)

instance HashAlgorithm crypto => FromJSONKey (Hash crypto a) where
  fromJSONKey = FromJSONKeyTextParser parseHash

instance ToJSON   (Hash crypto a) where
  toJSON = toJSON . hashToText

instance HashAlgorithm crypto => FromJSON (Hash crypto a) where
  parseJSON = Aeson.withText "hash" parseHash

hashToText :: Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.getHashBytesAsHex

parseHash :: HashAlgorithm crypto => Text -> Parser (Hash crypto a)
parseHash t = do
    bytes <- either badHex return (parseBase16 t)
    maybe badSize return (Crypto.hashFromBytes bytes)
  where
    badHex _ = fail "Hashes are expected in hex encoding"
    badSize  = fail "Hash is the wrong length"


--
-- Addresses JSON conversion, including as map keys
--

instance Crypto crypto => ToJSONKey (Addr crypto) where
  toJSONKey = ToJSONKeyText addrToText (Aeson.text . addrToText)

instance Crypto crypto => FromJSONKey (Addr crypto) where
  fromJSONKey = FromJSONKeyTextParser parseAddr

instance Crypto crypto => ToJSON (Addr crypto) where
  toJSON = toJSON . addrToText

instance Crypto crypto => FromJSON (Addr crypto) where
  parseJSON = Aeson.withText "address" parseAddr

addrToText :: Crypto crypto => Addr crypto -> Text
addrToText =
     Text.decodeLatin1 . Base16.encode . serialiseAddr

parseAddr :: Crypto crypto => Text -> Parser (Addr crypto)
parseAddr t = do
    bytes <- either badHex return (parseBase16 t)
    maybe badFormat return (deserialiseAddr bytes)
  where
    badHex h = fail $ "Addresses are expected in hex encoding for now: " ++ show h
    badFormat = fail "Address is not in the right format"

-- We are deriving ToJSON instances for all of these types mainly so we can dump
-- a JSON representation for the purposes of debug. Therefore ByteString that are
-- not printable should be hex encoded for readability.
instance ToJSON ByteString where
  toJSON bs =
    toJSON $
      if BS.all isPrint bs
        then bs
        else Base16.encode bs

instance ToJSON a => ToJSON (StrictSeq a) where
  toJSON ss =
    toJSON $ toList (getSeq ss)

deriving instance ToJSON a => ToJSON (StrictMaybe a)

deriving anyclass instance ToJSON DnsName
deriving anyclass instance ToJSON IPv4
deriving anyclass instance ToJSON IPv6
deriving anyclass instance ToJSON Ledger.Url
deriving anyclass instance ToJSON Port

deriving anyclass instance ToJSON (Ledger.GenDelegs TPraosStandardCrypto)
deriving anyclass instance ToJSON (Ledger.ProposedPPUpdates TPraosStandardCrypto)
deriving anyclass instance ToJSON (Ledger.StakeCreds TPraosStandardCrypto)
deriving anyclass instance ToJSON (Ledger.StakePools TPraosStandardCrypto)

deriving instance ToJSON Ledger.PoolMetaData
deriving instance ToJSON Ledger.Ptr
deriving instance ToJSON Ledger.StakePoolRelay

deriving instance ToJSON (Ledger.DPState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.DState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.FutureGenDeleg TPraosStandardCrypto)
deriving instance ToJSON (Ledger.LedgerState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.PoolParams TPraosStandardCrypto)
deriving instance ToJSON (Ledger.PParams' StrictMaybe)
deriving instance ToJSON (Ledger.PState TPraosStandardCrypto)
deriving instance ToJSON (Ledger.StakeReference TPraosStandardCrypto)
deriving instance ToJSON (Ledger.UTxOState TPraosStandardCrypto)

deriving instance ToJSONKey Ledger.Ptr
deriving instance ToJSONKey (Ledger.FutureGenDeleg TPraosStandardCrypto)
