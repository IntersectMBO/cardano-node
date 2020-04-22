{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Config.Shelley.Genesis
  ( ShelleyGenesis(..)
  , shelleyGenesisDefaults
  , shelleyGenesisToJSON
  , shelleyGenesisFromJSON
  ) where

import           Cardano.Prelude

import qualified Data.Text.Encoding as Text
import qualified Data.Map.Strict as Map
import qualified Data.Time as Time

import           Data.Aeson (Value, ToJSON(..), toJSON, (.=),
                             FromJSON(..), (.:),
                             ToJSONKey(..),   ToJSONKeyFunction(..),
                             FromJSONKey(..), FromJSONKeyFunction(..))
import           Data.Aeson.Types    (Parser)
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString.Base16 as Base16

import           Control.Monad.Fail (fail)

import           Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId(..))
import           Cardano.Slotting.Slot (EpochSize (..))

import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Consensus.BlockchainTime
                   (SlotLength (..), SystemStart (..), slotLengthFromSec)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)


import           Shelley.Spec.Ledger.Address (serialiseAddr, deserialiseAddr)
import           Shelley.Spec.Ledger.Coin   (Coin(..))
import           Shelley.Spec.Ledger.Crypto (Crypto)
import           Shelley.Spec.Ledger.Keys   (HashAlgorithm)
import           Shelley.Spec.Ledger.Keys   (DiscKeyHash(..))
import           Shelley.Spec.Ledger.TxData (Addr(..))



-- | Some reasonable starting defaults for constructing a 'ShelleyGenesis'.
--
-- You must override at least the following fields for this to be useful:
--
-- * 'sgStartTime' the time of the first block
-- * 'sgNetworkMagic' to a suitable testnet or mainnet network magic number.
-- * 'sgGenDelegs' to have some initial nodes
-- * 'sgInitialFunds' to have any money in the system
-- * 'sgMaxLovelaceSupply' must be at least the sum of the 'sgInitialFunds'
--   but more if you want to allow for rewards.
--
shelleyGenesisDefaults :: ShelleyGenesis crypto
shelleyGenesisDefaults =
  ShelleyGenesis
    {
      -- params for this specific chain
      sgStartTime             = SystemStart zeroTime
    , sgNetworkMagic          = NetworkMagic 0
    , sgProtocolMagicId       = ProtocolMagicId 0

      -- protocol params
    , sgSlotLength            = slotLengthFromSec 1 -- 1s slots
    , sgActiveSlotsCoeff      = 1/20                -- 20s block times on average
    , sgDecentralisationParam = 1
    , sgSecurityParam         = SecurityParam k
    , sgEpochLength           = EpochSize (10 * k)
    , sgSlotsPerKESPeriod     = 60 * 60 * 24        -- 1 day with 1s slots
    , sgMaxKESEvolutions      = 90                  -- 90 days
    , sgUpdateQuorum          = 5

      -- ledger params
    , sgMaxMajorPV            = 1000
    , sgMaxBodySize           = 1024 * 16
    , sgMaxHeaderSize         = 1400

      -- genesis keys and initial funds
    , sgGenDelegs             = Map.empty
    , sgInitialFunds          = Map.empty
    , sgMaxLovelaceSupply     = 0
    }
  where
    k = 2160
    zeroTime = Time.UTCTime (Time.fromGregorian 1970 1 1) 0 -- tradition


--
-- ShelleyGenesis JSON conversion
--

shelleyGenesisToJSON :: ShelleyGenesis TPraosStandardCrypto -> Value
shelleyGenesisToJSON = toJSON

shelleyGenesisFromJSON :: Value -> Parser (ShelleyGenesis TPraosStandardCrypto)
shelleyGenesisFromJSON = parseJSON

instance Crypto crypto => ToJSON (ShelleyGenesis crypto) where
  toJSON sg =
    Aeson.object
      [ "StartTime"             .= sgStartTime sg
        --TODO: this should not have both network magic and protocol magic
        -- they are different names for the same thing used in two ways.
      , "NetworkMagic"          .= sgNetworkMagic sg
      , "ProtocolMagicId"       .= sgProtocolMagicId sg
      , "ActiveSlotsCoeff"      .= sgActiveSlotsCoeff sg
      , "DecentralisationParam" .= sgDecentralisationParam sg
      , "SecurityParam"         .= sgSecurityParam sg
      , "EpochLength"           .= sgEpochLength sg
      , "SlotsPerKESPeriod"     .= sgSlotsPerKESPeriod sg
      , "MaxKESEvolutions"      .= sgMaxKESEvolutions sg
      , "SlotLength"            .= sgSlotLength sg
      , "UpdateQuorum"          .= sgUpdateQuorum sg
      , "MaxMajorPV"            .= sgMaxMajorPV sg
      , "MaxLovelaceSupply"     .= sgMaxLovelaceSupply sg
      , "MaxBodySize"           .= sgMaxBodySize sg
      , "MaxHeaderSize"         .= sgMaxHeaderSize sg
      , "GenDelegs"             .= sgGenDelegs sg
      , "InitialFunds"          .= sgInitialFunds sg
      ]

instance Crypto crypto => FromJSON (ShelleyGenesis crypto) where
  parseJSON =
    Aeson.withObject "ShelleyGenesis" $ \ obj ->
      ShelleyGenesis
        <$> obj .: "StartTime"
        <*> obj .: "NetworkMagic"
        <*> obj .: "ProtocolMagicId"
        <*> obj .: "ActiveSlotsCoeff"
        <*> obj .: "DecentralisationParam"
        <*> obj .: "SecurityParam"
        <*> obj .: "EpochLength"
        <*> obj .: "SlotsPerKESPeriod"
        <*> obj .: "MaxKESEvolutions"
        <*> obj .: "SlotLength"
        <*> obj .: "UpdateQuorum"
        <*> obj .: "MaxMajorPV"
        <*> obj .: "MaxLovelaceSupply"
        <*> obj .: "MaxBodySize"
        <*> obj .: "MaxHeaderSize"
        <*> obj .: "GenDelegs"
        <*> obj .: "InitialFunds"


--
-- Simple newtype wrappers JSON conversion
--

-- These ones are all just newtype wrappers of numbers,
-- so newtype deriving for the JSON format is ok.
deriving newtype instance Aeson.ToJSON   Coin
deriving newtype instance Aeson.FromJSON Coin

deriving newtype instance Aeson.ToJSON   EpochSize
deriving newtype instance Aeson.FromJSON EpochSize

deriving newtype instance Aeson.ToJSON   NetworkMagic
deriving newtype instance Aeson.FromJSON NetworkMagic

deriving newtype instance Aeson.ToJSON   SecurityParam
deriving newtype instance Aeson.FromJSON SecurityParam

deriving newtype instance Aeson.ToJSON   SlotLength
deriving newtype instance Aeson.FromJSON SlotLength

-- A UTCTime, with format like "2020-04-15 11:44:07"
deriving newtype instance Aeson.ToJSON   SystemStart
deriving newtype instance Aeson.FromJSON SystemStart


--
-- Genesis key hashes JSON conversion, including as map keys
--

deriving newtype instance ToJSONKey (DiscKeyHash disc crypto)
deriving newtype instance Crypto crypto =>
                          FromJSONKey (DiscKeyHash disc crypto)

deriving newtype instance ToJSON (DiscKeyHash disc crypto)
deriving newtype instance Crypto crypto =>
                          FromJSON (DiscKeyHash disc crypto)

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

