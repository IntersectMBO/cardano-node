{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Cardano.Config.Shelley.Orphans () where

import           Cardano.Prelude

import           Control.Monad.Fail (fail)
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as Text

import           Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Slotting.Slot (EpochSize (..))
import           Cardano.TracingOrphanInstances.Common () -- For ToJSON EpochNo
import           Ouroboros.Consensus.BlockchainTime
                   (SlotLength (..), SystemStart (..))
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Shelley.Node
                   (ShelleyGenesis (..), emptyGenesisStaking)
import           Shelley.Spec.Ledger.Address (serialiseAddr, deserialiseAddr)
import           Shelley.Spec.Ledger.BaseTypes (Nonce (..), UnitInterval (..))
import           Shelley.Spec.Ledger.Coin (Coin(..))
import           Shelley.Spec.Ledger.Crypto (Crypto)
import           Shelley.Spec.Ledger.Keys (KeyHash(..))
import           Shelley.Spec.Ledger.PParams (PParams, ProtVer (..))
import           Shelley.Spec.Ledger.TxData (Addr(..))

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
--    , "Staking"               .= sgStaking sg  --TODO
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
        <*> pure emptyGenesisStaking  --TODO

deriving instance ToJSON PParams
deriving instance ToJSON ProtVer
deriving instance ToJSON Nonce

--
-- Simple newtype wrappers JSON conversion
--

-- This instance is for PParams
deriving newtype instance ToJSON UnitInterval

-- These are for ShelleyGenesis.
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

-- A 'NominalDiffTime' time
instance Aeson.ToJSON   SlotLength where
  toJSON = toJSON . getSlotLength

instance Aeson.FromJSON SlotLength where
  parseJSON = fmap mkSlotLength . parseJSON

-- A UTCTime, with format like "2020-04-15 11:44:07"
deriving newtype instance Aeson.ToJSON   SystemStart
deriving newtype instance Aeson.FromJSON SystemStart


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
