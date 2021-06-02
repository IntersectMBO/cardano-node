{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.ScriptData (
    -- * Script data
    ScriptData(..),

    -- * Internal conversion functions
    toPlutusData,
    fromPlutusData,
    toAlonzoData,
    fromAlonzoData,

    -- * Data family instances
    AsType(..),
    Hash(..),
  ) where

import           Prelude

import qualified Data.ByteString as BS
import           Data.String (IsString)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Cardano.Ledger.SafeHash as Ledger

import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)

import qualified Cardano.Ledger.Alonzo.Data as Alonzo

import qualified Plutus.V1.Ledger.Api as Plutus

import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.KeysShelley
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw


-- ----------------------------------------------------------------------------
-- Script data
--

data ScriptData = ScriptDataConstructor Integer [ScriptData]
                | ScriptDataMap         [(ScriptData, ScriptData)]
                | ScriptDataList        [ScriptData]
                | ScriptDataNumber      Integer
                | ScriptDataBytes       BS.ByteString
  deriving (Eq, Ord, Show)
  -- Note the order of constructors is the same as the Plutus definitions
  -- so that the Ord instance is consistent with the Plutus one.
  -- This is checked by prop_ord_distributive_ScriptData

instance HasTypeProxy ScriptData where
    data AsType ScriptData = AsScriptData
    proxyToAsType _ = AsScriptData


-- ----------------------------------------------------------------------------
-- Script data hash
--

newtype instance Hash ScriptData =
    ScriptDataHash (Alonzo.DataHash StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash ScriptData)

instance SerialiseAsRawBytes (Hash ScriptData) where
    serialiseToRawBytes (ScriptDataHash dh) =
      Crypto.hashToBytes (Ledger.extractHash dh)

    deserialiseFromRawBytes (AsHash AsScriptData) bs =
      ScriptDataHash . Ledger.unsafeMakeSafeHash <$> Crypto.hashFromBytes bs

instance ToJSON (Hash ScriptData) where
    toJSON = toJSON . serialiseToRawBytesHexText

instance Aeson.ToJSONKey (Hash ScriptData) where
    toJSONKey = Aeson.toJSONKeyText serialiseToRawBytesHexText


-- ----------------------------------------------------------------------------
-- Conversion functions
--

toAlonzoData :: ScriptData -> Alonzo.Data ledgerera
toAlonzoData = Alonzo.Data . toPlutusData

fromAlonzoData :: Alonzo.Data ledgerera -> ScriptData
fromAlonzoData = fromPlutusData . Alonzo.getPlutusData


toPlutusData :: ScriptData -> Plutus.Data
toPlutusData (ScriptDataConstructor int xs)
                                  = Plutus.Constr int
                                      [ toPlutusData x | x <- xs ]
toPlutusData (ScriptDataMap  kvs) = Plutus.Map
                                      [ (toPlutusData k, toPlutusData v)
                                      | (k,v) <- kvs ]
toPlutusData (ScriptDataList  xs) = Plutus.List
                                      [ toPlutusData x | x <- xs ]
toPlutusData (ScriptDataNumber n) = Plutus.I n
toPlutusData (ScriptDataBytes bs) = Plutus.B bs


fromPlutusData :: Plutus.Data -> ScriptData
fromPlutusData (Plutus.Constr int xs)
                                = ScriptDataConstructor int
                                    [ fromPlutusData x | x <- xs ]
fromPlutusData (Plutus.Map kvs) = ScriptDataMap
                                    [ (fromPlutusData k, fromPlutusData v)
                                    | (k,v) <- kvs ]
fromPlutusData (Plutus.List xs) = ScriptDataList
                                    [ fromPlutusData x | x <- xs ]
fromPlutusData (Plutus.I     n) = ScriptDataNumber n
fromPlutusData (Plutus.B    bs) = ScriptDataBytes bs

