{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Metadata embedded in transactions
--
module Cardano.Api.TxMetadata (

    TxMetadata (TxMetadata, TxMetadataShelley),
    TxMetadataValue(..),
    makeTransactionMetadata,

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import           Data.Word
import           Data.Text (Text)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Lazy as Map.Lazy
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Cardano.Binary as CBOR

import qualified Shelley.Spec.Ledger.MetaData as Shelley

import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseCBOR


newtype TxMetadata = TxMetadataShelley Shelley.MetaData
    deriving (Eq, Show)

{-# COMPLETE TxMetadata #-}
pattern TxMetadata :: Map Word64 TxMetadataValue -> TxMetadata
pattern TxMetadata m <- TxMetadataShelley (fromShelleyMetaData -> m) where
    TxMetadata = TxMetadataShelley . toShelleyMetaData

data TxMetadataValue = TxMetaNumber Integer -- -2^64 .. 2^64-1
                     | TxMetaBytes  ByteString
                     | TxMetaText   Text
                     | TxMetaList   [TxMetadataValue]
                     | TxMetaMap    [(TxMetadataValue, TxMetadataValue)]
    deriving (Eq, Ord, Show)

-- | Merge metadata maps. When there are clashing entries the left hand side
-- takes precedence.
--
instance Semigroup TxMetadata where
    TxMetadataShelley (Shelley.MetaData m1)
      <> TxMetadataShelley (Shelley.MetaData m2) =

      TxMetadataShelley (Shelley.MetaData (m1 <> m2))

instance Monoid TxMetadata where
    mempty = TxMetadataShelley (Shelley.MetaData mempty)

instance HasTypeProxy TxMetadata where
    data AsType TxMetadata = AsTxMetadata
    proxyToAsType _ = AsTxMetadata

instance SerialiseAsCBOR TxMetadata where
    serialiseToCBOR (TxMetadataShelley tx) =
      CBOR.serialize' tx

    deserialiseFromCBOR AsTxMetadata bs =
      TxMetadataShelley <$>
        CBOR.decodeAnnotator "TxMetadata" fromCBOR (LBS.fromStrict bs)

makeTransactionMetadata :: Map Word64 TxMetadataValue -> TxMetadata
makeTransactionMetadata = TxMetadata


-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

toShelleyMetaData :: Map Word64 TxMetadataValue -> Shelley.MetaData
toShelleyMetaData =
    Shelley.MetaData
  . Map.map toShelleyMetaDatum
  where
    toShelleyMetaDatum :: TxMetadataValue -> Shelley.MetaDatum
    toShelleyMetaDatum (TxMetaNumber x) = Shelley.I x
    toShelleyMetaDatum (TxMetaBytes  x) = Shelley.B x
    toShelleyMetaDatum (TxMetaText   x) = Shelley.S x
    toShelleyMetaDatum (TxMetaList  xs) = Shelley.List
                                            [ toShelleyMetaDatum x | x <- xs ]
    toShelleyMetaDatum (TxMetaMap   xs) = Shelley.Map
                                            [ (toShelleyMetaDatum k,
                                               toShelleyMetaDatum v)
                                            | (k,v) <- xs ]

fromShelleyMetaData :: Shelley.MetaData -> Map Word64 TxMetadataValue
fromShelleyMetaData (Shelley.MetaData mdMap) =
    Map.Lazy.map fromShelleyMetaDatum mdMap
  where
    fromShelleyMetaDatum :: Shelley.MetaDatum -> TxMetadataValue
    fromShelleyMetaDatum (Shelley.I     x) = TxMetaNumber x
    fromShelleyMetaDatum (Shelley.B     x) = TxMetaBytes  x
    fromShelleyMetaDatum (Shelley.S     x) = TxMetaText   x
    fromShelleyMetaDatum (Shelley.List xs) = TxMetaList
                                               [ fromShelleyMetaDatum x | x <- xs ]
    fromShelleyMetaDatum (Shelley.Map  xs) = TxMetaMap
                                               [ (fromShelleyMetaDatum k,
                                                  fromShelleyMetaDatum v)
                                               | (k,v) <- xs ]

