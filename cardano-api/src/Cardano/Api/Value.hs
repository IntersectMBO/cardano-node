{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Currency values
--
module Cardano.Api.Value
  ( Lovelace(..)

    -- * Multi-asset values
  , Quantity(..)
  , PolicyId(..)
  , AssetName(..)
  , AssetId(..)
  , Value
  , selectAsset
  , valueFromList
  , valueToList
  , filterValue
  , negateValue

    -- ** Ada \/ Lovelace specifically
  , quantityToLovelace
  , lovelaceToQuantity
  , selectLovelace
  , lovelaceToValue

    -- * Era-dependent use of multi-assert values
  , MintValue(..)
  , TxOutValue(..)
  , AdaOnlyInEra(..)
  , MultiAssetInEra(..)

    -- * Internal conversion functions
  , toShelleyLovelace
  , fromShelleyLovelace
  , toMaryValue
  , fromMaryValue
  ) where

import           Prelude

import           Data.ByteString (ByteString)
import qualified Data.Map.Merge.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (IsString)

import qualified Cardano.Ledger.Era as Ledger
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Cardano.Ledger.Mary.Value as Mary

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import           Cardano.Api.Eras
import           Cardano.Api.Script


-- ----------------------------------------------------------------------------
-- Lovelace
--

newtype Lovelace = Lovelace Integer
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

instance Semigroup Lovelace where
  Lovelace a <> Lovelace b = Lovelace (a + b)

instance Monoid Lovelace where
  mempty = Lovelace 0

toShelleyLovelace :: Lovelace -> Shelley.Coin
toShelleyLovelace (Lovelace l) = Shelley.Coin l
--TODO: validate bounds

fromShelleyLovelace :: Shelley.Coin -> Lovelace
fromShelleyLovelace (Shelley.Coin l) = Lovelace l


-- ----------------------------------------------------------------------------
-- Multi asset Value
--

newtype Quantity = Quantity Integer
  deriving newtype (Eq, Ord, Num, Show)

instance Semigroup Quantity where
  Quantity a <> Quantity b = Quantity (a + b)

instance Monoid Quantity where
  mempty = Quantity 0

lovelaceToQuantity :: Lovelace -> Quantity
lovelaceToQuantity (Lovelace x) = Quantity x

quantityToLovelace :: Quantity -> Lovelace
quantityToLovelace (Quantity x) = Lovelace x


newtype PolicyId = PolicyId ScriptHash
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype AssetName = AssetName ByteString
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data AssetId = AdaAssetId
             | AssetId !PolicyId !AssetName
  deriving (Eq, Ord, Show)


newtype Value = Value (Map AssetId Quantity)
  deriving Eq

instance Show Value where
  showsPrec d v = showParen (d > 10) $
    showString "valueFromList " . shows (valueToList v)

instance Semigroup Value where
  Value a <> Value b = Value (mergeAssetMaps a b)

instance Monoid Value where
  mempty = Value Map.empty


{-# NOINLINE mergeAssetMaps #-} -- as per advice in Data.Map.Merge docs
mergeAssetMaps :: Map AssetId Quantity
               -> Map AssetId Quantity
               -> Map AssetId Quantity
mergeAssetMaps =
    Map.merge
      Map.preserveMissing
      Map.preserveMissing
      (Map.zipWithMaybeMatched mergeQuantity)
  where
    mergeQuantity :: AssetId -> Quantity -> Quantity -> Maybe Quantity
    mergeQuantity _k a b =
      case a <> b of
        Quantity 0 -> Nothing
        c          -> Just c

selectAsset :: Value -> (AssetId -> Quantity)
selectAsset (Value m) a = Map.findWithDefault mempty a m

valueFromList :: [(AssetId, Quantity)] -> Value
valueFromList = Value
              . Map.filter (/= 0)
              . Map.fromListWith (<>)

valueToList :: Value -> [(AssetId, Quantity)]
valueToList (Value m) = Map.toList m

-- | This lets you write @a - b@ as @a <> negateValue b@.
--
negateValue :: Value -> Value
negateValue (Value m) = Value (Map.map negate m)

filterValue :: (AssetId -> Bool) -> Value -> Value
filterValue p (Value m) = Value (Map.filterWithKey (\k _v -> p k) m)

selectLovelace :: Value -> Lovelace
selectLovelace = quantityToLovelace . flip selectAsset AdaAssetId

lovelaceToValue :: Lovelace -> Value
lovelaceToValue = Value . Map.singleton AdaAssetId . lovelaceToQuantity


toMaryValue :: forall ledgerera.
               Ledger.Crypto ledgerera ~ StandardCrypto
            => Value -> Mary.Value ledgerera
toMaryValue v =
    Mary.Value lovelace other
  where
    Quantity lovelace = selectAsset v AdaAssetId
      --TODO: write QC tests to show it's ok to use Map.fromAscListWith here
    other = Map.fromListWith Map.union
              [ (toMaryPolicyID pid, Map.singleton (toMaryAssetName name) q)
              | (AssetId pid name, Quantity q) <- valueToList v ]

    toMaryPolicyID :: PolicyId -> Mary.PolicyID ledgerera
    toMaryPolicyID (PolicyId sh) = Mary.PolicyID (toShelleyScriptHash sh)

    toMaryAssetName :: AssetName -> Mary.AssetName
    toMaryAssetName (AssetName n) = Mary.AssetName n


fromMaryValue :: forall ledgerera.
                 Ledger.Crypto ledgerera ~ StandardCrypto
              => Mary.Value ledgerera -> Value
fromMaryValue (Mary.Value lovelace other) =
    Value $
      --TODO: write QC tests to show it's ok to use Map.fromAscList here
      Map.fromList $
        [ (AdaAssetId, Quantity lovelace) | lovelace /= 0 ]
     ++ [ (AssetId (fromMaryPolicyID pid) (fromMaryAssetName name), Quantity q)
        | (pid, as) <- Map.toList other
        , (name, q) <- Map.toList as ]
  where
    fromMaryPolicyID :: Mary.PolicyID ledgerera -> PolicyId
    fromMaryPolicyID (Mary.PolicyID sh) = PolicyId (fromShelleyScriptHash sh)

    fromMaryAssetName :: Mary.AssetName -> AssetName
    fromMaryAssetName (Mary.AssetName n) = AssetName n


-- ----------------------------------------------------------------------------
-- Era-dependent use of multi-assert values
--

data MintValue era where

     MintNothing :: MintValue era

     MintValue   :: MultiAssetInEra era -> Value -> MintValue era

deriving instance Eq   (MintValue era)
deriving instance Show (MintValue era)


data TxOutValue era where

     TxOutAdaOnly :: AdaOnlyInEra era -> Lovelace -> TxOutValue era

     TxOutValue   :: MultiAssetInEra era -> Value -> TxOutValue era

deriving instance Eq   (TxOutValue era)
deriving instance Show (TxOutValue era)


-- | Representation of whether only ada transactions are supported in a
-- particular era.
--
data AdaOnlyInEra era where

     AdaOnlyInByronEra   :: AdaOnlyInEra ByronEra
     AdaOnlyInShelleyEra :: AdaOnlyInEra ShelleyEra
     AdaOnlyInAllegraEra :: AdaOnlyInEra AllegraEra

deriving instance Eq   (AdaOnlyInEra era)
deriving instance Show (AdaOnlyInEra era)

-- | Representation of whether multi-asset transactions are supported in a
-- particular era.
--
data MultiAssetInEra era where

     -- | Multi-asset transactions are supported in the 'Mary' era.
     MultiAssetInMaryEra :: MultiAssetInEra MaryEra

deriving instance Eq   (MultiAssetInEra era)
deriving instance Show (MultiAssetInEra era)

