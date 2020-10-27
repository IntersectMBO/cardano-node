{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Key
  ( AsType (AsPaymentKey, AsHash)

    -- ** Hashes
    -- | In Cardano most keys are identified by their hash, and hashes are
    -- used in many other places.
  , Hash(PaymentKeyHash)

  , PaymentKey
  , PaymentExtendedKey
  , GenesisKey
  , GenesisUTxOKey
  , GenesisDelegateKey
  , StakeKey
  , StakePoolKey
  ) where

import           Cardano.Prelude

import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import qualified Shelley.Spec.Ledger.Keys as Shelley

import           Cardano.Api.HasTypeProxy (HasTypeProxy (..))
import           Cardano.Api.Serialisation
import qualified Cardano.Crypto.Hash.Class as Crypto

data family Hash keyrole :: Type

instance HasTypeProxy a => HasTypeProxy (Hash a) where
    data AsType (Hash a) = AsHash (AsType a)
    proxyToAsType _ = AsHash (proxyToAsType (Proxy :: Proxy a))

newtype instance Hash PaymentKey =
    PaymentKeyHash (Shelley.KeyHash Shelley.Payment StandardCrypto)
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash PaymentKey) where
    serialiseToRawBytes (PaymentKeyHash (Shelley.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsPaymentKey) bs =
      PaymentKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

-- | Map the various Shelley key role types into corresponding 'Shelley.KeyRole'
-- types.
--
type family ShelleyKeyRole (keyrole :: Type) :: Shelley.KeyRole

data PaymentKey
data PaymentExtendedKey

instance HasTypeProxy PaymentKey where
    data AsType PaymentKey = AsPaymentKey
    proxyToAsType _ = AsPaymentKey

data GenesisKey
data GenesisUTxOKey
data GenesisDelegateKey
data StakeKey
data StakePoolKey

type instance ShelleyKeyRole PaymentKey         = Shelley.Payment
type instance ShelleyKeyRole GenesisKey         = Shelley.Genesis
type instance ShelleyKeyRole GenesisUTxOKey     = Shelley.Payment
type instance ShelleyKeyRole GenesisDelegateKey = Shelley.GenesisDelegate
type instance ShelleyKeyRole StakeKey           = Shelley.Staking
type instance ShelleyKeyRole StakePoolKey       = Shelley.StakePool

