{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


-- | Transaction bodies
--
module Cardano.Api.TxIn (
    -- * Transaction inputs
    TxIn(..),
    TxIx(..),

    -- * Transaction Ids
    TxId(..),
    parseTxId,

    -- * Data family instances
    AsType(AsTxId),

    -- * Internal conversion functions
    toByronTxId,
    toShelleyTxId,
    fromShelleyTxId,
    toByronTxIn,
    fromByronTxIn,
    toShelleyTxIn,
    fromShelleyTxIn,
    renderTxIn,
  ) where

import           Prelude

import           Control.Applicative (some)
import           Data.Aeson (withText)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)

import qualified Data.ByteString.Char8 as BSC
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Parsec ((<?>))
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.Parsec.Token as Parsec

import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hashing as Byron

import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Shelley
import qualified Cardano.Ledger.SafeHash as SafeHash

import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.TxIn as Ledger

import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseUsing
import           Cardano.Api.Utils

{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use section" -}

-- ----------------------------------------------------------------------------
-- Transaction Ids
--

newtype TxId = TxId (Shelley.Hash StandardCrypto Shelley.EraIndependentTxBody)
  -- We use the Shelley representation and convert to/from the Byron one
  deriving stock (Eq, Ord)
  deriving (Show, IsString)         via UsingRawBytesHex TxId
  deriving (ToJSON, FromJSON)       via UsingRawBytesHex TxId
  deriving (ToJSONKey, FromJSONKey) via UsingRawBytesHex TxId

instance HasTypeProxy TxId where
    data AsType TxId = AsTxId
    proxyToAsType _ = AsTxId

instance SerialiseAsRawBytes TxId where
    serialiseToRawBytes (TxId h) = Crypto.hashToBytes h
    deserialiseFromRawBytes AsTxId bs = TxId <$> Crypto.hashFromBytes bs

toByronTxId :: TxId -> Byron.TxId
toByronTxId (TxId h) =
    Byron.unsafeHashFromBytes (Crypto.hashToBytes h)

toShelleyTxId :: TxId -> Ledger.TxId StandardCrypto
toShelleyTxId (TxId h) =
    Ledger.TxId (SafeHash.unsafeMakeSafeHash (Crypto.castHash h))

fromShelleyTxId :: Ledger.TxId StandardCrypto -> TxId
fromShelleyTxId (Ledger.TxId h) =
    TxId (Crypto.castHash (SafeHash.extractHash h))


-- ----------------------------------------------------------------------------
-- Transaction inputs
--

data TxIn = TxIn TxId TxIx
  deriving (Eq, Ord, Show)

instance ToJSON TxIn where
  toJSON txIn = Aeson.String $ renderTxIn txIn

instance ToJSONKey TxIn where
  toJSONKey = toJSONKeyText renderTxIn

instance FromJSON TxIn where
  parseJSON = withText "TxIn" $ runParsecParser parseTxIn

instance FromJSONKey TxIn where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ runParsecParser parseTxIn

parseTxId :: Parsec.Parser TxId
parseTxId = do
  str <- some Parsec.hexDigit <?> "transaction id (hexadecimal)"
  failEitherWith
    (\e -> "Incorrect transaction id format: " ++ displayError e) $
    deserialiseFromRawBytesHex AsTxId $ BSC.pack str

parseTxIn :: Parsec.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (Parsec.char '#' *> parseTxIx)

parseTxIx :: Parsec.Parser TxIx
parseTxIx = TxIx . fromIntegral <$> decimal

decimal :: Parsec.Parser Integer
Parsec.TokenParser { Parsec.decimal = decimal } = Parsec.haskell


renderTxIn :: TxIn -> Text
renderTxIn (TxIn txId (TxIx ix)) =
  serialiseToRawBytesHexText txId <> "#" <> Text.pack (show ix)


newtype TxIx = TxIx Word
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)
  deriving newtype (ToJSON, FromJSON)

fromByronTxIn :: Byron.TxIn -> TxIn
fromByronTxIn (Byron.TxInUtxo txId index) =
  let shortBs = Byron.abstractHashToShort txId
      mApiHash = Crypto.hashFromBytesShort shortBs
  in case mApiHash of
       Just apiHash -> TxIn (TxId apiHash) (TxIx . fromIntegral $ toInteger index)
       Nothing -> error $ "Error converting Byron era TxId: " <> show txId

toByronTxIn :: TxIn -> Byron.TxIn
toByronTxIn (TxIn txid (TxIx txix)) =
    Byron.TxInUtxo (toByronTxId txid) (fromIntegral txix)

-- | This function may overflow on the transaction index. Call sites must ensure
-- that all uses of this function are appropriately guarded.
toShelleyTxIn :: TxIn -> Ledger.TxIn StandardCrypto
toShelleyTxIn (TxIn txid (TxIx txix)) =
    Ledger.TxIn (toShelleyTxId txid) (Ledger.TxIx $ fromIntegral txix)

fromShelleyTxIn :: Ledger.TxIn StandardCrypto -> TxIn
fromShelleyTxIn (Ledger.TxIn txid (Ledger.TxIx txix)) =
    TxIn (fromShelleyTxId txid) (TxIx (fromIntegral txix))
