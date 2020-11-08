{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Api.Script (
    Script(..)
  , parseScript
  , parseScriptAny
  , parseScriptAll
  , parseScriptAtLeast
  , parseScriptSig
  , scriptHash
  , MultiSigScript(..)
  , ScriptFeatureInEra(..)
  , SignatureFeature
  , TimeLocksFeature
  , makeMultiSigScript

    -- * Data family instances
  , AsType(..)
  , Hash(ScriptHash)
  ) where

import           Prelude

import           Data.Scientific (toBoundedInteger)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS

import           Data.Aeson (Value (..), object, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Control.Applicative
import           Control.Monad

import qualified Cardano.Binary as CBOR

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Slotting.Slot (SlotNo)

import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import qualified Cardano.Ledger.Core as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Scripts as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley

import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.KeysShelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope

import qualified Cardano.Api.Shelley.Serialisation.Legacy as Legacy


-- ----------------------------------------------------------------------------
-- Script type: covering all script languages
--

data Script era where

     ShelleyScript :: Shelley.Script StandardShelley -> Script Shelley

deriving stock instance (Eq (Script Shelley))
deriving stock instance (Show (Script Shelley))

instance HasTypeProxy era => HasTypeProxy (Script era) where
    data AsType (Script era) = AsScript (AsType era)
    proxyToAsType _ = AsScript (proxyToAsType (Proxy :: Proxy era))

instance SerialiseAsCBOR (Script Shelley) where
    serialiseToCBOR (ShelleyScript s) =
      -- We use 'WrappedMultiSig' here to support the legacy binary
      -- serialisation format for the @Script@ type from
      -- @cardano-ledger-specs@.
      --
      -- See the documentation of 'WrappedMultiSig' for more information.
      CBOR.serialize' (Legacy.WrappedMultiSig s)

    deserialiseFromCBOR (AsScript AsShelley) bs =
      -- We use 'WrappedMultiSig' here to support the legacy binary
      -- serialisation format for the @Script@ type from
      -- @cardano-ledger-specs@.
      --
      -- See the documentation of 'WrappedMultiSig' for more information.
      ShelleyScript . Legacy.unWrappedMultiSig <$>
        CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

instance HasTextEnvelope (Script Shelley) where
    textEnvelopeType _ = "Script"
    textEnvelopeDefaultDescr (ShelleyScript _) = "Multi-signature script"


-- ----------------------------------------------------------------------------
-- Script Hash
--

newtype instance Hash (Script era) = ScriptHash (Shelley.ScriptHash StandardShelley)
  deriving (Eq, Ord, Show)

instance HasTypeProxy era => SerialiseAsRawBytes (Hash (Script era)) where
    serialiseToRawBytes (ScriptHash (Shelley.ScriptHash h)) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes (AsHash (AsScript _)) bs =
      ScriptHash . Shelley.ScriptHash <$> Crypto.hashFromBytes bs

scriptHash :: Script era -> Hash (Script era)
scriptHash (ShelleyScript s) = ScriptHash (Shelley.hashScript s)


-- ----------------------------------------------------------------------------
-- The multi-signature script language
--

data MultiSigScript era where

     RequireSignature  :: !(ScriptFeatureInEra SignatureFeature era)
                       -> !(Hash PaymentKey)
                       -> MultiSigScript era

     RequireTimeBefore :: !(ScriptFeatureInEra TimeLocksFeature era)
                       -> !SlotNo
                       -> MultiSigScript era

     RequireTimeAfter  :: !(ScriptFeatureInEra TimeLocksFeature era)
                       -> !SlotNo
                       -> MultiSigScript era

     RequireAllOf      ::        [MultiSigScript era] -> MultiSigScript era
     RequireAnyOf      ::        [MultiSigScript era] -> MultiSigScript era
     RequireMOf        :: Int -> [MultiSigScript era] -> MultiSigScript era

deriving instance Eq   (MultiSigScript era)
deriving instance Show (MultiSigScript era)


-- | Script Features
--
-- These are used in conjunction with the era (e.g 'Shelley', 'Allegra' etc) to
-- specify which script features are enabled in a given era.
--
data ScriptFeatureInEra feature era where
     SignaturesInShelleyEra  :: ScriptFeatureInEra SignatureFeature Shelley
     SignaturesInAllegraEra  :: ScriptFeatureInEra SignatureFeature Allegra
     SignaturesInMaryEra     :: ScriptFeatureInEra SignatureFeature Mary

     TimeLocksInAllegraEra   :: ScriptFeatureInEra TimeLocksFeature Allegra
     TimeLocksInMaryEra      :: ScriptFeatureInEra TimeLocksFeature Mary

deriving instance Eq   (ScriptFeatureInEra feature era)
deriving instance Show (ScriptFeatureInEra feature era)

-- | The signature feature enables the use of 'RequireSignature' and is
-- available in the 'MultiSigScript' language from 'Shelley' era onwards.
--
data SignatureFeature

-- | The time lock feature makes it possible to make the script result depend
-- on the slot number which is a proxy for the time. Is available in the
-- 'MultiSigScript' language from 'Allegra' onwards.
--
data TimeLocksFeature


makeMultiSigScript :: MultiSigScript Shelley -> Script Shelley
makeMultiSigScript = ShelleyScript . go
  where
    go :: MultiSigScript Shelley -> Shelley.MultiSig StandardShelley
    go (RequireSignature _ (PaymentKeyHash kh))
                        = Shelley.RequireSignature (Shelley.coerceKeyRole kh)
    go (RequireAllOf s) = Shelley.RequireAllOf (map go s)
    go (RequireAnyOf s) = Shelley.RequireAnyOf (map go s)
    go (RequireMOf m s) = Shelley.RequireMOf m (map go s)


--
-- JSON serialisation
--

instance ToJSON (MultiSigScript era) where
  toJSON (RequireSignature _ pKeyHash) =
    object [ "type"    .= String "sig"
           , "keyHash" .= Text.decodeUtf8 (serialiseToRawBytesHex pKeyHash)
           ]
  toJSON (RequireTimeBefore _ slot) =
    object [ "type" .= String "before"
           , "slot" .= slot
           ]
  toJSON (RequireTimeAfter _ slot) =
    object [ "type" .= String "after"
           , "slot" .= slot
           ]
  toJSON (RequireAnyOf reqScripts) =
    object [ "type" .= String "any", "scripts" .= map toJSON reqScripts ]
  toJSON (RequireAllOf reqScripts) =
    object [ "type" .= String "all", "scripts" .= map toJSON reqScripts ]
  toJSON (RequireMOf reqNum reqScripts) =
    object [ "type" .= String "atLeast"
           , "required" .= reqNum
           , "scripts" .= map toJSON reqScripts
           ]

instance FromJSON (MultiSigScript Shelley) where
  parseJSON = parseScript

parseScript :: Value -> Aeson.Parser (MultiSigScript Shelley)
parseScript v = parseScriptSig v
                  <|> parseScriptAny v
                  <|> parseScriptAll v
                  <|> parseScriptAtLeast v

parseScriptAny :: Value -> Aeson.Parser (MultiSigScript Shelley)
parseScriptAny = Aeson.withObject "any" $ \obj -> do
  t <- obj .: "type"
  case t :: Text of
    "any" -> do s <- obj .: "scripts"
                RequireAnyOf <$> gatherMultiSigScripts s
    _ -> fail "\"any\" multi-signature script value not found"

parseScriptAll :: Value -> Aeson.Parser (MultiSigScript Shelley)
parseScriptAll = Aeson.withObject "all" $ \obj -> do
  t <- obj .: "type"
  case t :: Text of
    "all" -> do s <- obj .: "scripts"
                RequireAllOf <$> gatherMultiSigScripts s
    _ -> fail "\"all\" multi-signature script value not found"

parseScriptAtLeast :: Value -> Aeson.Parser (MultiSigScript Shelley)
parseScriptAtLeast = Aeson.withObject "atLeast" $ \obj -> do
  v <- obj .: "type"
  case v :: Text of
    "atLeast" -> do
      r <- obj .: "required"
      s <- obj .: "scripts"
      case r of
        Number sci ->
          case toBoundedInteger sci of
            Just reqInt ->
              do msigscripts <- gatherMultiSigScripts s
                 let numScripts = length msigscripts
                 when
                   (reqInt > numScripts)
                   (fail $ "Required number of script signatures exceeds the number of scripts."
                         <> " Required number: " <> show reqInt
                         <> " Number of scripts: " <> show numScripts)
                 return $ RequireMOf reqInt msigscripts
            Nothing -> fail $ "Error in multi-signature \"required\" key: "
                            <> show sci <> " is not a valid Int"
        _ -> fail "\"required\" value should be an integer"
    _        -> fail "\"atLeast\" multi-signature script value not found"

parseScriptSig :: Value -> Aeson.Parser (MultiSigScript Shelley)
parseScriptSig = Aeson.withObject "sig" $ \obj -> do
  v <- obj .: "type"
  case v :: Text of
    "sig" -> do k <- obj .: "keyHash"
                RequireSignature SignaturesInShelleyEra <$> convertToHash k
    _     -> fail "\"sig\" multi-signature script value not found"

convertToHash :: Text -> Aeson.Parser (Hash PaymentKey)
convertToHash txt = case deserialiseFromRawBytesHex (AsHash AsPaymentKey) $ Text.encodeUtf8 txt of
                      Just payKeyHash -> return payKeyHash
                      Nothing -> fail $ "Error deserialising payment key hash: " <> Text.unpack txt

gatherMultiSigScripts :: Vector Value -> Aeson.Parser [MultiSigScript Shelley]
gatherMultiSigScripts vs = sequence . Vector.toList $ Vector.map parseScript vs

