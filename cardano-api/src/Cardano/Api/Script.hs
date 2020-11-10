{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Api.Script (
    Script(SimpleScript, ShelleyScript, AllegraScript)
  , parseScript
  , parseScriptAny
  , parseScriptAll
  , parseScriptAtLeast
  , parseScriptSig
  , scriptHash
  , SimpleScript(..)
  , ScriptFeatureInEra(..)
  , SignatureFeature
  , TimeLocksFeature

    -- * Deprecated aliases
  , MultiSigScript
  , makeMultiSigScript

    -- * Data family instances
  , AsType(..)
  , Hash(ScriptHash)
  ) where

import           Prelude

import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (toList)
import           Data.Scientific (toBoundedInteger)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Data.Aeson (Value (..), object, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Sequence.Strict as Sequence
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Control.Applicative
import           Control.Monad

import qualified Cardano.Binary as CBOR

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Slotting.Slot (SlotNo)

import qualified Cardano.Ledger.Core as Shelley

import qualified Cardano.Ledger.ShelleyMA.Timelocks as Timelock
import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardShelley)
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Scripts as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley

import           Cardano.Api.Eras (Allegra, AsType (AsAllegra, AsByron, AsMary, AsShelley), Mary,
                     Shelley)
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.KeysShelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope

import qualified Cardano.Api.Shelley.Serialisation.Legacy as Legacy

{- HLINT ignore "Use section" -}


-- ----------------------------------------------------------------------------
-- Script type: covering all script languages
--

data Script era where

     ShelleyScript :: Shelley.Script StandardShelley -> Script Shelley
     AllegraScript :: Timelock.Timelock StandardAllegra -> Script Allegra

deriving stock instance (Eq (Script Shelley))
deriving stock instance (Show (Script Shelley))

deriving stock instance (Eq (Script Allegra))
deriving stock instance (Show (Script Allegra))

pattern SimpleScript :: HasScriptFeatures era
                     => SimpleScript era -> Script era
pattern SimpleScript s <- (scriptToSimpleScript -> s) where
    SimpleScript = simpleScriptToScript

{-# COMPLETE SimpleScript #-}

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

instance SerialiseAsCBOR (Script Allegra) where
    serialiseToCBOR (AllegraScript s) = CBOR.serialize' s
    deserialiseFromCBOR (AsScript AsAllegra) bs =
        AllegraScript <$> CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

instance HasTextEnvelope (Script Allegra) where
    textEnvelopeType _ = "Script"
    textEnvelopeDefaultDescr (AllegraScript _) = "Multi-signature script"

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
scriptHash (AllegraScript s) = ScriptHash (Timelock.hashTimelockScript s)


-- ----------------------------------------------------------------------------
-- The simple native script language
--

type MultiSigScript era = SimpleScript era

data SimpleScript era where

     RequireSignature  :: !(ScriptFeatureInEra SignatureFeature era)
                       -> !(Hash PaymentKey)
                       -> SimpleScript era

     RequireTimeBefore :: !(ScriptFeatureInEra TimeLocksFeature era)
                       -> !SlotNo
                       -> SimpleScript era

     RequireTimeAfter  :: !(ScriptFeatureInEra TimeLocksFeature era)
                       -> !SlotNo
                       -> SimpleScript era

     RequireAllOf      ::        [SimpleScript era] -> SimpleScript era
     RequireAnyOf      ::        [SimpleScript era] -> SimpleScript era
     RequireMOf        :: Int -> [SimpleScript era] -> SimpleScript era

deriving instance Eq   (SimpleScript era)
deriving instance Show (SimpleScript era)


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
-- available in the 'SimpleScript' language from 'Shelley' era onwards.
--
data SignatureFeature

-- | The time lock feature makes it possible to make the script result depend
-- on the slot number which is a proxy for the time. Is available in the
-- 'SimpleScript' language from 'Allegra' onwards.
--
data TimeLocksFeature

-- | Is the 'SimpleScript' language supported at all in this era?
--
data SimpleScriptSupportedInEra era where
     SimpleScriptInShelleyEra :: SimpleScriptSupportedInEra Shelley
     SimpleScriptInAllegraEra :: SimpleScriptSupportedInEra Allegra
     SimpleScriptInMaryEra    :: SimpleScriptSupportedInEra Mary

class HasScriptFeatures era where
   simpleScriptSupported :: SimpleScriptSupportedInEra era
   hasSignatureFeature   :: Maybe (ScriptFeatureInEra SignatureFeature era)
   hasTimeLocksFeature   :: Maybe (ScriptFeatureInEra TimeLocksFeature era)

instance HasScriptFeatures Shelley where
   simpleScriptSupported = SimpleScriptInShelleyEra
   hasSignatureFeature   = Just SignaturesInShelleyEra
   hasTimeLocksFeature   = Nothing

instance HasScriptFeatures Allegra where
   simpleScriptSupported = SimpleScriptInAllegraEra
   hasSignatureFeature   = Just SignaturesInAllegraEra
   hasTimeLocksFeature   = Just TimeLocksInAllegraEra

instance HasScriptFeatures Mary where
   simpleScriptSupported = SimpleScriptInMaryEra
   hasSignatureFeature   = Just SignaturesInMaryEra
   hasTimeLocksFeature   = Just TimeLocksInMaryEra


--TODO: add a deprecation pragma and switch to the SimpleScript constructor
makeMultiSigScript :: MultiSigScript Shelley -> Script Shelley
makeMultiSigScript = simpleScriptToScript

simpleScriptToScript :: forall era. HasScriptFeatures era
                     => SimpleScript era -> Script era
simpleScriptToScript =
    case simpleScriptSupported :: SimpleScriptSupportedInEra era of
      SimpleScriptInShelleyEra -> ShelleyScript . go
        where
          go :: SimpleScript Shelley -> Shelley.MultiSig StandardShelley
          go (RequireSignature _ (PaymentKeyHash kh))
                              = Shelley.RequireSignature (Shelley.coerceKeyRole kh)
          go (RequireAllOf s) = Shelley.RequireAllOf (map go s)
          go (RequireAnyOf s) = Shelley.RequireAnyOf (map go s)
          go (RequireMOf m s) = Shelley.RequireMOf m (map go s)

      SimpleScriptInAllegraEra -> AllegraScript . go
        where
          go :: MultiSigScript Allegra -> Timelock.Timelock StandardAllegra
          go (RequireSignature _ (PaymentKeyHash kh))
                              = Timelock.Multi (Shelley.RequireSignature (Shelley.coerceKeyRole kh))
          go (RequireAllOf s) = Timelock.TimelockAnd  (fmap go $ Sequence.fromList s)
          go (RequireAnyOf s) = Timelock.TimelockOr (fmap go $ Sequence.fromList s)
          go (RequireMOf _m _s) = error "To fill in when ledger specs is updated in cabal project file"
          go (RequireTimeBefore _ sBefore) =
            Timelock.Interval $ Timelock.ValidityInterval { Timelock.validFrom = SNothing
                                                          , Timelock.validTo = SJust sBefore
                                                          }
          go (RequireTimeAfter _ sAfter) =
            Timelock.Interval $ Timelock.ValidityInterval { Timelock.validFrom = SJust sAfter
                                                          , Timelock.validTo = SNothing
                                                          }
      SimpleScriptInMaryEra    -> error "TODO: simpleScriptToScript conversion for Mary era"

scriptToSimpleScript :: Script era -> SimpleScript era
scriptToSimpleScript (ShelleyScript s0) = go s0
  where
    go :: Shelley.MultiSig StandardShelley -> SimpleScript Shelley
    go (Shelley.RequireSignature kh)
                                = RequireSignature SignaturesInShelleyEra
                                    (PaymentKeyHash (Shelley.coerceKeyRole kh))
    go (Shelley.RequireAllOf s) = RequireAllOf (map go s)
    go (Shelley.RequireAnyOf s) = RequireAnyOf (map go s)
    go (Shelley.RequireMOf m s) = RequireMOf m (map go s)

scriptToSimpleScript (AllegraScript s0) = go s0
  where
    go :: Timelock.Timelock StandardAllegra -> SimpleScript Allegra
    go (Timelock.Multi (Shelley.RequireSignature kh))
                   = RequireSignature SignaturesInAllegraEra
                       (PaymentKeyHash (Shelley.coerceKeyRole kh))
    go (Timelock.Multi (Shelley.RequireAllOf s)) = RequireAllOf $ map ms s
    go (Timelock.Multi (Shelley.RequireAnyOf s)) = RequireAnyOf $ map ms s
    go (Timelock.Multi (Shelley.RequireMOf i s)) = RequireMOf i $ map ms s
    go (Timelock.TimelockAnd seq') = RequireAllOf . map go $ toList seq'
    go (Timelock.TimelockOr seq') = RequireAnyOf . map go $ toList seq'
    go (Timelock.Interval (Timelock.ValidityInterval before after)) =
      case (before, after) of
        (SJust b, SNothing) ->  RequireTimeBefore TimeLocksInAllegraEra b
        (SNothing, SJust a) ->  RequireTimeAfter TimeLocksInAllegraEra a
        _ -> error "Cardano.Api.Script.scriptToSimpleScript: Upper and lower\
                   \ bound has been specified in a given ValidityInterval."

    ms :: Shelley.MultiSig StandardAllegra -> SimpleScript Allegra
    ms (Shelley.RequireSignature kh)
                                = RequireSignature SignaturesInAllegraEra
                                    (PaymentKeyHash (Shelley.coerceKeyRole kh))
    ms (Shelley.RequireAllOf s) = RequireAllOf (map ms s)
    ms (Shelley.RequireAnyOf s) = RequireAnyOf (map ms s)
    ms (Shelley.RequireMOf m s) = RequireMOf m (map ms s)


--
-- JSON serialisation
--

instance ToJSON (SimpleScript era) where
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

instance HasScriptFeatures era => FromJSON (SimpleScript era) where
  parseJSON = parseScript

parseScript :: HasScriptFeatures era
            => Value -> Aeson.Parser (SimpleScript era)
parseScript v = maybe mempty (flip parseScriptSig    v) hasSignatureFeature
            <|> maybe mempty (flip parseScriptBefore v) hasTimeLocksFeature
            <|> maybe mempty (flip parseScriptAfter  v) hasTimeLocksFeature
            <|> parseScriptAny v
            <|> parseScriptAll v
            <|> parseScriptAtLeast v

parseScriptAny :: HasScriptFeatures era
               => Value -> Aeson.Parser (SimpleScript era)
parseScriptAny = Aeson.withObject "any" $ \obj -> do
  t <- obj .: "type"
  case t :: Text of
    "any" -> do s <- obj .: "scripts"
                RequireAnyOf <$> gatherSimpleScriptTerms s
    _ -> fail "\"any\" script value not found"

parseScriptAll :: HasScriptFeatures era
               => Value -> Aeson.Parser (SimpleScript era)
parseScriptAll = Aeson.withObject "all" $ \obj -> do
  t <- obj .: "type"
  case t :: Text of
    "all" -> do s <- obj .: "scripts"
                RequireAllOf <$> gatherSimpleScriptTerms s
    _ -> fail "\"all\" script value not found"

parseScriptAtLeast :: HasScriptFeatures era
                   => Value -> Aeson.Parser (SimpleScript era)
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
              do scripts <- gatherSimpleScriptTerms s
                 let numScripts = length scripts
                 when
                   (reqInt > numScripts)
                   (fail $ "Required number of script signatures exceeds the number of scripts."
                         <> " Required number: " <> show reqInt
                         <> " Number of scripts: " <> show numScripts)
                 return $ RequireMOf reqInt scripts
            Nothing -> fail $ "Error in \"required\" key: "
                            <> show sci <> " is not a valid Int"
        _ -> fail "\"required\" value should be an integer"
    _        -> fail "\"atLeast\" script value not found"

parseScriptSig :: ScriptFeatureInEra SignatureFeature era
               -> Value -> Aeson.Parser (SimpleScript era)
parseScriptSig signaturesInEra = Aeson.withObject "sig" $ \obj -> do
  v <- obj .: "type"
  case v :: Text of
    "sig" -> do k <- obj .: "keyHash"
                RequireSignature signaturesInEra <$> convertToHash k
    _     -> fail "\"sig\" script value not found"


parseScriptBefore :: ScriptFeatureInEra TimeLocksFeature era
                  -> Value -> Aeson.Parser (SimpleScript era)
parseScriptBefore timelocksInEra = Aeson.withObject "before" $ \obj -> do
  v <- obj .: "type"
  case v :: Text of
    "before" -> RequireTimeBefore timelocksInEra <$> obj .: "slot"
    _        -> fail "\"before\" script value not found"

parseScriptAfter :: ScriptFeatureInEra TimeLocksFeature era
                 -> Value -> Aeson.Parser (SimpleScript era)
parseScriptAfter timelocksInEra = Aeson.withObject "after" $ \obj -> do
  v <- obj .: "type"
  case v :: Text of
    "after" -> RequireTimeAfter timelocksInEra <$> obj .: "slot"
    _       -> fail "\"after\" script value not found"

convertToHash :: Text -> Aeson.Parser (Hash PaymentKey)
convertToHash txt = case deserialiseFromRawBytesHex (AsHash AsPaymentKey) $ Text.encodeUtf8 txt of
                      Just payKeyHash -> return payKeyHash
                      Nothing -> fail $ "Error deserialising payment key hash: " <> Text.unpack txt

gatherSimpleScriptTerms :: HasScriptFeatures era
                        => Vector Value -> Aeson.Parser [SimpleScript era]
gatherSimpleScriptTerms = sequence . Vector.toList . Vector.map parseScript

