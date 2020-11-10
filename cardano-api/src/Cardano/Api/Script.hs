{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
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
    Script
      ( SimpleScript
      , ShelleyScript
      , AllegraScript
      , MaryScript
      )
  , ScriptHash(..)
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
  ) where

import           Prelude

import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (toList)
import           Data.Scientific (toBoundedInteger)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Data.Aeson (Value (..), object, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Sequence.Strict as Seq
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Control.Applicative
import           Control.Monad

import qualified Cardano.Binary as CBOR

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Slotting.Slot (SlotNo)

import qualified Cardano.Ledger.Core as Shelley
import qualified Cardano.Ledger.Era  as Ledger

import qualified Cardano.Ledger.ShelleyMA.Timelocks as Timelock
import           Ouroboros.Consensus.Shelley.Eras
                   (StandardAllegra, StandardMary, StandardShelley,
                    StandardCrypto)
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Scripts as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley

import           Cardano.Api.Eras
                   (Shelley, Allegra, Mary,
                    AsType (AsByron, AsShelley, AsAllegra, AsMary))
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
     MaryScript    :: Timelock.Timelock StandardMary -> Script Mary

deriving stock instance (Eq (Script Shelley))
deriving stock instance (Show (Script Shelley))

deriving stock instance (Eq (Script Allegra))
deriving stock instance (Show (Script Allegra))

deriving stock instance (Eq (Script Mary))
deriving stock instance (Show (Script Mary))

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

instance SerialiseAsCBOR (Script Mary) where
    serialiseToCBOR (MaryScript s) = CBOR.serialize' s
    deserialiseFromCBOR (AsScript AsMary) bs =
        MaryScript <$> CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

instance HasTextEnvelope (Script Mary) where
    textEnvelopeType _ = "Script"
    textEnvelopeDefaultDescr (MaryScript _) = "Multi-signature script"

-- ----------------------------------------------------------------------------
-- Script Hash
--

-- | We have this type separate from the 'Hash' type to avoid the script
-- hash type being parametrised by the era. The representation is era
-- independent, and there are many places where we want to use a script
-- hash where we don't want things to be era-parametrised.
--
newtype ScriptHash = ScriptHash (Shelley.ScriptHash StandardShelley)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex ScriptHash

instance HasTypeProxy ScriptHash where
    data AsType ScriptHash = AsScriptHash
    proxyToAsType _ = AsScriptHash

instance SerialiseAsRawBytes ScriptHash where
    serialiseToRawBytes (ScriptHash (Shelley.ScriptHash h)) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes AsScriptHash bs =
      ScriptHash . Shelley.ScriptHash <$> Crypto.hashFromBytes bs

scriptHash :: Script era -> ScriptHash
scriptHash (ShelleyScript s) = ScriptHash (Shelley.hashScript s)
-- We're using a single monomorphic type for the script hash, and
-- arbitrarily picked the Shelley one, so we have to convert the hashes
-- from the other eras.
scriptHash (AllegraScript s) = ScriptHash
                             . (\(Shelley.ScriptHash sh) ->
                                   Shelley.ScriptHash (Crypto.castHash sh))
                             $ Timelock.hashTimelockScript s
scriptHash (MaryScript s)    = ScriptHash
                             . (\(Shelley.ScriptHash sh) ->
                                   Shelley.ScriptHash (Crypto.castHash sh))
                             $ Timelock.hashTimelockScript s


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

      SimpleScriptInAllegraEra -> AllegraScript . simpleScriptToTimelock
      SimpleScriptInMaryEra    -> MaryScript    . simpleScriptToTimelock

-- | Conversion for the 'Timelock.Timelock' language that is shared between the
-- Allegra and Mary eras.
--
simpleScriptToTimelock :: (Ledger.Era ledgerera,
                           Ledger.Crypto ledgerera ~ StandardCrypto)
                       => SimpleScript era -> Timelock.Timelock ledgerera
simpleScriptToTimelock = go
  where
    go (RequireSignature _ (PaymentKeyHash kh))
                        = Timelock.RequireSignature (Shelley.coerceKeyRole kh)
    go (RequireAllOf s) = Timelock.RequireAllOf (Seq.fromList (map go s))
    go (RequireAnyOf s) = Timelock.RequireAnyOf (Seq.fromList (map go s))
    go (RequireMOf m s) = Timelock.RequireMOf m (Seq.fromList (map go s))
    go (RequireTimeBefore _ sl) = Timelock.RequireTimeExpire (SJust sl)
    go (RequireTimeAfter  _ sl) = Timelock.RequireTimeStart  (SJust sl)


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

scriptToSimpleScript (AllegraScript s) = timelockToSimpleScript
                                           SignaturesInAllegraEra
                                           TimeLocksInAllegraEra s
scriptToSimpleScript (MaryScript    s) = timelockToSimpleScript
                                           SignaturesInMaryEra
                                           TimeLocksInMaryEra s

-- | Conversion for the 'Timelock.Timelock' language that is shared between the
-- Allegra and Mary eras.
--
timelockToSimpleScript :: forall ledgerera era.
                          (Ledger.Era ledgerera,
                           Ledger.Crypto ledgerera ~ StandardCrypto)
                       => ScriptFeatureInEra SignatureFeature era
                       -> ScriptFeatureInEra TimeLocksFeature era
                       -> Timelock.Timelock ledgerera -> SimpleScript era
timelockToSimpleScript signaturesInEra timeLocksInEra = go
  where
    go :: Timelock.Timelock ledgerera -> SimpleScript era
    go (Timelock.RequireSignature kh) = RequireSignature signaturesInEra
                                          (PaymentKeyHash (Shelley.coerceKeyRole kh))
    go (Timelock.RequireTimeExpire (SJust sl)) = RequireTimeBefore timeLocksInEra sl
    go (Timelock.RequireTimeStart  (SJust sl)) = RequireTimeAfter  timeLocksInEra sl
    go (Timelock.RequireAllOf      s) = RequireAllOf (map go (toList s))
    go (Timelock.RequireAnyOf      s) = RequireAnyOf (map go (toList s))
    go (Timelock.RequireMOf      i s) = RequireMOf i (map go (toList s))

    go (Timelock.RequireTimeExpire SNothing) = error "TODO: review need for this case with ledger team"
    go (Timelock.RequireTimeStart  SNothing) = error "TODO: review need for this case with ledger team"


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

