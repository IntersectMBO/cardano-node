{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.MultiSig
  ( -- * Scripts
    -- | Both 'PaymentCredential's and 'StakeCredential's can use scripts.
    -- Shelley supports multi-signatures via scripts.
    Script(..),
    Hash( ScriptHashShelley
        , ScriptHashAllegra
        , ScriptHashMary),
    parseScriptShelley,
    parseScriptAllegra,
    parseScriptMary,
    parseScriptAny,
    parseScriptAll,
    parseScriptAtLeast,
    parseScriptBefore,
    parseScriptAfter,
    parseScriptSig,

    -- ** Script addresses
    -- | Making addresses from scripts.
    scriptHashShelley,
    scriptHashAllegra,
    scriptHashMary,

    -- ** multi signature scripts
    -- | Making multi-signature scripts.
    MultiSigScript(..),
    ScriptFeatureInEra(..),
    SignatureFeature,
    TimeLocksFeature,
    makeMultiSigScriptShelley,
    makeMultiSigScriptAllegra,
    makeMultiSigScriptMary,

    AsType (AsShelleyScript),

  ) where

import           Cardano.Prelude
import           Prelude (error)

import           Control.Monad (fail)
import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Scientific (toBoundedInteger)
import qualified Data.Sequence.Strict as Sequence
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Key
import           Cardano.Api.Serialisation

import           Cardano.Binary (FromCBOR (fromCBOR))
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Slotting.Slot (SlotNo (..))

--
-- Shelley imports
--

import qualified Cardano.Api.Shelley.Serialisation.Legacy as Legacy
import           Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Ledger
import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardMary, StandardShelley)
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Scripts as Ledger
import qualified Shelley.Spec.Ledger.Scripts as Shelley


-- ----------------------------------------------------------------------------
-- Scripts
--

data Script era where
  ShelleyScript :: Ledger.MultiSig StandardShelley -> Script Shelley
  AllegraScript :: Ledger.Timelock StandardAllegra -> Script Allegra
  MaryScript :: Ledger.Timelock StandardMary -> Script Mary

deriving instance Eq (Script Shelley)
deriving instance Show (Script Shelley)

deriving instance Eq (Script Allegra)
deriving instance Show (Script Allegra)

deriving instance Eq (Script Mary)
deriving instance Show (Script Mary)

instance HasTypeProxy (Script Shelley) where
    data AsType (Script Shelley) = AsShelleyScript
    proxyToAsType _ = AsShelleyScript

instance SerialiseAsRawBytes (Hash (Script Shelley)) where
    serialiseToRawBytes (ScriptHashShelley (Shelley.ScriptHash h)) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes (AsHash AsShelleyScript) bs =
      ScriptHashShelley . Shelley.ScriptHash <$> Crypto.hashFromBytes bs

instance SerialiseAsCBOR (Script Shelley) where
    serialiseToCBOR (ShelleyScript s) =
     -- We use 'WrappedMultiSig' here to support the legacy binary
      -- serialisation format for the @Script@ type from
      -- @cardano-ledger-specs@.
      --
      -- See the documentation of 'WrappedMultiSig' for more information.
      CBOR.serialize' (Legacy.WrappedMultiSig s)

    deserialiseFromCBOR AsShelleyScript bs =
     -- We use 'WrappedMultiSig' here to support the legacy binary
      -- serialisation format for the @Script@ type from
      -- @cardano-ledger-specs@.
      --
      -- See the documentation of 'WrappedMultiSig' for more information.
      ShelleyScript . Legacy.unWrappedMultiSig <$>
        CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

instance HasTextEnvelope (Script Shelley) where
    textEnvelopeType _ = "Shelley Script"
    textEnvelopeDefaultDescr (ShelleyScript _) = "Shelley multi signature script"

instance HasTypeProxy (Script Allegra) where
    data AsType (Script Allegra) = AsAllegraScript
    proxyToAsType _ = AsAllegraScript

instance SerialiseAsRawBytes (Hash (Script Allegra)) where
    serialiseToRawBytes (ScriptHashAllegra (Shelley.ScriptHash h)) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes (AsHash AsAllegraScript) bs =
      ScriptHashAllegra . Shelley.ScriptHash <$> Crypto.hashFromBytes bs

instance SerialiseAsCBOR (Script Allegra) where
    serialiseToCBOR (AllegraScript s) =
      CBOR.serialize' s

    deserialiseFromCBOR AsAllegraScript bs =
      AllegraScript <$>
        CBOR.decodeAnnotator "AllegraScript" fromCBOR (LBS.fromStrict bs)

instance HasTextEnvelope (Script Allegra) where
    textEnvelopeType _ = "Allegra Script"
    textEnvelopeDefaultDescr (AllegraScript _) =  "Allegra multi signature script"

instance HasTypeProxy (Script Mary) where
    data AsType (Script Mary) = AsMaryScript
    proxyToAsType _ = AsMaryScript

instance SerialiseAsRawBytes (Hash (Script Mary)) where
    serialiseToRawBytes (ScriptHashMary (Shelley.ScriptHash h)) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes (AsHash AsMaryScript) bs =
      ScriptHashMary . Shelley.ScriptHash <$> Crypto.hashFromBytes bs

instance SerialiseAsCBOR (Script Mary) where
    serialiseToCBOR (MaryScript s) =
      CBOR.serialize' s

    deserialiseFromCBOR AsMaryScript bs =
      MaryScript <$>
        CBOR.decodeAnnotator "MaryScript" fromCBOR (LBS.fromStrict bs)

instance HasTextEnvelope (Script Mary) where
    textEnvelopeType _ = "Mary Script"
    textEnvelopeDefaultDescr (MaryScript _) =  "Mary multi signature script"

newtype instance Hash (Script Shelley) = ScriptHashShelley (Shelley.ScriptHash StandardShelley)
  deriving (Eq, Ord, Show)

newtype instance Hash (Script Allegra) = ScriptHashAllegra (Shelley.ScriptHash StandardShelley)
  deriving (Eq, Ord, Show)


newtype instance Hash (Script Mary) = ScriptHashMary (Shelley.ScriptHash StandardShelley)
  deriving (Eq, Ord, Show)

scriptHashShelley :: Script Shelley -> Hash (Script Shelley)
scriptHashShelley (ShelleyScript s) = ScriptHashShelley (Shelley.hashMultiSigScript s)

scriptHashAllegra :: Script Allegra -> Hash (Script Allegra)
scriptHashAllegra (AllegraScript s) = ScriptHashAllegra (Ledger.hashTimelockScript s)

scriptHashMary :: Script Mary -> Hash (Script Mary)
scriptHashMary (MaryScript s) = ScriptHashMary (Ledger.hashTimelockScript s)

data MultiSigScript era where
  RequireSignature  :: Hash PaymentKey
                     -> ScriptFeatureInEra SignatureFeature era
                     -> MultiSigScript era

  RequireTimeBefore :: SlotNo
                    -> ScriptFeatureInEra TimeLocksFeature era
                    -> MultiSigScript era

  RequireTimeAfter  :: SlotNo
                    -> ScriptFeatureInEra TimeLocksFeature era
                    -> MultiSigScript era

  RequireAllOf      ::        [MultiSigScript era] -> MultiSigScript era
  RequireAnyOf      ::        [MultiSigScript era] -> MultiSigScript era
  RequireMOf        :: Int -> [MultiSigScript era] -> MultiSigScript era

deriving instance Eq (MultiSigScript Shelley)
deriving instance Show (MultiSigScript Shelley)

deriving instance Eq (MultiSigScript Allegra)
deriving instance Show (MultiSigScript Allegra)

deriving instance Eq (MultiSigScript Mary)
deriving instance Show (MultiSigScript Mary)

-- | Script Features
-- These are used within 'ScriptFeatureInEra' in conjunction with the era
-- (e.g 'Shelley', 'Allegra' etc) to specify which script features are
-- enabled in a given era.
data SignatureFeature
data TimeLocksFeature


data ScriptFeatureInEra sfeat era where
     SignaturesInShelleyEra  :: ScriptFeatureInEra SignatureFeature Shelley
     SignaturesInAllegraEra  :: ScriptFeatureInEra SignatureFeature Allegra
     SignaturesInMaryEra     :: ScriptFeatureInEra SignatureFeature Mary

     TimeLocksInAllegraEra   :: ScriptFeatureInEra TimeLocksFeature Allegra
     TimeLocksInMaryEra      :: ScriptFeatureInEra TimeLocksFeature Mary

deriving instance Eq (ScriptFeatureInEra SignatureFeature Shelley)
deriving instance Show (ScriptFeatureInEra SignatureFeature Shelley)

deriving instance Eq (ScriptFeatureInEra SignatureFeature Allegra)
deriving instance Show (ScriptFeatureInEra SignatureFeature Allegra)

deriving instance Eq (ScriptFeatureInEra SignatureFeature Mary)
deriving instance Show (ScriptFeatureInEra SignatureFeature Mary)

deriving instance Eq (ScriptFeatureInEra TimeLocksFeature Allegra)
deriving instance Show (ScriptFeatureInEra TimeLocksFeature Allegra)

deriving instance Eq (ScriptFeatureInEra TimeLocksFeature Mary)
deriving instance Show (ScriptFeatureInEra TimeLocksFeature Mary)

-- Needed for roundtripping tests
deriving instance Eq (ScriptFeatureInEra TimeLocksFeature Shelley)
deriving instance Show (ScriptFeatureInEra TimeLocksFeature Shelley)

makeMultiSigScriptShelley :: MultiSigScript Shelley -> Script Shelley
makeMultiSigScriptShelley = ShelleyScript . go
  where
    go :: MultiSigScript Shelley -> Shelley.MultiSig StandardShelley
    go (RequireSignature (PaymentKeyHash kh) SignaturesInShelleyEra)
                        = Shelley.RequireSignature (Shelley.coerceKeyRole kh)
    go (RequireAllOf s) = Shelley.RequireAllOf (map go s)
    go (RequireAnyOf s) = Shelley.RequireAnyOf (map go s)
    go (RequireMOf m s) = Shelley.RequireMOf m (map go s)
    go (RequireTimeBefore _ _) = error "Timelocks not available in Shelley era"
    go (RequireTimeAfter _ _) = error "Timelocks not available in Shelley era"

makeMultiSigScriptAllegra :: MultiSigScript Allegra -> Script Allegra
makeMultiSigScriptAllegra = AllegraScript . go
  where
    go :: MultiSigScript Allegra -> Ledger.Timelock StandardAllegra
    go (RequireSignature (PaymentKeyHash kh) SignaturesInAllegraEra)
                        = Ledger.Multi (Ledger.RequireSignature (Shelley.coerceKeyRole kh))
    go (RequireAllOf s) = Ledger.TimelockAnd  (fmap go $ Sequence.fromList s)
    go (RequireAnyOf s) = Ledger.TimelockOr (fmap go $ Sequence.fromList s)
    go (RequireMOf _m _s) = panic "fill in"
    go (RequireTimeBefore sBefore TimeLocksInAllegraEra) =
      Ledger.Interval $ Ledger.ValidityInterval { validFrom = SNothing
                                                , validTo = SJust sBefore
                                                }
    go (RequireTimeAfter sAfter TimeLocksInAllegraEra) =
      Ledger.Interval $ Ledger.ValidityInterval { validFrom = SJust sAfter
                                                , validTo = SNothing
                                                }

makeMultiSigScriptMary :: MultiSigScript Mary -> Script Mary
makeMultiSigScriptMary = MaryScript . go
  where
    go :: MultiSigScript Mary -> Ledger.Timelock StandardMary
    go (RequireSignature (PaymentKeyHash kh) SignaturesInMaryEra)
                        = Ledger.Multi (Ledger.RequireSignature (Shelley.coerceKeyRole kh))
    go (RequireAllOf s) = Ledger.TimelockAnd  (fmap go $ Sequence.fromList s)
    go (RequireAnyOf s) = Ledger.TimelockOr (fmap go $ Sequence.fromList s)
    go (RequireMOf _m _s) = panic "fill in"
    go (RequireTimeBefore sBefore TimeLocksInMaryEra) =
      Ledger.Interval $ Ledger.ValidityInterval { validFrom = SNothing
                                                , validTo = SJust sBefore
                                                }
    go (RequireTimeAfter sAfter TimeLocksInMaryEra) =
      Ledger.Interval $ Ledger.ValidityInterval { validFrom = SJust sAfter
                                                , validTo = SNothing
                                                }

signatureValue :: Hash PaymentKey -> Value
signatureValue verKeyHash =
     object [ "keyHash" .= String (Text.decodeUtf8 . serialiseToRawBytesHex $ verKeyHash)
            , "type" .= String "sig"
            ]

requireAnyOfValue :: ToJSON (MultiSigScript era) => [MultiSigScript era] -> Value
requireAnyOfValue scripts =
  object [ "type" .= String "any", "scripts" .= map toJSON scripts ]

requireAllOfValue :: ToJSON (MultiSigScript era) => [MultiSigScript era] -> Value
requireAllOfValue scripts =
  object [ "type" .= String "all", "scripts" .= map toJSON scripts ]

requireAtLeastValue :: ToJSON (MultiSigScript era) => Int -> [MultiSigScript era] -> Value
requireAtLeastValue reqNum scripts=
  object [ "type" .= String "atLeast"
         , "required" .= reqNum
         , "scripts" .= map toJSON scripts
         ]

requireTimeBeforeValue :: SlotNo -> ScriptFeatureInEra TimeLocksFeature era -> Value
requireTimeBeforeValue (SlotNo sBefore) _ =
  object [ "type" .= String "before"
         , "slot" .= Number (fromInteger $ fromIntegral sBefore)
         ]

requireTimeAfterValue :: SlotNo -> ScriptFeatureInEra TimeLocksFeature era -> Value
requireTimeAfterValue (SlotNo sAfter) _ =
  object [ "type" .= String "after"
         , "slot" .= Number (fromInteger $ fromIntegral sAfter)
         ]

gatherMultiSigScripts
  :: (Value -> Aeson.Parser (MultiSigScript era))
  -> Vector Value
  -> Aeson.Parser [MultiSigScript era]
gatherMultiSigScripts parser vs = sequence . Vector.toList $ Vector.map parser vs

parseScriptAny
  :: (Value -> Aeson.Parser (MultiSigScript era))
  -> Value
  -> Aeson.Parser (MultiSigScript era)
parseScriptAny recurse = Aeson.withObject "any" $ \obj -> do
  t <- obj .: "type"
  case t :: Text of
    "any" -> do s <- obj .: "scripts"
                RequireAnyOf <$> gatherMultiSigScripts recurse s
    _ -> fail "\"any\" multi-signature script value not found"

parseScriptAll
  :: (Value -> Aeson.Parser (MultiSigScript era))
  -> Value
  -> Aeson.Parser (MultiSigScript era)
parseScriptAll recurse = Aeson.withObject "all" $ \obj -> do
  t <- obj .: "type"
  case t :: Text of
    "all" -> do s <- obj .: "scripts"
                RequireAllOf <$> gatherMultiSigScripts recurse s
    _ -> fail "\"all\" multi-signature script value not found"

parseScriptAtLeast
  :: (Value -> Aeson.Parser (MultiSigScript era))
  -> Value
  -> Aeson.Parser (MultiSigScript era)
parseScriptAtLeast recurse = Aeson.withObject "atLeast" $ \obj -> do
  v <- obj .: "type"
  case v :: Text of
    "atLeast" -> do
      r <- obj .: "required"
      s <- obj .: "scripts"
      case r of
        Number sci ->
          case toBoundedInteger sci of
            Just reqInt ->
              do msigscripts <- gatherMultiSigScripts recurse s
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


parseScriptSig :: ScriptFeatureInEra SignatureFeature era -> Value -> Aeson.Parser (MultiSigScript era)
parseScriptSig sFeatInEra = Aeson.withObject "sig" $ \obj -> do
  v <- obj .: "type"
  case v :: Text of
    "sig" -> do k <- obj .: "keyHash"
                flip RequireSignature sFeatInEra <$> convertToHash k
    _     -> fail "\"sig\" multi-signature script value not found"

convertToHash :: Text -> Aeson.Parser (Hash PaymentKey)
convertToHash txt = case deserialiseFromRawBytesHex (AsHash AsPaymentKey) $ Text.encodeUtf8 txt of
                      Just payKeyHash -> return payKeyHash
                      Nothing -> fail $ "Error deserialising payment key hash: " <> Text.unpack txt

parseScriptBefore
  :: ScriptFeatureInEra TimeLocksFeature era
  -> Value
  -> Aeson.Parser (MultiSigScript era)
parseScriptBefore sFeatInEra = Aeson.withObject "before" $ \obj -> do
  v <- obj .: "type"
  case v :: Text of
    "before" -> do s <- obj .: "slot"
                   return $ RequireTimeBefore s sFeatInEra
    _        -> fail "\"before\" multi-signature script value not found"

parseScriptAfter
  :: ScriptFeatureInEra TimeLocksFeature era
  -> Value
  -> Aeson.Parser (MultiSigScript era)
parseScriptAfter sFeatInEra = Aeson.withObject "after" $ \obj -> do
  v <- obj .: "type"
  case v :: Text of
    "after" -> do s <- obj .: "slot"
                  return $ RequireTimeAfter s sFeatInEra
    _       -> fail "\"after\" multi-signature script value not found"

instance ToJSON (MultiSigScript Shelley) where
  toJSON (RequireSignature pKeyHash SignaturesInShelleyEra) = signatureValue pKeyHash
  toJSON (RequireAnyOf anyScripts) = requireAnyOfValue anyScripts
  toJSON (RequireAllOf allScripts) = requireAllOfValue allScripts
  toJSON (RequireMOf reqNum reqScripts) = requireAtLeastValue reqNum reqScripts

  toJSON (RequireTimeBefore _ _) =
    error "Timelocks not available in Shelley era multi signature scripts"
  toJSON (RequireTimeAfter _ _) =
    error "Timelocks not available in Shelley era multi signature scripts"

instance FromJSON (MultiSigScript Shelley) where
  parseJSON = parseScriptShelley

parseScriptShelley :: Value -> Aeson.Parser (MultiSigScript Shelley)
parseScriptShelley v =     parseScriptSig SignaturesInShelleyEra v
                       <|> parseScriptAny parseScriptShelley v
                       <|> parseScriptAll parseScriptShelley v
                       <|> parseScriptAtLeast parseScriptShelley v

instance ToJSON (MultiSigScript Allegra) where
  toJSON (RequireSignature pKeyHash SignaturesInAllegraEra) = signatureValue pKeyHash
  toJSON (RequireAnyOf anyScripts) = requireAnyOfValue anyScripts
  toJSON (RequireAllOf allScripts) = requireAllOfValue allScripts
  toJSON (RequireMOf reqNum reqScripts) = requireAtLeastValue reqNum reqScripts
  toJSON (RequireTimeBefore sBefore TimeLocksInAllegraEra) =
    requireTimeBeforeValue sBefore TimeLocksInAllegraEra
  toJSON (RequireTimeAfter sAfter TimeLocksInAllegraEra) =
    requireTimeAfterValue sAfter TimeLocksInAllegraEra

instance FromJSON (MultiSigScript Allegra) where
  parseJSON = parseScriptAllegra

parseScriptAllegra :: Value -> Aeson.Parser (MultiSigScript Allegra)
parseScriptAllegra v =     parseScriptSig SignaturesInAllegraEra v
                       <|> parseScriptBefore TimeLocksInAllegraEra v
                       <|> parseScriptAfter TimeLocksInAllegraEra v
                       <|> parseScriptAny parseScriptAllegra v
                       <|> parseScriptAll parseScriptAllegra v
                       <|> parseScriptAtLeast parseScriptAllegra v

instance ToJSON (MultiSigScript Mary) where
  toJSON (RequireSignature pKeyHash SignaturesInMaryEra) = signatureValue pKeyHash
  toJSON (RequireAnyOf anyScripts) = requireAnyOfValue anyScripts
  toJSON (RequireAllOf allScripts) = requireAllOfValue allScripts
  toJSON (RequireMOf reqNum reqScripts) = requireAtLeastValue reqNum reqScripts
  toJSON (RequireTimeBefore sBefore TimeLocksInMaryEra) =
    requireTimeBeforeValue sBefore TimeLocksInMaryEra
  toJSON (RequireTimeAfter sAfter TimeLocksInMaryEra) =
    requireTimeAfterValue sAfter TimeLocksInMaryEra

instance FromJSON (MultiSigScript Mary) where
  parseJSON = parseScriptMary

parseScriptMary :: Value -> Aeson.Parser (MultiSigScript Mary)
parseScriptMary v =     parseScriptSig SignaturesInMaryEra v
                    <|> parseScriptBefore TimeLocksInMaryEra v
                    <|> parseScriptAfter TimeLocksInMaryEra v
                    <|> parseScriptAny parseScriptMary v
                    <|> parseScriptAll parseScriptMary v
                    <|> parseScriptAtLeast parseScriptMary v
