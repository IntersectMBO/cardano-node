{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Use section" -}

module Cardano.Api.Script (
    -- * Languages
    SimpleScript',
    PlutusScriptV1,
    PlutusScriptV2,
    ScriptLanguage(..),
    PlutusScriptVersion(..),
    AnyScriptLanguage(..),
    AnyPlutusScriptVersion(..),
    IsPlutusScriptLanguage(..),
    IsScriptLanguage(..),

    -- * Scripts in a specific language
    Script(..),

    -- * Scripts in any language
    ScriptInAnyLang(..),
    toScriptInAnyLang,

    -- * Scripts in an era
    ScriptInEra(..),
    toScriptInEra,
    eraOfScriptInEra,

    -- * Reference scripts
    ReferenceScript(..),
    ReferenceTxInsScriptsInlineDatumsSupportedInEra(..),
    refInsScriptsAndInlineDatsSupportedInEra,
    refScriptToShelleyScript,

    -- * Use of a script in an era as a witness
    WitCtxTxIn, WitCtxMint, WitCtxStake,
    WitCtx(..),
    ScriptWitness(..),
    Witness(..),
    KeyWitnessInCtx(..),
    ScriptWitnessInCtx(..),
    ScriptDatum(..),
    ScriptRedeemer,
    scriptWitnessScript,

    -- ** Languages supported in each era
    ScriptLanguageInEra(..),
    scriptLanguageSupportedInEra,
    languageOfScriptLanguageInEra,
    eraOfScriptLanguageInEra,

    -- * The simple script language
    SimpleScript(..),
    SimpleScriptOrReferenceInput(..),

    -- * The Plutus script language
    PlutusScript(..),
    PlutusScriptOrReferenceInput(..),
    examplePlutusScriptAlwaysSucceeds,
    examplePlutusScriptAlwaysFails,

    -- * Script data
    ScriptData(..),

    -- * Script execution units
    ExecutionUnits(..),

    -- * Script hashes
    ScriptHash(..),
    hashScript,

    -- * Internal conversion functions
    toShelleyScript,
    fromShelleyBasedScript,
    toShelleyMultiSig,
    fromShelleyMultiSig,
    toAllegraTimelock,
    fromAllegraTimelock,
    toAlonzoExUnits,
    fromAlonzoExUnits,
    toShelleyScriptHash,
    fromShelleyScriptHash,
    toPlutusData,
    fromPlutusData,
    toAlonzoData,
    fromAlonzoData,
    toAlonzoLanguage,
    fromAlonzoLanguage,
    fromShelleyScriptToReferenceScript,

    -- * Data family instances
    AsType(..),
    Hash(..),
  ) where

import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import           Data.Either.Combinators (maybeToRight)
import           Data.Foldable (toList)
import           Data.Functor
import           Data.Scientific (toBoundedInteger)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import           Data.Typeable (Typeable)
import           Numeric.Natural (Natural)

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

import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Core as Ledger

import qualified Cardano.Ledger.Keys as Shelley
import qualified Cardano.Ledger.Shelley.Scripts as Shelley
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Timelock
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo

import qualified PlutusLedgerApi.Test.Examples as Plutus

import           Cardano.Api.EraCast
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.ScriptData
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.SerialiseUsing
import           Cardano.Api.TxIn
import           Cardano.Api.Utils (failEitherWith)
import qualified Data.Sequence.Strict as Seq

-- ----------------------------------------------------------------------------
-- Types for script language and version
--

data SimpleScript'

-- | The original simple script language which supports
--
-- * require a signature from a given key (by verification key hash)
-- * n-way and combinator
-- * n-way or combinator
-- * m-of-n combinator
--
-- This version of the language was introduced in the 'ShelleyEra'.
--

-- | The second version of the simple script language. It has all the features
-- of the original simple script language plus new atomic predicates:
--
-- * require the time be before a given slot number
-- * require the time be after a given slot number
--
-- This version of the language was introduced in the 'AllegraEra'.
--
-- However we opt for a single type level tag 'SimpleScript'' as the second version of
-- of the language introduced in the Allegra era is a superset of the language introduced
-- in the Shelley era.

-- | Place holder type to show what the pattern is to extend to multiple
-- languages, not just multiple versions of a single language.
--
data PlutusScriptV1
data PlutusScriptV2

instance HasTypeProxy SimpleScript' where
    data AsType SimpleScript' = AsSimpleScript
    proxyToAsType _ = AsSimpleScript

instance HasTypeProxy PlutusScriptV1 where
    data AsType PlutusScriptV1 = AsPlutusScriptV1
    proxyToAsType :: Proxy PlutusScriptV1 -> AsType PlutusScriptV1
    proxyToAsType _ = AsPlutusScriptV1

instance HasTypeProxy PlutusScriptV2 where
    data AsType PlutusScriptV2 = AsPlutusScriptV2
    proxyToAsType _ = AsPlutusScriptV2

-- ----------------------------------------------------------------------------
-- Value level representation for script languages
--
data ScriptLanguage lang where

     SimpleScriptLanguage :: ScriptLanguage SimpleScript'

     PlutusScriptLanguage :: PlutusScriptVersion lang -> ScriptLanguage lang

deriving instance (Eq   (ScriptLanguage lang))
deriving instance (Show (ScriptLanguage lang))

instance TestEquality ScriptLanguage where
    testEquality SimpleScriptLanguage SimpleScriptLanguage = Just Refl

    testEquality (PlutusScriptLanguage lang)
                 (PlutusScriptLanguage lang') = testEquality lang lang'

    testEquality  _ _ = Nothing


data PlutusScriptVersion lang where
    PlutusScriptV1 :: PlutusScriptVersion PlutusScriptV1
    PlutusScriptV2 :: PlutusScriptVersion PlutusScriptV2

deriving instance (Eq   (PlutusScriptVersion lang))
deriving instance (Show (PlutusScriptVersion lang))

instance TestEquality PlutusScriptVersion where
    testEquality PlutusScriptV1 PlutusScriptV1 = Just Refl
    testEquality PlutusScriptV2 PlutusScriptV2 = Just Refl
    testEquality _ _ = Nothing


data AnyScriptLanguage where
     AnyScriptLanguage :: ScriptLanguage lang -> AnyScriptLanguage

deriving instance (Show AnyScriptLanguage)

instance Eq AnyScriptLanguage where
    a == b = fromEnum a == fromEnum b

instance Ord AnyScriptLanguage where
    compare a b = compare (fromEnum a) (fromEnum b)

instance Enum AnyScriptLanguage where
    toEnum 0 = AnyScriptLanguage SimpleScriptLanguage
    toEnum 1 = AnyScriptLanguage (PlutusScriptLanguage PlutusScriptV1)
    toEnum 2 = AnyScriptLanguage (PlutusScriptLanguage PlutusScriptV2)
    toEnum err = error $ "AnyScriptLanguage.toEnum: bad argument: " <> show err

    fromEnum (AnyScriptLanguage SimpleScriptLanguage) = 0
    fromEnum (AnyScriptLanguage (PlutusScriptLanguage PlutusScriptV1)) = 1
    fromEnum (AnyScriptLanguage (PlutusScriptLanguage PlutusScriptV2)) = 2

instance Bounded AnyScriptLanguage where
    minBound = AnyScriptLanguage SimpleScriptLanguage
    maxBound = AnyScriptLanguage (PlutusScriptLanguage PlutusScriptV2)


data AnyPlutusScriptVersion where
     AnyPlutusScriptVersion :: PlutusScriptVersion lang
                            -> AnyPlutusScriptVersion

deriving instance (Show AnyPlutusScriptVersion)

instance Eq AnyPlutusScriptVersion where
    a == b = fromEnum a == fromEnum b

instance Ord AnyPlutusScriptVersion where
    compare a b = compare (fromEnum a) (fromEnum b)

instance Enum AnyPlutusScriptVersion where
    toEnum 0 = AnyPlutusScriptVersion PlutusScriptV1
    toEnum 1 = AnyPlutusScriptVersion PlutusScriptV2
    toEnum err = error $ "AnyPlutusScriptVersion.toEnum: bad argument: " <> show err

    fromEnum (AnyPlutusScriptVersion PlutusScriptV1) = 0
    fromEnum (AnyPlutusScriptVersion PlutusScriptV2) = 1

instance Bounded AnyPlutusScriptVersion where
    minBound = AnyPlutusScriptVersion PlutusScriptV1
    maxBound = AnyPlutusScriptVersion PlutusScriptV2

instance ToCBOR AnyPlutusScriptVersion where
    toCBOR = toCBOR . fromEnum

instance FromCBOR AnyPlutusScriptVersion where
    fromCBOR = do
      n <- fromCBOR
      if n >= fromEnum (minBound :: AnyPlutusScriptVersion) &&
         n <= fromEnum (maxBound :: AnyPlutusScriptVersion)
        then return $! toEnum n
        else fail "plutus script version out of bounds"

instance ToJSON AnyPlutusScriptVersion where
    toJSON (AnyPlutusScriptVersion PlutusScriptV1) =
      Aeson.String "PlutusScriptV1"
    toJSON (AnyPlutusScriptVersion PlutusScriptV2) =
      Aeson.String "PlutusScriptV2"

parsePlutusScriptVersion :: Text -> Aeson.Parser AnyPlutusScriptVersion
parsePlutusScriptVersion t =
  case t of
    "PlutusScriptV1" -> return (AnyPlutusScriptVersion PlutusScriptV1)
    "PlutusScriptV2" -> return (AnyPlutusScriptVersion PlutusScriptV2)
    _                -> fail "Expected PlutusScriptV1 or PlutusScriptV2"

instance FromJSON AnyPlutusScriptVersion where
    parseJSON = Aeson.withText "PlutusScriptVersion" parsePlutusScriptVersion

instance Aeson.FromJSONKey AnyPlutusScriptVersion where
    fromJSONKey = Aeson.FromJSONKeyTextParser parsePlutusScriptVersion

instance Aeson.ToJSONKey AnyPlutusScriptVersion where
    toJSONKey = Aeson.toJSONKeyText toText
      where
        toText :: AnyPlutusScriptVersion -> Text
        toText (AnyPlutusScriptVersion PlutusScriptV1) = "PlutusScriptV1"
        toText (AnyPlutusScriptVersion PlutusScriptV2) = "PlutusScriptV2"

toAlonzoLanguage :: AnyPlutusScriptVersion -> Alonzo.Language
toAlonzoLanguage (AnyPlutusScriptVersion PlutusScriptV1) = Alonzo.PlutusV1
toAlonzoLanguage (AnyPlutusScriptVersion PlutusScriptV2) = Alonzo.PlutusV2

fromAlonzoLanguage :: Alonzo.Language -> AnyPlutusScriptVersion
fromAlonzoLanguage Alonzo.PlutusV1 = AnyPlutusScriptVersion PlutusScriptV1
fromAlonzoLanguage Alonzo.PlutusV2 = AnyPlutusScriptVersion PlutusScriptV2


class HasTypeProxy lang => IsScriptLanguage lang where
    scriptLanguage :: ScriptLanguage lang

instance IsScriptLanguage SimpleScript' where
    scriptLanguage = SimpleScriptLanguage

instance IsScriptLanguage PlutusScriptV1 where
    scriptLanguage = PlutusScriptLanguage PlutusScriptV1

instance IsScriptLanguage PlutusScriptV2 where
    scriptLanguage = PlutusScriptLanguage PlutusScriptV2

class IsScriptLanguage lang => IsPlutusScriptLanguage lang where
    plutusScriptVersion :: PlutusScriptVersion lang

instance IsPlutusScriptLanguage PlutusScriptV1 where
    plutusScriptVersion = PlutusScriptV1

instance IsPlutusScriptLanguage PlutusScriptV2 where
    plutusScriptVersion = PlutusScriptV2

-- ----------------------------------------------------------------------------
-- Script type: covering all script languages
--

-- | A script in a particular language.
--
-- See also 'ScriptInAnyLang' for a script in any of the known languages.
--
-- See also 'ScriptInEra' for a script in a language that is available within
-- a particular era.
--
-- Note that some but not all scripts have an external JSON syntax, hence this
-- type has no JSON serialisation instances. The 'SimpleScript' family of
-- languages do have a JSON syntax and thus have 'ToJSON'\/'FromJSON' instances.
--
data Script lang where

     SimpleScript :: !SimpleScript
                  -> Script SimpleScript'

     PlutusScript :: !(PlutusScriptVersion lang)
                  -> !(PlutusScript lang)
                  -> Script lang

deriving instance (Eq   (Script lang))
deriving instance (Show (Script lang))

instance HasTypeProxy lang => HasTypeProxy (Script lang) where
    data AsType (Script lang) = AsScript (AsType lang)
    proxyToAsType _ = AsScript (proxyToAsType (Proxy :: Proxy lang))

instance IsScriptLanguage lang => SerialiseAsCBOR (Script lang) where
    serialiseToCBOR (SimpleScript s) =
      CBOR.serialize' (toAllegraTimelock s :: Timelock.Timelock StandardCrypto)

    serialiseToCBOR (PlutusScript PlutusScriptV1 s) =
      CBOR.serialize' s

    serialiseToCBOR (PlutusScript PlutusScriptV2 s) =
      CBOR.serialize' s

    deserialiseFromCBOR _ bs =
      case scriptLanguage :: ScriptLanguage lang of
        SimpleScriptLanguage ->
              SimpleScript . fromAllegraTimelock
          <$> CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

        PlutusScriptLanguage PlutusScriptV1 ->
              PlutusScript PlutusScriptV1
          <$> CBOR.decodeFull' bs

        PlutusScriptLanguage PlutusScriptV2 ->
              PlutusScript PlutusScriptV2
          <$> CBOR.decodeFull' bs

instance IsScriptLanguage lang => HasTextEnvelope (Script lang) where
    textEnvelopeType _ =
      case scriptLanguage :: ScriptLanguage lang of
        SimpleScriptLanguage -> "SimpleScript"
        PlutusScriptLanguage PlutusScriptV1 -> "PlutusScriptV1"
        PlutusScriptLanguage PlutusScriptV2 -> "PlutusScriptV2"


-- ----------------------------------------------------------------------------
-- Scripts in any language
--

-- | Sometimes it is necessary to handle all languages without making static
-- type distinctions between languages. For example, when reading external
-- input, or before the era context is known.
--
-- Use 'toScriptInEra' to convert to a script in the context of an era.
--
data ScriptInAnyLang where
     ScriptInAnyLang :: ScriptLanguage lang
                     -> Script lang
                     -> ScriptInAnyLang

deriving instance Show ScriptInAnyLang

-- The GADT in the ScriptInAnyLang constructor requires a custom Eq instance
instance Eq ScriptInAnyLang where
    (==) (ScriptInAnyLang lang  script)
         (ScriptInAnyLang lang' script') =
      case testEquality lang lang' of
        Nothing   -> False
        Just Refl -> script == script'

instance ToJSON ScriptInAnyLang where
  toJSON (ScriptInAnyLang l s) =
    object [ "scriptLanguage" .= show l
           , "script" .= obtainScriptLangConstraint l
                           (serialiseToTextEnvelope Nothing s)
           ]
      where
       obtainScriptLangConstraint
         :: ScriptLanguage lang
         -> (IsScriptLanguage lang => a)
         -> a
       obtainScriptLangConstraint SimpleScriptLanguage f = f
       obtainScriptLangConstraint (PlutusScriptLanguage PlutusScriptV1) f = f
       obtainScriptLangConstraint (PlutusScriptLanguage PlutusScriptV2) f = f

instance FromJSON ScriptInAnyLang where
  parseJSON = Aeson.withObject "ScriptInAnyLang" $ \o -> do
    textEnvelopeScript <- o .: "script"
    case textEnvelopeToScript textEnvelopeScript of
      Left textEnvErr -> fail $ displayError textEnvErr
      Right (ScriptInAnyLang l s) -> pure $ ScriptInAnyLang l s

-- | Convert a script in a specific statically-known language to a
-- 'ScriptInAnyLang'.
--
-- No inverse to this is provided, just do case analysis on the 'ScriptLanguage'
-- field within the 'ScriptInAnyLang' constructor.
--
toScriptInAnyLang :: Script lang -> ScriptInAnyLang
toScriptInAnyLang s@(SimpleScript _) =
    ScriptInAnyLang SimpleScriptLanguage s
toScriptInAnyLang s@(PlutusScript v _) =
    ScriptInAnyLang (PlutusScriptLanguage v) s

instance HasTypeProxy ScriptInAnyLang where
    data AsType ScriptInAnyLang = AsScriptInAnyLang
    proxyToAsType _ = AsScriptInAnyLang


-- ----------------------------------------------------------------------------
-- Scripts in the context of a ledger era
--

data ScriptInEra era where
     ScriptInEra :: ScriptLanguageInEra lang era
                 -> Script lang
                 -> ScriptInEra era

deriving instance Show (ScriptInEra era)

-- The GADT in the ScriptInEra constructor requires a custom instance
instance Eq (ScriptInEra era) where
    (==) (ScriptInEra langInEra  script)
         (ScriptInEra langInEra' script') =
      case testEquality (languageOfScriptLanguageInEra langInEra)
                        (languageOfScriptLanguageInEra langInEra') of
        Nothing   -> False
        Just Refl -> script == script'


data ScriptLanguageInEra lang era where

     SimpleScriptInShelley   :: ScriptLanguageInEra SimpleScript' ShelleyEra
     SimpleScriptInAllegra   :: ScriptLanguageInEra SimpleScript' AllegraEra
     SimpleScriptInMary      :: ScriptLanguageInEra SimpleScript' MaryEra
     SimpleScriptInAlonzo    :: ScriptLanguageInEra SimpleScript' AlonzoEra
     SimpleScriptInBabbage   :: ScriptLanguageInEra SimpleScript' BabbageEra

     PlutusScriptV1InAlonzo  :: ScriptLanguageInEra PlutusScriptV1 AlonzoEra
     PlutusScriptV1InBabbage :: ScriptLanguageInEra PlutusScriptV1 BabbageEra

     PlutusScriptV2InBabbage :: ScriptLanguageInEra PlutusScriptV2 BabbageEra



deriving instance Eq   (ScriptLanguageInEra lang era)
deriving instance Show (ScriptLanguageInEra lang era)

instance ToJSON (ScriptLanguageInEra lang era) where
  toJSON sLangInEra = Aeson.String . Text.pack $ show sLangInEra


instance HasTypeProxy era => HasTypeProxy (ScriptInEra era) where
    data AsType (ScriptInEra era) = AsScriptInEra (AsType era)
    proxyToAsType _ = AsScriptInEra (proxyToAsType (Proxy :: Proxy era))


-- | Check if a given script language is supported in a given era, and if so
-- return the evidence.
--
scriptLanguageSupportedInEra :: CardanoEra era
                             -> ScriptLanguage lang
                             -> Maybe (ScriptLanguageInEra lang era)
scriptLanguageSupportedInEra era lang =
    case (era, lang) of
      (ShelleyEra, SimpleScriptLanguage) ->
        Just SimpleScriptInShelley

      (AllegraEra, SimpleScriptLanguage) ->
        Just SimpleScriptInAllegra

      (MaryEra, SimpleScriptLanguage) ->
        Just SimpleScriptInMary

      (AlonzoEra, SimpleScriptLanguage) ->
        Just SimpleScriptInAlonzo

      (BabbageEra, SimpleScriptLanguage) ->
        Just SimpleScriptInBabbage

      (AlonzoEra, PlutusScriptLanguage PlutusScriptV1) ->
        Just PlutusScriptV1InAlonzo

      (BabbageEra, PlutusScriptLanguage PlutusScriptV1) ->
        Just PlutusScriptV1InBabbage

      (BabbageEra, PlutusScriptLanguage PlutusScriptV2) ->
        Just PlutusScriptV2InBabbage

      _ -> Nothing

languageOfScriptLanguageInEra :: ScriptLanguageInEra lang era
                              -> ScriptLanguage lang
languageOfScriptLanguageInEra langInEra =
    case langInEra of
      SimpleScriptInShelley   -> SimpleScriptLanguage
      SimpleScriptInAllegra   -> SimpleScriptLanguage
      SimpleScriptInMary      -> SimpleScriptLanguage
      SimpleScriptInAlonzo    -> SimpleScriptLanguage
      SimpleScriptInBabbage   -> SimpleScriptLanguage

      PlutusScriptV1InAlonzo  -> PlutusScriptLanguage PlutusScriptV1
      PlutusScriptV1InBabbage -> PlutusScriptLanguage PlutusScriptV1
      PlutusScriptV2InBabbage -> PlutusScriptLanguage PlutusScriptV2

eraOfScriptLanguageInEra :: ScriptLanguageInEra lang era
                         -> ShelleyBasedEra era
eraOfScriptLanguageInEra langInEra =
    case langInEra of
      SimpleScriptInShelley   -> ShelleyBasedEraShelley

      SimpleScriptInAllegra   -> ShelleyBasedEraAllegra

      SimpleScriptInMary      -> ShelleyBasedEraMary

      SimpleScriptInAlonzo    -> ShelleyBasedEraAlonzo
      PlutusScriptV1InAlonzo  -> ShelleyBasedEraAlonzo

      SimpleScriptInBabbage   -> ShelleyBasedEraBabbage
      PlutusScriptV1InBabbage -> ShelleyBasedEraBabbage
      PlutusScriptV2InBabbage -> ShelleyBasedEraBabbage

-- | Given a target era and a script in some language, check if the language is
-- supported in that era, and if so return a 'ScriptInEra'.
--
toScriptInEra :: CardanoEra era -> ScriptInAnyLang -> Maybe (ScriptInEra era)
toScriptInEra era (ScriptInAnyLang lang s) = do
    lang' <- scriptLanguageSupportedInEra era lang
    return (ScriptInEra lang' s)

eraOfScriptInEra :: ScriptInEra era -> ShelleyBasedEra era
eraOfScriptInEra (ScriptInEra langInEra _) = eraOfScriptLanguageInEra langInEra

-- ----------------------------------------------------------------------------
-- Scripts used in a transaction (in an era) to witness authorised use
--

-- | A tag type for the context in which a script is used in a transaction.
--
-- This type tags the context as being to witness a transaction input.
--
data WitCtxTxIn

-- | A tag type for the context in which a script is used in a transaction.
--
-- This type tags the context as being to witness minting.
--
data WitCtxMint

-- | A tag type for the context in which a script is used in a transaction.
--
-- This type tags the context as being to witness the use of stake addresses in
-- both certificates and withdrawals.
--
data WitCtxStake


-- | This GADT provides a value-level representation of all the witness
-- contexts. This enables pattern matching on the context to allow them to be
-- treated in a non-uniform way.
--
data WitCtx witctx where
     WitCtxTxIn  :: WitCtx WitCtxTxIn
     WitCtxMint  :: WitCtx WitCtxMint
     WitCtxStake :: WitCtx WitCtxStake

-- | Scripts can now exist in the UTxO at a transaction output. We can
-- reference these scripts via specification of a reference transaction input
-- in order to witness spending inputs, withdrawals, certificates
-- or to mint tokens. This datatype encapsulates this concept.
data PlutusScriptOrReferenceInput lang
  = PScript (PlutusScript lang)
  | PReferenceScript TxIn (Maybe ScriptHash)
  deriving (Eq, Show)


data SimpleScriptOrReferenceInput lang
  = SScript SimpleScript
  | SReferenceScript TxIn (Maybe ScriptHash)
  deriving (Eq, Show)

-- | A /use/ of a script within a transaction body to witness that something is
-- being used in an authorised manner. That can be
--
-- * spending a transaction input
-- * minting tokens
-- * using a certificate (stake address certs specifically)
-- * withdrawing from a reward account
--
-- For simple script languages, the use of the script is the same in all
-- contexts. For Plutus scripts, using a script involves supplying a redeemer.
-- In addition, Plutus scripts used for spending inputs must also supply the
-- datum value used when originally creating the TxOut that is now being spent.
--
data ScriptWitness witctx era where

     SimpleScriptWitness :: ScriptLanguageInEra SimpleScript' era
                         -> SimpleScriptOrReferenceInput SimpleScript'
                         -> ScriptWitness witctx era

     PlutusScriptWitness :: ScriptLanguageInEra lang era
                         -> PlutusScriptVersion lang
                         -> PlutusScriptOrReferenceInput lang
                         -> ScriptDatum witctx
                         -> ScriptRedeemer
                         -> ExecutionUnits
                         -> ScriptWitness witctx era

deriving instance Show (ScriptWitness witctx era)

-- The GADT in the SimpleScriptWitness constructor requires a custom instance
instance Eq (ScriptWitness witctx era) where
    (==) (SimpleScriptWitness langInEra  script)
         (SimpleScriptWitness langInEra' script') =
      case testEquality (languageOfScriptLanguageInEra langInEra)
                        (languageOfScriptLanguageInEra langInEra') of
        Nothing   -> False
        Just Refl -> script == script'

    (==) (PlutusScriptWitness langInEra  version   script
                              datum      redeemer  execUnits)
         (PlutusScriptWitness langInEra' version'  script'
                              datum'     redeemer' execUnits') =
      case testEquality (languageOfScriptLanguageInEra langInEra)
                        (languageOfScriptLanguageInEra langInEra') of
        Nothing   -> False
        Just Refl ->    version   == version'
                     && script    == script'
                     && datum     == datum'
                     && redeemer  == redeemer'
                     && execUnits == execUnits'

    (==)  _ _ = False

type ScriptRedeemer = HashableScriptData

data ScriptDatum witctx where
     ScriptDatumForTxIn    :: HashableScriptData -> ScriptDatum WitCtxTxIn
     InlineScriptDatum     :: ScriptDatum WitCtxTxIn
     NoScriptDatumForMint  :: ScriptDatum WitCtxMint
     NoScriptDatumForStake :: ScriptDatum WitCtxStake

deriving instance Eq   (ScriptDatum witctx)
deriving instance Show (ScriptDatum witctx)

-- We cannot always extract a script from a script witness due to reference scripts.
-- Reference scripts exist in the UTxO, so without access to the UTxO we cannot
-- retrieve the script.
scriptWitnessScript :: ScriptWitness witctx era -> Maybe (ScriptInEra era)
scriptWitnessScript (SimpleScriptWitness SimpleScriptInShelley (SScript script)) =
    Just $ ScriptInEra SimpleScriptInShelley (SimpleScript script)

scriptWitnessScript (SimpleScriptWitness SimpleScriptInAllegra (SScript script)) =
    Just $ ScriptInEra SimpleScriptInAllegra (SimpleScript script)

scriptWitnessScript (SimpleScriptWitness SimpleScriptInMary (SScript script)) =
    Just $ ScriptInEra SimpleScriptInMary (SimpleScript script)

scriptWitnessScript (SimpleScriptWitness SimpleScriptInAlonzo (SScript script)) =
    Just $ ScriptInEra SimpleScriptInAlonzo (SimpleScript script)

scriptWitnessScript (SimpleScriptWitness SimpleScriptInBabbage (SScript script)) =
    Just $ ScriptInEra SimpleScriptInBabbage (SimpleScript script)

scriptWitnessScript (PlutusScriptWitness langInEra version (PScript script) _ _ _) =
    Just $ ScriptInEra langInEra (PlutusScript version script)

scriptWitnessScript (SimpleScriptWitness _ (SReferenceScript _ _)) =
    Nothing

scriptWitnessScript (PlutusScriptWitness _ _ (PReferenceScript _ _) _ _ _) =
    Nothing

-- ----------------------------------------------------------------------------
-- The kind of witness to use, key (signature) or script
--

data Witness witctx era where

     KeyWitness    :: KeyWitnessInCtx witctx
                   -> Witness         witctx era

     ScriptWitness :: ScriptWitnessInCtx witctx
                   -> ScriptWitness      witctx era
                   -> Witness            witctx era

deriving instance Eq   (Witness witctx era)
deriving instance Show (Witness witctx era)

data KeyWitnessInCtx witctx where

     KeyWitnessForSpending  :: KeyWitnessInCtx WitCtxTxIn
     KeyWitnessForStakeAddr :: KeyWitnessInCtx WitCtxStake

data ScriptWitnessInCtx witctx where

     ScriptWitnessForSpending  :: ScriptWitnessInCtx WitCtxTxIn
     ScriptWitnessForMinting   :: ScriptWitnessInCtx WitCtxMint
     ScriptWitnessForStakeAddr :: ScriptWitnessInCtx WitCtxStake

deriving instance Eq   (KeyWitnessInCtx witctx)
deriving instance Show (KeyWitnessInCtx witctx)

deriving instance Eq   (ScriptWitnessInCtx witctx)
deriving instance Show (ScriptWitnessInCtx witctx)


-- ----------------------------------------------------------------------------
-- Script execution units
--

-- | The units for how long a script executes for and how much memory it uses.
-- This is used to declare the resources used by a particular use of a script.
--
-- This type is also used to describe the limits for the maximum overall
-- execution units per transaction or per block.
--
data ExecutionUnits =
     ExecutionUnits {
        -- | This corresponds roughly to the time to execute a script.
        executionSteps  :: Natural,

        -- | This corresponds roughly to the peak memory used during script
        -- execution.
        executionMemory :: Natural
     }
  deriving (Eq, Show)

instance ToCBOR ExecutionUnits where
  toCBOR ExecutionUnits{executionSteps, executionMemory} =
      CBOR.encodeListLen 2
   <> toCBOR executionSteps
   <> toCBOR executionMemory

instance FromCBOR ExecutionUnits where
  fromCBOR = do
    CBOR.enforceSize "ExecutionUnits" 2
    ExecutionUnits
      <$> fromCBOR
      <*> fromCBOR

instance ToJSON ExecutionUnits where
  toJSON ExecutionUnits{executionSteps, executionMemory} =
    object [ "steps"  .= executionSteps
           , "memory" .= executionMemory ]

instance FromJSON ExecutionUnits where
  parseJSON =
    Aeson.withObject "ExecutionUnits" $ \o ->
      ExecutionUnits
        <$> o .: "steps"
        <*> o .: "memory"

toAlonzoExUnits :: ExecutionUnits -> Alonzo.ExUnits
toAlonzoExUnits ExecutionUnits{executionSteps, executionMemory} =
  Alonzo.ExUnits {
    Alonzo.exUnitsSteps = executionSteps,
    Alonzo.exUnitsMem   = executionMemory
  }

fromAlonzoExUnits :: Alonzo.ExUnits -> ExecutionUnits
fromAlonzoExUnits Alonzo.ExUnits{Alonzo.exUnitsSteps, Alonzo.exUnitsMem} =
  ExecutionUnits {
    executionSteps  = exUnitsSteps,
    executionMemory = exUnitsMem
  }


-- ----------------------------------------------------------------------------
-- Script Hash
--

-- | We have this type separate from the 'Hash' type to avoid the script
-- hash type being parametrised by the era. The representation is era
-- independent, and there are many places where we want to use a script
-- hash where we don't want things to be era-parametrised.
--
newtype ScriptHash = ScriptHash (Shelley.ScriptHash StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString)   via UsingRawBytesHex ScriptHash
  deriving (ToJSON, FromJSON) via UsingRawBytesHex ScriptHash

instance HasTypeProxy ScriptHash where
    data AsType ScriptHash = AsScriptHash
    proxyToAsType _ = AsScriptHash

instance SerialiseAsRawBytes ScriptHash where
    serialiseToRawBytes (ScriptHash (Shelley.ScriptHash h)) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes AsScriptHash bs =
      maybeToRight (SerialiseAsRawBytesError "Enable to deserialise ScriptHash") $
        ScriptHash . Shelley.ScriptHash <$> Crypto.hashFromBytes bs


hashScript :: Script lang -> ScriptHash
hashScript (SimpleScript s) =
    -- We convert to the Allegra-era version specifically and hash that.
    -- Later ledger eras have to be compatible anyway.
    ScriptHash
  . Ledger.hashScript @(ShelleyLedgerEra AllegraEra)
  . (toAllegraTimelock :: SimpleScript -> Timelock.Timelock StandardCrypto)
  $ s

hashScript (PlutusScript PlutusScriptV1 (PlutusScriptSerialised script)) =
    -- For Plutus V1, we convert to the Alonzo-era version specifically and
    -- hash that. Later ledger eras have to be compatible anyway.
    ScriptHash
  . Ledger.hashScript @(ShelleyLedgerEra AlonzoEra)
  $ Alonzo.PlutusScript Alonzo.PlutusV1 script

hashScript (PlutusScript PlutusScriptV2 (PlutusScriptSerialised script)) =
    ScriptHash
  . Ledger.hashScript @(ShelleyLedgerEra BabbageEra)
  $ Alonzo.PlutusScript Alonzo.PlutusV2 script

toShelleyScriptHash :: ScriptHash -> Shelley.ScriptHash StandardCrypto
toShelleyScriptHash (ScriptHash h) =  h

fromShelleyScriptHash :: Shelley.ScriptHash StandardCrypto -> ScriptHash
fromShelleyScriptHash = ScriptHash


-- ----------------------------------------------------------------------------
-- The simple script language
--

data SimpleScript
  = RequireSignature !(Hash PaymentKey)
  | RequireTimeBefore !SlotNo
  | RequireTimeAfter !SlotNo
  | RequireAllOf ![SimpleScript]
  | RequireAnyOf ![SimpleScript]
  | RequireMOf !Int ![SimpleScript]
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- The Plutus script language
--

-- | Plutus scripts.
--
-- Note that Plutus scripts have a binary serialisation but no JSON
-- serialisation.
--
data PlutusScript lang where
     PlutusScriptSerialised :: ShortByteString -> PlutusScript lang
  deriving stock (Eq, Ord)
  deriving stock (Show) -- TODO: would be nice to use via UsingRawBytesHex
                        -- however that adds an awkward HasTypeProxy lang =>
                        -- constraint to other Show instances elsewhere
  deriving (ToCBOR, FromCBOR) via (UsingRawBytes (PlutusScript lang))
  deriving anyclass SerialiseAsCBOR

instance HasTypeProxy lang => HasTypeProxy (PlutusScript lang) where
    data AsType (PlutusScript lang) = AsPlutusScript (AsType lang)
    proxyToAsType _ = AsPlutusScript (proxyToAsType (Proxy :: Proxy lang))

instance (HasTypeProxy lang, Typeable lang) => SerialiseAsRawBytes (PlutusScript lang) where
    serialiseToRawBytes (PlutusScriptSerialised sbs) = SBS.fromShort sbs

    deserialiseFromRawBytes (AsPlutusScript _) bs =
      -- TODO alonzo: validate the script syntax and fail decoding if invalid
      Right (PlutusScriptSerialised (SBS.toShort bs))

instance (IsPlutusScriptLanguage lang, Typeable lang) =>
         HasTextEnvelope (PlutusScript lang) where
    textEnvelopeType _ =
      case plutusScriptVersion :: PlutusScriptVersion lang of
        PlutusScriptV1 -> "PlutusScriptV1"
        PlutusScriptV2 -> "PlutusScriptV2"


-- | An example Plutus script that always succeeds, irrespective of inputs.
--
-- For example, if one were to use this for a payment address then it would
-- allow anyone to spend from it.
--
-- The exact script depends on the context in which it is to be used.
--
examplePlutusScriptAlwaysSucceeds :: WitCtx witctx
                                  -> PlutusScript PlutusScriptV1
examplePlutusScriptAlwaysSucceeds =
    PlutusScriptSerialised
  . Plutus.alwaysSucceedingNAryFunction
  . scriptArityForWitCtx

-- | An example Plutus script that always fails, irrespective of inputs.
--
-- For example, if one were to use this for a payment address then it would
-- be impossible for anyone to ever spend from it.
--
-- The exact script depends on the context in which it is to be used.
--
examplePlutusScriptAlwaysFails :: WitCtx witctx
                               -> PlutusScript PlutusScriptV1
examplePlutusScriptAlwaysFails =
    PlutusScriptSerialised
  . Plutus.alwaysFailingNAryFunction
  . scriptArityForWitCtx

-- | The expected arity of the Plutus function, depending on the context in
-- which it is used.
--
-- The script inputs consist of
--
-- * the optional datum (for txins)
-- * the redeemer
-- * the Plutus representation of the tx and environment
--
scriptArityForWitCtx :: WitCtx witctx -> Natural
scriptArityForWitCtx WitCtxTxIn  = 3
scriptArityForWitCtx WitCtxMint  = 2
scriptArityForWitCtx WitCtxStake = 2


-- ----------------------------------------------------------------------------
-- Conversion functions
--

toShelleyScript :: ScriptInEra era -> Ledger.Script (ShelleyLedgerEra era)
toShelleyScript (ScriptInEra langInEra (SimpleScript script)) =
    case langInEra of
      SimpleScriptInShelley -> either (error . show) id (toShelleyMultiSig script)
      SimpleScriptInAllegra -> toAllegraTimelock script
      SimpleScriptInMary    -> toAllegraTimelock script
      SimpleScriptInAlonzo  -> Alonzo.TimelockScript (toAllegraTimelock script)
      SimpleScriptInBabbage -> Alonzo.TimelockScript (toAllegraTimelock script)

toShelleyScript (ScriptInEra langInEra (PlutusScript PlutusScriptV1
                                         (PlutusScriptSerialised script))) =
    case langInEra of
      PlutusScriptV1InAlonzo  -> Alonzo.PlutusScript Alonzo.PlutusV1 script
      PlutusScriptV1InBabbage -> Alonzo.PlutusScript Alonzo.PlutusV1 script

toShelleyScript (ScriptInEra langInEra (PlutusScript PlutusScriptV2
                                         (PlutusScriptSerialised script))) =
    case langInEra of
      PlutusScriptV2InBabbage -> Alonzo.PlutusScript Alonzo.PlutusV2 script

fromShelleyBasedScript  :: ShelleyBasedEra era
                        -> Ledger.Script (ShelleyLedgerEra era)
                        -> ScriptInEra era
fromShelleyBasedScript era script =
  case era of
    ShelleyBasedEraShelley ->
      ScriptInEra SimpleScriptInShelley
        . SimpleScript $ fromShelleyMultiSig script
    ShelleyBasedEraAllegra ->
      ScriptInEra SimpleScriptInAllegra
        . SimpleScript $ fromAllegraTimelock script
    ShelleyBasedEraMary ->
      ScriptInEra SimpleScriptInMary
        . SimpleScript $ fromAllegraTimelock script
    ShelleyBasedEraAlonzo ->
      case script of
        Alonzo.TimelockScript s ->
          ScriptInEra SimpleScriptInAlonzo
            . SimpleScript $ fromAllegraTimelock s
        Alonzo.PlutusScript Alonzo.PlutusV1 s ->
          ScriptInEra PlutusScriptV1InAlonzo
            . PlutusScript PlutusScriptV1
            $ PlutusScriptSerialised s
        Alonzo.PlutusScript Alonzo.PlutusV2 _ ->
          error "fromShelleyBasedScript: PlutusV2 not supported in Alonzo era"
    ShelleyBasedEraBabbage ->
      case script of
        Alonzo.TimelockScript s ->
          ScriptInEra SimpleScriptInBabbage
            . SimpleScript $ fromAllegraTimelock  s
        Alonzo.PlutusScript Alonzo.PlutusV1 s ->
          ScriptInEra PlutusScriptV1InBabbage
            . PlutusScript PlutusScriptV1 $ PlutusScriptSerialised s
        Alonzo.PlutusScript Alonzo.PlutusV2 s ->
          ScriptInEra PlutusScriptV2InBabbage
            . PlutusScript PlutusScriptV2 $ PlutusScriptSerialised s


data MultiSigError = MultiSigErrorTimelockNotsupported deriving Show

-- | Conversion for the 'Shelley.MultiSig' language used by the Shelley era.
--
toShelleyMultiSig :: SimpleScript -> Either MultiSigError (Shelley.MultiSig StandardCrypto)
toShelleyMultiSig = go
  where
    go :: SimpleScript -> Either MultiSigError (Shelley.MultiSig StandardCrypto)
    go (RequireSignature (PaymentKeyHash kh)) =
      return $ Shelley.RequireSignature (Shelley.coerceKeyRole kh)
    go (RequireAllOf s) = mapM go s <&> Shelley.RequireAllOf
    go (RequireAnyOf s) = mapM go s <&> Shelley.RequireAnyOf
    go (RequireMOf m s) = mapM go s <&> Shelley.RequireMOf m
    go _ = Left MultiSigErrorTimelockNotsupported

-- | Conversion for the 'Shelley.MultiSig' language used by the Shelley era.
--
fromShelleyMultiSig :: Shelley.MultiSig StandardCrypto -> SimpleScript
fromShelleyMultiSig = go
  where
    go (Shelley.RequireSignature kh)
                                = RequireSignature
                                    (PaymentKeyHash (Shelley.coerceKeyRole kh))
    go (Shelley.RequireAllOf s) = RequireAllOf (map go s)
    go (Shelley.RequireAnyOf s) = RequireAnyOf (map go s)
    go (Shelley.RequireMOf m s) = RequireMOf m (map go s)

-- | Conversion for the 'Timelock.Timelock' language that is shared between the
-- Allegra and Mary eras.
--
toAllegraTimelock :: SimpleScript -> Timelock.Timelock StandardCrypto
toAllegraTimelock = go
  where
    go :: SimpleScript -> Timelock.Timelock StandardCrypto
    go (RequireSignature (PaymentKeyHash kh))
                        = Timelock.RequireSignature (Shelley.coerceKeyRole kh)
    go (RequireAllOf s) = Timelock.RequireAllOf (Seq.fromList (map go s))
    go (RequireAnyOf s) = Timelock.RequireAnyOf (Seq.fromList (map go s))
    go (RequireMOf m s) = Timelock.RequireMOf m (Seq.fromList (map go s))
    go (RequireTimeBefore t) = Timelock.RequireTimeExpire t
    go (RequireTimeAfter  t) = Timelock.RequireTimeStart  t

-- | Conversion for the 'Timelock.Timelock' language that is shared between the
-- Allegra and Mary eras.
--
fromAllegraTimelock :: Timelock.Timelock StandardCrypto -> SimpleScript
fromAllegraTimelock = go
  where
    go (Timelock.RequireSignature kh) = RequireSignature
                                          (PaymentKeyHash (Shelley.coerceKeyRole kh))
    go (Timelock.RequireTimeExpire t) = RequireTimeBefore t
    go (Timelock.RequireTimeStart  t) = RequireTimeAfter t
    go (Timelock.RequireAllOf      s) = RequireAllOf (map go (toList s))
    go (Timelock.RequireAnyOf      s) = RequireAnyOf (map go (toList s))
    go (Timelock.RequireMOf      i s) = RequireMOf i (map go (toList s))


-- ----------------------------------------------------------------------------
-- JSON serialisation
--

-- Remember that Plutus scripts do not have a JSON syntax, and so do not have
-- and JSON instances. The only JSON format they support is via the
-- HasTextEnvelope class which just wraps the binary format.
--
-- Because of this the 'Script' type also does not have any JSON instances, but
-- the 'SimpleScript' type does.

instance ToJSON SimpleScript where
  toJSON (RequireSignature pKeyHash) =
    object [ "type"    .= String "sig"
           , "keyHash" .= serialiseToRawBytesHexText pKeyHash
           ]
  toJSON (RequireTimeBefore slot) =
    object [ "type" .= String "before"
           , "slot" .= slot
           ]
  toJSON (RequireTimeAfter slot) =
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


instance FromJSON SimpleScript where
  parseJSON = parseSimpleScript

-- TODO: Left off here. You need to comb through cardano-api's property tests concerning simple scripts
parseSimpleScript :: Value -> Aeson.Parser SimpleScript
parseSimpleScript v = parseScriptSig v <|>
                      parseScriptBefore v <|>
                      parseScriptAfter v <|>
                      parseScriptAny v <|>
                      parseScriptAll v <|>
                      parseScriptAtLeast v

parseScriptAny :: Value -> Aeson.Parser SimpleScript
parseScriptAny =
    Aeson.withObject "any" $ \obj -> do
      t <- obj .: "type"
      case t :: Text of
        "any" -> do vs <- obj .: "scripts"
                    RequireAnyOf <$> gatherSimpleScriptTerms vs
        _ -> fail "\"any\" script value not found"

parseScriptAll :: Value -> Aeson.Parser SimpleScript
parseScriptAll =
    Aeson.withObject "all" $ \obj -> do
      t <- obj .: "type"
      case t :: Text of
        "all" -> do vs <- obj .: "scripts"
                    RequireAllOf <$> gatherSimpleScriptTerms vs
        _ -> fail "\"all\" script value not found"

parseScriptAtLeast :: Value -> Aeson.Parser SimpleScript
parseScriptAtLeast =
    Aeson.withObject "atLeast" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "atLeast" -> do
          r  <- obj .: "required"
          vs <- obj .: "scripts"
          case r of
            Number sci ->
              case toBoundedInteger sci of
                Just reqInt ->
                  do scripts <- gatherSimpleScriptTerms vs
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

gatherSimpleScriptTerms :: Vector Value -> Aeson.Parser [SimpleScript]
gatherSimpleScriptTerms = mapM parseSimpleScript . Vector.toList

parseScriptSig :: Value -> Aeson.Parser SimpleScript
parseScriptSig =
    Aeson.withObject "sig" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "sig" -> do k <- obj .: "keyHash"
                    RequireSignature <$> parsePaymentKeyHash k
        _     -> fail "\"sig\" script value not found"

parseScriptBefore :: Value -> Aeson.Parser SimpleScript
parseScriptBefore =
    Aeson.withObject "before" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "before" ->  RequireTimeBefore <$> obj .: "slot"
        _ -> fail "\"before\" script value not found"

parseScriptAfter :: Value -> Aeson.Parser SimpleScript
parseScriptAfter =
    Aeson.withObject "after" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "after" -> RequireTimeAfter <$> obj .: "slot"
        _       -> fail "\"after\" script value not found"

parsePaymentKeyHash :: Text -> Aeson.Parser (Hash PaymentKey)
parsePaymentKeyHash =
  failEitherWith
    (\e -> "Error deserialising payment key hash: " ++ displayError e)
  . deserialiseFromRawBytesHex (AsHash AsPaymentKey)
  . Text.encodeUtf8


-- ----------------------------------------------------------------------------
-- Reference scripts
--

-- | A reference scripts is a script that can exist at a transaction output. This greatly
-- reduces the size of transactions that use scripts as the script no longer
-- has to be added to the transaction, they can now be referenced via a transaction output.

data ReferenceScript era where
     ReferenceScript :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era
                     -> ScriptInAnyLang
                     -> ReferenceScript era

     ReferenceScriptNone :: ReferenceScript era

deriving instance Eq (ReferenceScript era)
deriving instance Show (ReferenceScript era)
deriving instance Typeable (ReferenceScript era)

instance IsCardanoEra era => ToJSON (ReferenceScript era) where
  toJSON (ReferenceScript _ s) = object ["referenceScript" .= s]
  toJSON ReferenceScriptNone = Aeson.Null

instance IsCardanoEra era => FromJSON (ReferenceScript era) where
  parseJSON = Aeson.withObject "ReferenceScript" $ \o ->
    case refInsScriptsAndInlineDatsSupportedInEra (cardanoEra :: CardanoEra era) of
      Nothing -> pure ReferenceScriptNone
      Just refSupInEra ->
        ReferenceScript refSupInEra <$> o .: "referenceScript"

instance EraCast ReferenceScript where
  eraCast toEra = \case
    ReferenceScriptNone -> pure ReferenceScriptNone
    v@(ReferenceScript (_ :: ReferenceTxInsScriptsInlineDatumsSupportedInEra fromEra) scriptInAnyLang) ->
      case refInsScriptsAndInlineDatsSupportedInEra toEra of
        Nothing -> Left $ EraCastError v (cardanoEra @fromEra) toEra
        Just supportedInEra -> Right $ ReferenceScript supportedInEra scriptInAnyLang

data ReferenceTxInsScriptsInlineDatumsSupportedInEra era where
    ReferenceTxInsScriptsInlineDatumsInBabbageEra :: ReferenceTxInsScriptsInlineDatumsSupportedInEra BabbageEra

deriving instance Eq (ReferenceTxInsScriptsInlineDatumsSupportedInEra era)
deriving instance Show (ReferenceTxInsScriptsInlineDatumsSupportedInEra era)

refInsScriptsAndInlineDatsSupportedInEra
  :: CardanoEra era -> Maybe (ReferenceTxInsScriptsInlineDatumsSupportedInEra era)
refInsScriptsAndInlineDatsSupportedInEra ByronEra   = Nothing
refInsScriptsAndInlineDatsSupportedInEra ShelleyEra = Nothing
refInsScriptsAndInlineDatsSupportedInEra AllegraEra = Nothing
refInsScriptsAndInlineDatsSupportedInEra MaryEra    = Nothing
refInsScriptsAndInlineDatsSupportedInEra AlonzoEra  = Nothing
refInsScriptsAndInlineDatsSupportedInEra BabbageEra = Just ReferenceTxInsScriptsInlineDatumsInBabbageEra

refScriptToShelleyScript
  :: CardanoEra era
  -> ReferenceScript era
  -> StrictMaybe (Ledger.Script (ShelleyLedgerEra era))
refScriptToShelleyScript era (ReferenceScript _ s) =
  case toScriptInEra era s of
    Just sInEra -> SJust $ toShelleyScript sInEra
    Nothing -> SNothing
refScriptToShelleyScript _ ReferenceScriptNone = SNothing

fromShelleyScriptToReferenceScript
  :: ShelleyBasedEra era -> Ledger.Script (ShelleyLedgerEra era) -> ReferenceScript era
fromShelleyScriptToReferenceScript sbe script =
   scriptInEraToRefScript $ fromShelleyBasedScript sbe script

scriptInEraToRefScript :: ScriptInEra era -> ReferenceScript era
scriptInEraToRefScript sIne@(ScriptInEra _ s) =
  case refInsScriptsAndInlineDatsSupportedInEra era of
    Nothing -> ReferenceScriptNone
    Just supp ->
      -- Any script can be a reference script
      ReferenceScript supp $ toScriptInAnyLang s
 where
  era = shelleyBasedToCardanoEra $ eraOfScriptInEra sIne

-- Helpers

textEnvelopeToScript :: TextEnvelope -> Either TextEnvelopeError ScriptInAnyLang
textEnvelopeToScript = deserialiseFromTextEnvelopeAnyOf textEnvTypes
 where
  textEnvTypes :: [FromSomeType HasTextEnvelope ScriptInAnyLang]
  textEnvTypes =
    [ FromSomeType (AsScript AsSimpleScript)
                   (ScriptInAnyLang SimpleScriptLanguage)
    , FromSomeType (AsScript AsPlutusScriptV1)
                   (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1))
    , FromSomeType (AsScript AsPlutusScriptV2)
                   (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2))
    ]
