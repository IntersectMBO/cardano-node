{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Complete, signed transactions
--
module Cardano.Api.Tx (

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(..),
    getTxBody,
    getTxWitnesses,

    -- ** Signing in one go
    ShelleySigningKey(..),
    toShelleySigningKey,
    signByronTransaction,
    signShelleyTransaction,
    -- ** Incremental signing and separate witnesses
    makeSignedTransaction,
    Witness(..),
    makeByronKeyWitness,
    ShelleyWitnessSigningKey(..),
    makeShelleyKeyWitness,
    WitnessNetworkIdOrByronAddress (..),
    makeShelleyBootstrapWitness,
    makeScriptWitness,
    makeShelleySignature,
    getShelleyKeyWitnessVerificationKey,

    -- * Data family instances
    AsType(AsTx, AsByronTx, AsShelleyTx, AsByronWitness, AsShelleyWitness),
  ) where

import           Prelude

import           Data.Maybe

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

--
-- Common types, consensus, network
--
import           Cardano.Binary (Annotated (..))
import qualified Cardano.Binary as CBOR
import qualified Cardano.Prelude as CBOR (cborError)

--
-- Crypto API used by consensus and Shelley (and should be used by Byron)
--
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Util as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD

--
-- Byron imports
--
import qualified Cardano.Crypto.Hashing as Byron
import qualified Cardano.Crypto.ProtocolMagic as Byron
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron

--
-- Shelley imports
--
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import qualified Cardano.Ledger.Core as Ledger (Script)

import qualified Shelley.Spec.Ledger.Address.Bootstrap as Shelley
import           Shelley.Spec.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Shelley.Spec.Ledger.Hashing as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Scripts as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley

import qualified Cardano.Api.Shelley.Serialisation.Legacy as Legacy

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Key
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysShelley
import           Cardano.Api.NetworkId
import           Cardano.Api.Script
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.TxBody


-- ----------------------------------------------------------------------------
-- Signed transactions
--

data Tx era where

     ByronTx
       :: Byron.ATxAux ByteString
       -> Tx ByronEra

     ShelleyTx
       :: ShelleyBasedEra era
       -> Shelley.Tx (ShelleyLedgerEra era)
       -> Tx era

-- The GADT in the ShelleyTx case requires a custom instance
instance Eq (Tx era) where
    (==) (ByronTx txA)
         (ByronTx txB) = txA == txB

    (==) (ShelleyTx era txA)
         (ShelleyTx _   txB) =
      case era of
        ShelleyBasedEraShelley -> txA == txB
        ShelleyBasedEraAllegra -> txA == txB
        ShelleyBasedEraMary    -> txA == txB

    (==) (ByronTx{}) (ShelleyTx era _) = case era of {}

-- The GADT in the ShelleyTx case requires a custom instance
instance Show (Tx era) where
    showsPrec p (ByronTx tx) =
      showParen (p >= 11) $
        showString "ByronTx "
      . showsPrec 11 tx

    showsPrec p (ShelleyTx ShelleyBasedEraShelley tx) =
      showParen (p >= 11) $
        showString "ShelleyTx ShelleyBasedEraShelley "
      . showsPrec 11 tx

    showsPrec p (ShelleyTx ShelleyBasedEraAllegra tx) =
      showParen (p >= 11) $
        showString "ShelleyTx ShelleyBasedEraAllegra "
      . showsPrec 11 tx

    showsPrec p (ShelleyTx ShelleyBasedEraMary tx) =
      showParen (p >= 11) $
        showString "ShelleyTx ShelleyBasedEraMary "
      . showsPrec 11 tx


instance HasTypeProxy era => HasTypeProxy (Tx era) where
    data AsType (Tx era) = AsTx (AsType era)
    proxyToAsType _ = AsTx (proxyToAsType (Proxy :: Proxy era))

pattern AsByronTx :: AsType (Tx ByronEra)
pattern AsByronTx   = AsTx AsByronEra
{-# COMPLETE AsByronTx #-}

pattern AsShelleyTx :: AsType (Tx ShelleyEra)
pattern AsShelleyTx = AsTx AsShelleyEra
{-# COMPLETE AsShelleyTx #-}


instance IsCardanoEra era => SerialiseAsCBOR (Tx era) where
    serialiseToCBOR (ByronTx tx) = CBOR.recoverBytes tx

    serialiseToCBOR (ShelleyTx era tx) =
      case era of
        ShelleyBasedEraShelley -> serialiseShelleyBasedTx tx
        ShelleyBasedEraAllegra -> serialiseShelleyBasedTx tx
        ShelleyBasedEraMary    -> serialiseShelleyBasedTx tx

    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronTx <$>
            CBOR.decodeFullAnnotatedBytes
              "Byron Tx" fromCBOR (LBS.fromStrict bs)

        -- Use the same derialisation impl, but at different types:
        ShelleyEra -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraShelley) bs
        AllegraEra -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraAllegra) bs
        MaryEra    -> deserialiseShelleyBasedTx
                        (ShelleyTx ShelleyBasedEraMary) bs

-- | The serialisation format for the different Shelley-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
--
serialiseShelleyBasedTx :: ToCBOR tx => tx -> ByteString
serialiseShelleyBasedTx = CBOR.serialize'

deserialiseShelleyBasedTx :: FromCBOR (CBOR.Annotator tx)
                          => (tx -> tx')
                          -> ByteString
                          -> Either CBOR.DecoderError tx'
deserialiseShelleyBasedTx mkTx bs =
    mkTx <$> CBOR.decodeAnnotator "Shelley Tx" fromCBOR (LBS.fromStrict bs)


instance IsCardanoEra era => HasTextEnvelope (Tx era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> "TxSignedByron"
        ShelleyEra -> "TxSignedShelley"
        AllegraEra -> "Tx AllegraEra"
        MaryEra    -> "Tx MaryEra"


data Witness era where

     ByronKeyWitness
       :: Byron.TxInWitness
       -> Witness ByronEra

     ShelleyBootstrapWitness
       :: ShelleyBasedEra era
       -> Shelley.BootstrapWitness (ShelleyLedgerEra era)
       -> Witness era

     ShelleyKeyWitness
       :: ShelleyBasedEra era
       -> Shelley.WitVKey Shelley.Witness (ShelleyLedgerEra era)
       -> Witness era

     ShelleyScriptWitness
       :: ShelleyBasedEra era
       -> Ledger.Script (ShelleyLedgerEra era)
       -> Witness era

{-
deriving instance Eq (Witness ByronEra)
deriving instance Show (Witness ByronEra)

deriving instance Eq (Witness ShelleyEra)
deriving instance Show (Witness ShelleyEra)
-}

instance HasTypeProxy (Witness ByronEra) where
    data AsType (Witness ByronEra) = AsByronWitness
    proxyToAsType _ = AsByronWitness

instance HasTypeProxy (Witness ShelleyEra) where
    data AsType (Witness ShelleyEra) = AsShelleyWitness
    proxyToAsType _ = AsShelleyWitness

instance SerialiseAsCBOR (Witness ByronEra) where
    serialiseToCBOR (ShelleyBootstrapWitness era _) = case era of {}
    serialiseToCBOR (ShelleyKeyWitness era _) = case era of {}
    serialiseToCBOR (ShelleyScriptWitness era _) = case era of {}
    serialiseToCBOR (ByronKeyWitness wit) = CBOR.serialize' wit

    deserialiseFromCBOR AsByronWitness bs =
      ByronKeyWitness <$> CBOR.decodeFull' bs

instance SerialiseAsCBOR (Witness ShelleyEra) where
    serialiseToCBOR = CBOR.serializeEncoding' . encodeShelleyWitness
      where
        encodeShelleyWitness :: Witness ShelleyEra -> CBOR.Encoding
        encodeShelleyWitness (ShelleyKeyWitness _ wit) =
            CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> toCBOR wit
        encodeShelleyWitness (ShelleyBootstrapWitness _ wit) =
            CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> toCBOR wit
        encodeShelleyWitness (ShelleyScriptWitness _ wit) =
            CBOR.encodeListLen 2
              <> CBOR.encodeWord 2
              -- We use 'WrappedMultiSig' here to support the legacy
              -- binary serialisation format for the @Script@ type from
              -- @cardano-ledger-specs@.
              --
              -- See the documentation of 'WrappedMultiSig' for more
              -- information.
              <> toCBOR (Legacy.WrappedMultiSig wit)

    deserialiseFromCBOR AsShelleyWitness bs =
        CBOR.decodeAnnotator "Shelley Witness"
                             decodeShelleyWitness (LBS.fromStrict bs)
      where
        decodeShelleyWitness :: CBOR.Decoder s (CBOR.Annotator (Witness ShelleyEra))
        decodeShelleyWitness =  do
          CBOR.decodeListLenOf 2
          t <- CBOR.decodeWord
          case t of
            0 -> fmap (fmap (ShelleyKeyWitness ShelleyBasedEraShelley)) fromCBOR
            1 -> fmap (fmap (ShelleyBootstrapWitness ShelleyBasedEraShelley)) fromCBOR
            -- We use 'WrappedMultiSig' here to support the legacy binary
            -- serialisation format for the @Script@ type from
            -- @cardano-ledger-specs@.
            --
            -- See the documentation of 'WrappedMultiSig' for more
            -- information.
            2 -> fmap (fmap (ShelleyScriptWitness ShelleyBasedEraShelley . Legacy.unWrappedMultiSig)) fromCBOR
            _ -> CBOR.cborError $ CBOR.DecoderErrorUnknownTag
                                    "Shelley Witness" (fromIntegral t)

instance HasTextEnvelope (Witness ByronEra) where
    textEnvelopeType _ = "TxWitnessByron"

instance HasTextEnvelope (Witness ShelleyEra) where
    textEnvelopeType _ = "TxWitnessShelley"


getTxBody :: Tx era -> TxBody era
getTxBody (ByronTx Byron.ATxAux { Byron.aTaTx = txbody }) =
    ByronTxBody txbody

getTxBody (ShelleyTx ShelleyBasedEraShelley Shelley.Tx {
                       Shelley._body     = txbody,
                       Shelley._metadata = txmetadata
                     }) =
    ShelleyTxBody ShelleyBasedEraShelley txbody (strictMaybeToMaybe txmetadata)

getTxBody (ShelleyTx ShelleyBasedEraAllegra _) =
    error "TODO: getTxBody AllegraEra"
getTxBody (ShelleyTx ShelleyBasedEraMary _) =
    error "TODO: getTxBody MaryEra"


getTxWitnesses :: Tx era -> [Witness era]
getTxWitnesses (ByronTx Byron.ATxAux { Byron.aTaWitness = witnesses }) =
    map ByronKeyWitness
  . Vector.toList
  . unAnnotated
  $ witnesses

getTxWitnesses (ShelleyTx ShelleyBasedEraShelley Shelley.Tx {
                       Shelley._witnessSet =
                         Shelley.WitnessSet
                           addrWits
                           msigWits
                           bootWits
                     }) =
    map (ShelleyBootstrapWitness ShelleyBasedEraShelley) (Set.elems bootWits)
 ++ map (ShelleyKeyWitness       ShelleyBasedEraShelley) (Set.elems addrWits)
 ++ map (ShelleyScriptWitness    ShelleyBasedEraShelley) (Map.elems msigWits)

getTxWitnesses (ShelleyTx ShelleyBasedEraAllegra _) =
    error "TODO: getTxWitnesses AllegraEra"
getTxWitnesses (ShelleyTx ShelleyBasedEraMary _) =
    error "TODO: getTxWitnesses MaryEra"


makeSignedTransaction :: [Witness era]
                      -> TxBody era
                      -> Tx era
makeSignedTransaction witnesses (ByronTxBody txbody) =
    ByronTx
  . Byron.annotateTxAux
  $ Byron.mkTxAux
      (unAnnotated txbody)
      (Vector.fromList (map selectByronWitness witnesses))
  where
    selectByronWitness :: Witness ByronEra -> Byron.TxInWitness
    selectByronWitness (ByronKeyWitness w) = w
    selectByronWitness (ShelleyBootstrapWitness era _) = case era of {}
    selectByronWitness (ShelleyKeyWitness       era _) = case era of {}
    selectByronWitness (ShelleyScriptWitness    era _) = case era of {}

makeSignedTransaction witnesses (ShelleyTxBody ShelleyBasedEraShelley txbody txmetadata) =
    ShelleyTx ShelleyBasedEraShelley $
      Shelley.Tx
        txbody
        (Shelley.WitnessSet
          (Set.fromList [ w | ShelleyKeyWitness ShelleyBasedEraShelley w <- witnesses ])
          (Map.fromList [ (Shelley.hashMultiSigScript sw, sw)
                        | ShelleyScriptWitness ShelleyBasedEraShelley sw <- witnesses ])
          (Set.fromList [ w | ShelleyBootstrapWitness ShelleyBasedEraShelley w <- witnesses ]))
        (maybeToStrictMaybe txmetadata)
makeSignedTransaction _ (ShelleyTxBody ShelleyBasedEraAllegra _ _) =
    error "TODO: makeSignedTransaction AllegraEra"
makeSignedTransaction _ (ShelleyTxBody ShelleyBasedEraMary _ _) =
    error "TODO: makeSignedTransaction MaryEra"

makeByronKeyWitness :: NetworkId
                    -> TxBody ByronEra
                    -> SigningKey ByronKey
                    -> Witness ByronEra
makeByronKeyWitness _ (ShelleyTxBody era _ _) = case era of {}
makeByronKeyWitness nw (ByronTxBody txbody) =
    let txhash :: Byron.Hash Byron.Tx
        txhash = Byron.hashDecoded txbody

        pm :: Byron.ProtocolMagicId
        pm = toByronProtocolMagicId nw

        -- To allow sharing of the txhash computation across many signatures we
        -- define and share the txhash outside the lambda for the signing key:
     in \(ByronSigningKey sk) ->
        ByronKeyWitness $
          Byron.VKWitness
            (Byron.toVerification sk)
            (Byron.sign pm Byron.SignTx sk (Byron.TxSigData txhash))

-- | Either a network ID or a Byron address to be used in constructing a
-- Shelley bootstrap witness.
data WitnessNetworkIdOrByronAddress
  = WitnessNetworkId !NetworkId
  -- ^ Network ID.
  --
  -- If this value is used in the construction of a Shelley bootstrap witness,
  -- the result will not consist of a derivation path. If that is required,
  -- specify a 'WitnessByronAddress' value instead.
  | WitnessByronAddress !(Address ByronAddr)
  -- ^ Byron address.
  --
  -- If this value is used in the construction of a Shelley bootstrap witness,
  -- both the network ID and derivation path will be extracted from the
  -- address and used in the construction of the witness.

makeShelleyBootstrapWitness :: WitnessNetworkIdOrByronAddress
                            -> TxBody era
                            -> SigningKey ByronKey
                            -> Witness era
makeShelleyBootstrapWitness nwOrAddr
                            (ShelleyTxBody ShelleyBasedEraShelley txbody _)
                            (ByronSigningKey sk) =
    ShelleyBootstrapWitness ShelleyBasedEraShelley $
      -- Byron era witnesses were weird. This reveals all that weirdness.
      Shelley.BootstrapWitness {
        Shelley.bwKey        = vk,
        Shelley.bwSig        = signature,
        Shelley.bwChainCode  = chainCode,
        Shelley.bwAttributes = attributes
      }
  where
    -- Starting with the easy bits: we /can/ convert the Byron verification key
    -- to a the pair of a Shelley verification key plus the chain code.
    --
    (vk, chainCode) = Shelley.unpackByronVKey (Byron.toVerification sk)

    -- Now the hairy bits.
    --
    -- Byron era signing keys were all /extended/ ed25519 keys. We have to
    -- produce a signature using this extended signing key directly. They
    -- /cannot/ be converted to a plain (non-extended) signing keys. Since we
    -- now support extended signing keys for the Shelley too, we are able to
    -- reuse that here.
    --
    signature :: Shelley.SignedDSIGN StandardCrypto
                  (Shelley.Hash StandardCrypto Shelley.EraIndependentTxBody)
    signature = makeShelleySignature
                  txhash
                  -- Make the signature with the extended key directly:
                  (ShelleyExtendedSigningKey (Byron.unSigningKey sk))

    txhash :: Shelley.Hash StandardCrypto Shelley.EraIndependentTxBody
    txhash = Shelley.eraIndTxBodyHash txbody

    -- And finally we need to provide the extra suffix bytes necessary to
    -- reconstruct the mini-Merkel tree that is a Byron address. The suffix
    -- bytes are the serialised address attributes.
    attributes =
      CBOR.serialize' $
        Byron.mkAttributes Byron.AddrAttributes {
          Byron.aaVKDerivationPath = derivationPath,
          Byron.aaNetworkMagic     = networkMagic
        }

    -- The 'WitnessNetworkIdOrByronAddress' value converted to an 'Either'.
    eitherNwOrAddr :: Either NetworkId (Address ByronAddr)
    eitherNwOrAddr =
      case nwOrAddr of
        WitnessNetworkId nw -> Left nw
        WitnessByronAddress addr -> Right addr

    unByronAddr :: Address ByronAddr -> Byron.Address
    unByronAddr (ByronAddress addr) = addr

    unAddrAttrs :: Address ByronAddr -> Byron.AddrAttributes
    unAddrAttrs = Byron.attrData . Byron.addrAttributes . unByronAddr

    derivationPath :: Maybe Byron.HDAddressPayload
    derivationPath =
      either
        (const Nothing)
        (Byron.aaVKDerivationPath . unAddrAttrs)
        eitherNwOrAddr

    networkMagic :: Byron.NetworkMagic
    networkMagic =
      either
        toByronNetworkMagic
        (Byron.aaNetworkMagic . unAddrAttrs)
        eitherNwOrAddr

makeShelleyBootstrapWitness _ (ShelleyTxBody ShelleyBasedEraAllegra _ _) _ =
    error "TODO: makeShelleyBootstrapWitness AllegraEra"
makeShelleyBootstrapWitness _ (ShelleyTxBody ShelleyBasedEraMary _ _) _ =
    error "TODO: makeShelleyBootstrapWitness MaryEra"
makeShelleyBootstrapWitness _ ByronTxBody{} _ =
    error "TODO: makeShelleyBootstrapWitness ByronEra"


data ShelleyWitnessSigningKey =
       WitnessPaymentKey         (SigningKey PaymentKey)
     | WitnessPaymentExtendedKey (SigningKey PaymentExtendedKey)
     | WitnessStakeKey           (SigningKey StakeKey)
     | WitnessStakeExtendedKey   (SigningKey StakeExtendedKey)
     | WitnessStakePoolKey       (SigningKey StakePoolKey)
     | WitnessGenesisKey         (SigningKey GenesisKey)
     | WitnessGenesisExtendedKey (SigningKey GenesisExtendedKey)
     | WitnessGenesisDelegateKey (SigningKey GenesisDelegateKey)
     | WitnessGenesisDelegateExtendedKey
                                 (SigningKey GenesisDelegateExtendedKey)
     | WitnessGenesisUTxOKey     (SigningKey GenesisUTxOKey)


makeShelleyKeyWitness :: TxBody era
                      -> ShelleyWitnessSigningKey
                      -> Witness era
makeShelleyKeyWitness (ShelleyTxBody ShelleyBasedEraShelley txbody _) =
    let txhash :: Shelley.Hash StandardCrypto Shelley.EraIndependentTxBody
        txhash = Shelley.hashAnnotated txbody

        -- To allow sharing of the txhash computation across many signatures we
        -- define and share the txhash outside the lambda for the signing key:
     in \wsk ->
        let sk        = toShelleySigningKey wsk
            vk        = getShelleyKeyWitnessVerificationKey sk
            signature = makeShelleySignature txhash sk
         in ShelleyKeyWitness ShelleyBasedEraShelley $
              Shelley.WitVKey vk signature
makeShelleyKeyWitness (ShelleyTxBody ShelleyBasedEraAllegra _ _) =
    error "TODO: makeShelleyKeyWitness AllegraEra"
makeShelleyKeyWitness (ShelleyTxBody ShelleyBasedEraMary _ _) =
    error "TODO: makeShelleyKeyWitness MaryEra"
makeShelleyKeyWitness ByronTxBody{} =
    error "TODO: makeShelleyKeyWitness ByronEra"


-- | We support making key witnesses with both normal and extended signing keys.
--
data ShelleySigningKey =
       -- | A normal ed25519 signing key
       ShelleyNormalSigningKey   (Shelley.SignKeyDSIGN StandardCrypto)

       -- | An extended ed25519 signing key
     | ShelleyExtendedSigningKey Crypto.HD.XPrv


toShelleySigningKey :: ShelleyWitnessSigningKey -> ShelleySigningKey
toShelleySigningKey key = case key of
  WitnessPaymentKey     (PaymentSigningKey     sk) -> ShelleyNormalSigningKey sk
  WitnessStakeKey       (StakeSigningKey       sk) -> ShelleyNormalSigningKey sk
  WitnessStakePoolKey   (StakePoolSigningKey   sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisKey     (GenesisSigningKey     sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisUTxOKey (GenesisUTxOSigningKey sk) -> ShelleyNormalSigningKey sk
  WitnessGenesisDelegateKey (GenesisDelegateSigningKey sk) ->
    ShelleyNormalSigningKey sk

  -- The cases for extended keys
  WitnessPaymentExtendedKey (PaymentExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessStakeExtendedKey (StakeExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessGenesisExtendedKey (GenesisExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk

  WitnessGenesisDelegateExtendedKey (GenesisDelegateExtendedSigningKey sk) ->
    ShelleyExtendedSigningKey sk


getShelleyKeyWitnessVerificationKey
  :: ShelleySigningKey
  -> Shelley.VKey Shelley.Witness StandardCrypto
getShelleyKeyWitnessVerificationKey (ShelleyNormalSigningKey sk) =
      (Shelley.coerceKeyRole :: Shelley.VKey Shelley.Payment StandardCrypto
                             -> Shelley.VKey Shelley.Witness StandardCrypto)
    . (\(PaymentVerificationKey vk) -> vk)
    . getVerificationKey
    . PaymentSigningKey
    $ sk

getShelleyKeyWitnessVerificationKey (ShelleyExtendedSigningKey sk) =
      (Shelley.coerceKeyRole :: Shelley.VKey Shelley.Payment StandardCrypto
                             -> Shelley.VKey Shelley.Witness StandardCrypto)
    . (\(PaymentVerificationKey vk) -> vk)
    . (castVerificationKey :: VerificationKey PaymentExtendedKey
                           -> VerificationKey PaymentKey)
    . getVerificationKey
    . PaymentExtendedSigningKey
    $ sk


makeShelleySignature
  :: Crypto.SignableRepresentation tosign
  => tosign
  -> ShelleySigningKey
  -> Shelley.SignedDSIGN StandardCrypto tosign
makeShelleySignature tosign (ShelleyNormalSigningKey sk) =
    Crypto.signedDSIGN () tosign sk

makeShelleySignature tosign (ShelleyExtendedSigningKey sk) =
    fromXSignature $
      Crypto.HD.sign
        BS.empty  -- passphrase for (unused) in-memory encryption
        sk
        (Crypto.getSignableRepresentation tosign)
  where
    fromXSignature :: Crypto.HD.XSignature
                   -> Shelley.SignedDSIGN StandardCrypto b
    fromXSignature =
        Crypto.SignedDSIGN
      . fromMaybe impossible
      . Crypto.rawDeserialiseSigDSIGN
      . Crypto.HD.unXSignature

    impossible =
      error "makeShelleyKeyWitnessSignature: byron and shelley signature sizes do not match"


makeScriptWitness :: forall era. Script era -> Witness era
makeScriptWitness (ShelleyScript s) = ShelleyScriptWitness ShelleyBasedEraShelley s
makeScriptWitness (AllegraScript s) = ShelleyScriptWitness ShelleyBasedEraAllegra s
makeScriptWitness (MaryScript    s) = ShelleyScriptWitness ShelleyBasedEraMary    s

-- order of signing keys must match txins
signByronTransaction :: NetworkId
                     -> TxBody ByronEra
                     -> [SigningKey ByronKey]
                     -> Tx ByronEra
signByronTransaction nw txbody sks =
    makeSignedTransaction witnesses txbody
  where
    witnesses = map (makeByronKeyWitness nw txbody) sks

-- signing keys is a set
signShelleyTransaction :: TxBody era
                       -> [ShelleyWitnessSigningKey]
                       -> Tx era
signShelleyTransaction txbody sks =
    makeSignedTransaction witnesses txbody
  where
    witnesses = map (makeShelleyKeyWitness txbody) sks

