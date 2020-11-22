{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}


-- | Transaction bodies
--
module Cardano.Api.TxBody (

    -- * Transaction Ids
    TxId(..),
    getTxId,

    -- * Transaction inputs and outputs
    TxIn(..),
    TxIx(..),
    TxOut(..),
    TxOutValue(..),
    AdaOnlyInEra(..),
    MultiAssetInEra(..),
    genesisUTxOPseudoTxIn,

    -- * Transaction bodies
    TxBody(..),
    TTL,
    TxFee,
    MintValue(..),
    makeByronTransaction,
    makeShelleyTransaction,
    TxExtraContent(..),
    txExtraContentEmpty,

    -- * Data family instances
    AsType(AsTxId, AsTxBody, AsByronTxBody, AsShelleyTxBody),
  ) where

import           Prelude

import qualified Data.List.NonEmpty as NonEmpty
import           Data.String (IsString)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set

import           Cardano.Binary (Annotated (..), reAnnotate, recoverBytes)
import qualified Cardano.Binary as CBOR
import qualified Shelley.Spec.Ledger.Serialization as CBOR
                   (decodeNullMaybe, encodeNullMaybe)

import           Cardano.Slotting.Slot (SlotNo (..))
import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Cardano.Crypto.Hashing as Byron
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron

import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Shelley as Ledger
import qualified Cardano.Ledger.ShelleyMA.TxBody ()
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.BaseTypes (maybeToStrictMaybe)
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.MetaData as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysShelley
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.TxMetadata
import           Cardano.Api.Utils
import           Cardano.Api.Value


-- ----------------------------------------------------------------------------
-- Transaction Ids
--

newtype TxId = TxId (Shelley.Hash StandardCrypto ())
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)
               -- We use the Shelley representation and convert the Byron one

instance HasTypeProxy TxId where
    data AsType TxId = AsTxId
    proxyToAsType _ = AsTxId

instance SerialiseAsRawBytes TxId where
    serialiseToRawBytes (TxId h) = Crypto.hashToBytes h
    deserialiseFromRawBytes AsTxId bs = TxId <$> Crypto.hashFromBytes bs

toByronTxId :: TxId -> Byron.TxId
toByronTxId (TxId h) =
    Byron.unsafeHashFromBytes (Crypto.hashToBytes h)

toShelleyTxId :: Ledger.Crypto ledgerera ~ StandardCrypto
              => TxId -> Shelley.TxId ledgerera
toShelleyTxId (TxId h) =
    Shelley.TxId (Crypto.castHash h)

-- | Calculate the transaction identifier for a 'TxBody'.
--
getTxId :: TxBody era -> TxId
getTxId (ByronTxBody tx) =
    TxId
  . Crypto.UnsafeHash
  . SBS.toShort
  . recoverBytes
  $ tx

getTxId (ShelleyTxBody era tx _) =
    case era of
      ShelleyBasedEraShelley -> getTxIdShelley tx
      ShelleyBasedEraAllegra -> getTxIdShelley tx
      ShelleyBasedEraMary    -> getTxIdShelley tx
  where
    getTxIdShelley :: Ledger.Crypto ledgerera ~ StandardCrypto
                   => Ledger.TxBodyConstraints ledgerera
                   => Ledger.TxBody ledgerera -> TxId
    getTxIdShelley =
        TxId
      . Crypto.castHash
      . (\(Shelley.TxId txhash) -> txhash)
      . Shelley.txid


-- ----------------------------------------------------------------------------
-- Transaction inputs and outputs
--

data TxIn = TxIn TxId TxIx

deriving instance Eq TxIn
deriving instance Show TxIn

newtype TxIx = TxIx Word
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

data TxOut era = TxOut (AddressInEra era) (TxOutValue era)

deriving instance Eq   (TxOut era)
deriving instance Show (TxOut era)

toByronTxIn  :: TxIn -> Byron.TxIn
toByronTxIn (TxIn txid (TxIx txix)) =
    Byron.TxInUtxo (toByronTxId txid) (fromIntegral txix)

toByronTxOut :: TxOut ByronEra -> Maybe Byron.TxOut
toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress addr))
                    (TxOutAdaOnly AdaOnlyInByronEra value)) =
    Byron.TxOut addr <$> toByronLovelace value

toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _))
                    (TxOutValue era _)) = case era of {}

toByronTxOut (TxOut (AddressInEra (ShelleyAddressInEra era) ShelleyAddress{})
                    _) = case era of {}

toByronLovelace :: Lovelace -> Maybe Byron.Lovelace
toByronLovelace (Lovelace x) =
    case Byron.integerToLovelace x of
      Left  _  -> Nothing
      Right x' -> Just x'

toShelleyTxIn  :: (Ledger.Era ledgerera,
                   Ledger.Crypto ledgerera ~ StandardCrypto)
               => TxIn -> Shelley.TxIn ledgerera
toShelleyTxIn (TxIn txid (TxIx txix)) =
    Shelley.TxIn (toShelleyTxId txid) (fromIntegral txix)

toShelleyTxOut :: forall era ledgerera.
                 (ShelleyLedgerEra era ~ ledgerera,
                  IsShelleyBasedEra era, Ledger.ShelleyBased ledgerera)
               => TxOut era -> Shelley.TxOut ledgerera
toShelleyTxOut (TxOut _ (TxOutAdaOnly AdaOnlyInByronEra _)) =
    case shelleyBasedEra :: ShelleyBasedEra era of {}

toShelleyTxOut (TxOut addr (TxOutAdaOnly AdaOnlyInShelleyEra value)) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut (TxOut addr (TxOutAdaOnly AdaOnlyInAllegraEra value)) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut (TxOut _addr (TxOutValue MultiAssetInMaryEra _value)) =
    error "toShelleyTxOut: TODO: TxOutValue MultiAssetInMaryEra"




-- ----------------------------------------------------------------------------
-- Transaction bodies
--

data TxBody era where

     ByronTxBody
       :: Annotated Byron.Tx ByteString
       -> TxBody ByronEra

     ShelleyTxBody
       :: ShelleyBasedEra era
       -> Ledger.TxBody (ShelleyLedgerEra era)
       -> Maybe Shelley.MetaData
       -> TxBody era
     -- The 'ShelleyBasedEra' GADT tells us what era we are in.
     -- The 'ShelleyLedgerEra' type family maps that to the era type from the
     -- ledger lib. The 'Ledger.TxBody' type family maps that to a specific
     -- tx body type, which is different for each Shelley-based era.


-- The GADT in the ShelleyTxBody case requires a custom instance
instance Eq (TxBody era) where
    (==) (ByronTxBody txbodyA)
         (ByronTxBody txbodyB) = txbodyA == txbodyB

    (==) (ShelleyTxBody era txbodyA txmetadataA)
         (ShelleyTxBody _   txbodyB txmetadataB) =
         txmetadataA == txmetadataB
      && case era of
           ShelleyBasedEraShelley -> txbodyA == txbodyB
           ShelleyBasedEraAllegra -> txbodyA == txbodyB
           ShelleyBasedEraMary    -> txbodyA == txbodyB

    (==) (ByronTxBody{}) (ShelleyTxBody era _ _) = case era of {}


-- The GADT in the ShelleyTxBody case requires a custom instance
instance Show (TxBody era) where
    showsPrec p (ByronTxBody txbody) =
      showParen (p >= 11)
        ( showString "ByronTxBody "
        . showsPrec 11 txbody
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraShelley txbody txmetadata) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraShelley "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txmetadata
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraAllegra txbody txmetadata) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraAllegra "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txmetadata
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraMary txbody txmetadata) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraMary "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txmetadata
        )

instance HasTypeProxy era => HasTypeProxy (TxBody era) where
    data AsType (TxBody era) = AsTxBody (AsType era)
    proxyToAsType _ = AsTxBody (proxyToAsType (Proxy :: Proxy era))

pattern AsByronTxBody :: AsType (TxBody ByronEra)
pattern AsByronTxBody   = AsTxBody AsByronEra
{-# COMPLETE AsByronTxBody #-}

pattern AsShelleyTxBody :: AsType (TxBody ShelleyEra)
pattern AsShelleyTxBody = AsTxBody AsShelleyEra
{-# COMPLETE AsShelleyTxBody #-}


instance IsCardanoEra era => SerialiseAsCBOR (TxBody era) where

    serialiseToCBOR (ByronTxBody txbody) =
      recoverBytes txbody

    serialiseToCBOR (ShelleyTxBody ShelleyBasedEraShelley txbody txmetadata) =
      CBOR.serializeEncoding' $
          CBOR.encodeListLen 2
       <> CBOR.toCBOR txbody
       <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata
    serialiseToCBOR (ShelleyTxBody ShelleyBasedEraAllegra _ _) =
      error "TODO: SerialiseAsCBOR (TxBody AllegraEra)"
    serialiseToCBOR (ShelleyTxBody ShelleyBasedEraMary _ _) =
      error "TODO: SerialiseAsCBOR (TxBody MaryEra)"

    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronTxBody <$>
            CBOR.decodeFullAnnotatedBytes
              "Byron TxBody"
              CBOR.fromCBORAnnotated
              (LBS.fromStrict bs)
        ShelleyEra ->
          CBOR.decodeAnnotator
            "Shelley TxBody"
            decodeAnnotatedPair
            (LBS.fromStrict bs)
        AllegraEra -> error "TODO: SerialiseAsCBOR (TxBody AllegraEra)"
        MaryEra    -> error "TODO: SerialiseAsCBOR (TxBody MaryEra)"
      where
        decodeAnnotatedPair :: CBOR.Decoder s (CBOR.Annotator (TxBody ShelleyEra))
        decodeAnnotatedPair =  do
          CBOR.decodeListLenOf 2
          txbody     <- fromCBOR
          txmetadata <- CBOR.decodeNullMaybe fromCBOR
          return $ CBOR.Annotator $ \fbs ->
            ShelleyTxBody
              ShelleyBasedEraShelley
              (CBOR.runAnnotator txbody fbs)
              (CBOR.runAnnotator <$> txmetadata <*> pure fbs)


instance IsCardanoEra era => HasTextEnvelope (TxBody era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> "TxUnsignedByron"
        ShelleyEra -> "TxUnsignedShelley"
        AllegraEra -> "TxBodyAllegra"
        MaryEra    -> "TxBodyMary"


data ByronTxBodyConversionError =
       ByronTxBodyEmptyTxIns
     | ByronTxBodyEmptyTxOuts
     | ByronTxBodyLovelaceOverflow (TxOut ByronEra)
     deriving Show

makeByronTransaction :: [TxIn]
                     -> [TxOut ByronEra]
                     -> Either ByronTxBodyConversionError
                               (TxBody ByronEra)
makeByronTransaction ins outs = do
    ins'  <- NonEmpty.nonEmpty ins        ?! ByronTxBodyEmptyTxIns
    let ins'' = NonEmpty.map toByronTxIn ins'

    outs'  <- NonEmpty.nonEmpty outs      ?! ByronTxBodyEmptyTxOuts
    outs'' <- traverse
                (\out -> toByronTxOut out ?! ByronTxBodyLovelaceOverflow out)
                outs'
    return $
      ByronTxBody $
        reAnnotate $
          Annotated
            (Byron.UnsafeTx ins'' outs'' (Byron.mkAttributes ()))
            ()


data TxExtraContent =
     TxExtraContent {
       txMetadata        :: Maybe TxMetadata,
       txWithdrawals     :: [(StakeAddress, Lovelace)],
       txCertificates    :: [Certificate],
       txUpdateProposal  :: Maybe UpdateProposal
     }

txExtraContentEmpty :: TxExtraContent
txExtraContentEmpty =
    TxExtraContent {
      txMetadata        = Nothing,
      txWithdrawals     = [],
      txCertificates    = [],
      txUpdateProposal  = Nothing
    }

type TxFee = Lovelace
type TTL   = SlotNo

makeShelleyTransaction :: forall era.
                          IsShelleyBasedEra era
                       => TxExtraContent
                       -> TTL
                       -> TxFee
                       -> [TxIn]
                       -> [TxOut era]
                       -> TxBody era
makeShelleyTransaction TxExtraContent {
                         txMetadata,
                         txWithdrawals,
                         txCertificates,
                         txUpdateProposal
                       } ttl fee ins outs =
    --TODO: validate the txins are not empty, and tx out coin values are in range
    case shelleyBasedEra :: ShelleyBasedEra era of
      ShelleyBasedEraShelley ->
        ShelleyTxBody
          ShelleyBasedEraShelley
          (Shelley.TxBody
            (Set.fromList (map toShelleyTxIn ins))
            (Seq.fromList (map toShelleyTxOut outs))
            (Seq.fromList (map toShelleyCertificate txCertificates))
            (toShelleyWithdrawal txWithdrawals)
            (toShelleyLovelace fee)
            ttl
            (toShelleyUpdate <$> maybeToStrictMaybe txUpdateProposal)
            (toShelleyMetadataHash <$> maybeToStrictMaybe txMetadata))
          (toShelleyMetadata <$> txMetadata)
      ShelleyBasedEraAllegra -> error "TODO: makeShelleyTransaction AllegraEra"
      ShelleyBasedEraMary    -> error "TODO: makeShelleyTransaction MaryEra"


toShelleyWithdrawal :: [(StakeAddress, Lovelace)] -> Shelley.Wdrl ledgerera
toShelleyWithdrawal withdrawals =
    Shelley.Wdrl $
      Map.fromList
        [ (toShelleyStakeAddr stakeAddr, toShelleyLovelace value)
        | (stakeAddr, value) <- withdrawals ]


-- | Compute the 'TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Shelley initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'TxIn' to use as an input to the spending transaction.
--
genesisUTxOPseudoTxIn :: NetworkId -> Hash GenesisUTxOKey -> TxIn
genesisUTxOPseudoTxIn nw (GenesisUTxOKeyHash kh) =
    --TODO: should handle Byron UTxO case too.
    fromShelleyTxIn (Shelley.initialFundsPseudoTxIn addr)
  where
    addr = Shelley.Addr
             (toShelleyNetwork nw)
             (Shelley.KeyHashObj kh)
             Shelley.StakeRefNull

    fromShelleyTxIn  :: Shelley.TxIn StandardShelley -> TxIn
    fromShelleyTxIn (Shelley.TxIn txid txix) =
        TxIn (fromShelleyTxId txid) (TxIx (fromIntegral txix))

    fromShelleyTxId :: Shelley.TxId StandardShelley -> TxId
    fromShelleyTxId (Shelley.TxId h) =
        TxId (Crypto.castHash h)

