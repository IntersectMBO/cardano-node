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

    -- * Transaction bodies
    TxBody(..),
    makeTransactionBody,
    TxBodyContent(..),
    TxBodyError(..),

    -- ** Transitional utils
    makeByronTransaction,
    makeShelleyTransaction,

    -- * Transaction Ids
    TxId(..),
    getTxId,

    -- * Transaction inputs
    TxIn(..),
    TxIx(..),
    genesisUTxOPseudoTxIn,

    -- * Transaction outputs
    TxOut(..),
    TxOutValue(..),

    -- * Other transaction body types
    TxFee(..),
    TxValidityLowerBound(..),
    TxValidityUpperBound(..),
    TxMetadataInEra(..),
    TxAuxScripts(..),
    TxWithdrawals(..),
    TxCertificates(..),
    TxUpdateProposal(..),
    TxMintValue(..),

    -- * Era-dependent transaction body features
    MultiAssetSupportedInEra(..),
    OnlyAdaSupportedInEra(..),
    TxFeesExplicitInEra(..),
    TxFeesImplicitInEra(..),
    ValidityUpperBoundSupportedInEra(..),
    ValidityNoUpperBoundSupportedInEra(..),
    ValidityLowerBoundSupportedInEra(..),
    TxMetadataSupportedInEra(..),
    AuxScriptsSupportedInEra(..),
    WithdrawalsSupportedInEra(..),
    CertificatesSupportedInEra(..),
    UpdateProposalSupportedInEra(..),

    -- ** Feature availability functions
    multiAssetSupportedInEra,
    txFeesExplicitInEra,
    validityUpperBoundSupportedInEra,
    validityNoUpperBoundSupportedInEra,
    validityLowerBoundSupportedInEra,
    txMetadataSupportedInEra,
    auxScriptsSupportedInEra,
    withdrawalsSupportedInEra,
    certificatesSupportedInEra,
    updateProposalSupportedInEra,

    -- * Data family instances
    AsType(AsTxId, AsTxBody, AsByronTxBody, AsShelleyTxBody),
  ) where

import           Prelude

import           Data.Bifunctor (first)
import           Data.Maybe (fromMaybe)
import           Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.String (IsString)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Control.Monad (guard)

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
import qualified Cardano.Ledger.Shelley.Constraints as Ledger
import qualified Cardano.Ledger.ShelleyMA.TxBody as Allegra
import qualified Cardano.Ledger.ShelleyMA.Metadata as Allegra
import qualified Shelley.Spec.Ledger.Metadata as Ledger (MetadataHash, hashMetadata)
import           Ouroboros.Consensus.Shelley.Eras
                   (StandardShelley, StandardAllegra, StandardMary)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe(..), maybeToStrictMaybe)
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Metadata as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysShelley
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Script
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
  . fromMaybe impossible
  . Crypto.hashFromBytesShort
  . Byron.abstractHashToShort
  . Byron.hashDecoded
  $ tx
  where
    impossible =
      error "getTxId: byron and shelley hash sizes do not match"

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
-- Transaction inputs
--

data TxIn = TxIn TxId TxIx

deriving instance Eq TxIn
deriving instance Show TxIn

newtype TxIx = TxIx Word
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)


toByronTxIn  :: TxIn -> Byron.TxIn
toByronTxIn (TxIn txid (TxIx txix)) =
    Byron.TxInUtxo (toByronTxId txid) (fromIntegral txix)

toShelleyTxIn  :: (Ledger.Era ledgerera,
                   Ledger.Crypto ledgerera ~ StandardCrypto)
               => TxIn -> Shelley.TxIn ledgerera
toShelleyTxIn (TxIn txid (TxIx txix)) =
    Shelley.TxIn (toShelleyTxId txid) (fromIntegral txix)


-- ----------------------------------------------------------------------------
-- Transaction outputs
--

data TxOut era = TxOut (AddressInEra era) (TxOutValue era)

deriving instance Eq   (TxOut era)
deriving instance Show (TxOut era)


toByronTxOut :: TxOut ByronEra -> Maybe Byron.TxOut
toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress addr))
                    (TxOutAdaOnly AdaOnlyInByronEra value)) =
    Byron.TxOut addr <$> toByronLovelace value

toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _))
                    (TxOutValue era _)) = case era of {}

toByronTxOut (TxOut (AddressInEra (ShelleyAddressInEra era) ShelleyAddress{})
                    _) = case era of {}


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

toShelleyTxOut (TxOut addr (TxOutValue MultiAssetInMaryEra value)) =
    Shelley.TxOut (toShelleyAddr addr) (toMaryValue value)


-- ----------------------------------------------------------------------------
-- Era-dependent transaction body features
--

-- | A representation of whether the era supports multi-asset transactions.
--
-- The Mary and subsequent eras support multi-asset transactions.
--
-- The negation of this is 'OnlyAdaSupportedInEra'.
--
data MultiAssetSupportedInEra era where

     -- | Multi-asset transactions are supported in the 'Mary' era.
     MultiAssetInMaryEra :: MultiAssetSupportedInEra MaryEra

deriving instance Eq   (MultiAssetSupportedInEra era)
deriving instance Show (MultiAssetSupportedInEra era)

-- | A representation of whether the era supports only ada transactions.
--
-- Prior to the Mary era only ada transactions are supported. Multi-assets are
-- supported from the Mary era onwards.
--
-- This is the negation of 'MultiAssetSupportedInEra'. It exists since we need
-- evidence to be positive.
--
data OnlyAdaSupportedInEra era where

     AdaOnlyInByronEra   :: OnlyAdaSupportedInEra ByronEra
     AdaOnlyInShelleyEra :: OnlyAdaSupportedInEra ShelleyEra
     AdaOnlyInAllegraEra :: OnlyAdaSupportedInEra AllegraEra

deriving instance Eq   (OnlyAdaSupportedInEra era)
deriving instance Show (OnlyAdaSupportedInEra era)

multiAssetSupportedInEra :: CardanoEra era
                         -> Either (OnlyAdaSupportedInEra era)
                                   (MultiAssetSupportedInEra era)
multiAssetSupportedInEra ByronEra   = Left AdaOnlyInByronEra
multiAssetSupportedInEra ShelleyEra = Left AdaOnlyInShelleyEra
multiAssetSupportedInEra AllegraEra = Left AdaOnlyInAllegraEra
multiAssetSupportedInEra MaryEra    = Right MultiAssetInMaryEra


-- | A representation of whether the era requires explicitly specified fees in
-- transactions.
--
-- The Byron era tx fees are implicit (as the difference bettween the sum of
-- outputs and sum of inputs), but all later eras the fees are specified in the
-- transaction explicitly.
--
data TxFeesExplicitInEra era where

     TxFeesExplicitInShelleyEra :: TxFeesExplicitInEra ShelleyEra
     TxFeesExplicitInAllegraEra :: TxFeesExplicitInEra AllegraEra
     TxFeesExplicitInMaryEra    :: TxFeesExplicitInEra MaryEra

deriving instance Eq   (TxFeesExplicitInEra era)
deriving instance Show (TxFeesExplicitInEra era)

-- | A representation of whether the era requires implicitly specified fees in
-- transactions.
--
-- This is the negation of 'TxFeesExplicitInEra'.
--
data TxFeesImplicitInEra era where
     TxFeesImplicitInByronEra :: TxFeesImplicitInEra ByronEra

deriving instance Eq   (TxFeesImplicitInEra era)
deriving instance Show (TxFeesImplicitInEra era)

txFeesExplicitInEra :: CardanoEra era
                    -> Either (TxFeesImplicitInEra era)
                              (TxFeesExplicitInEra era)
txFeesExplicitInEra ByronEra   = Left  TxFeesImplicitInByronEra
txFeesExplicitInEra ShelleyEra = Right TxFeesExplicitInShelleyEra
txFeesExplicitInEra AllegraEra = Right TxFeesExplicitInAllegraEra
txFeesExplicitInEra MaryEra    = Right TxFeesExplicitInMaryEra


-- | A representation of whether the era supports transactions with an upper
-- bound on the range of slots in which they are valid.
--
-- The Shelley and subsequent eras support an upper bound on the validity
-- range. In the Shelley era specifically it is actually required. It is
-- optional in later eras.
--
data ValidityUpperBoundSupportedInEra era where

     ValidityUpperBoundInShelleyEra :: ValidityUpperBoundSupportedInEra ShelleyEra
     ValidityUpperBoundInAllegraEra :: ValidityUpperBoundSupportedInEra AllegraEra
     ValidityUpperBoundInMaryEra    :: ValidityUpperBoundSupportedInEra MaryEra

deriving instance Eq   (ValidityUpperBoundSupportedInEra era)
deriving instance Show (ValidityUpperBoundSupportedInEra era)

validityUpperBoundSupportedInEra :: CardanoEra era
                                 -> Maybe (ValidityUpperBoundSupportedInEra era)
validityUpperBoundSupportedInEra ByronEra   = Nothing
validityUpperBoundSupportedInEra ShelleyEra = Just ValidityUpperBoundInShelleyEra
validityUpperBoundSupportedInEra AllegraEra = Just ValidityUpperBoundInAllegraEra
validityUpperBoundSupportedInEra MaryEra    = Just ValidityUpperBoundInMaryEra


-- | A representation of whether the era supports transactions having /no/
-- upper bound on the range of slots in which they are valid.
--
-- Note that the 'ShelleyEra' /does not support/ omitting a validity upper
-- bound. It was introduced as a /required/ field in Shelley and then made
-- optional in Allegra and subsequent eras.
--
-- The Byron era supports this by virtue of the fact that it does not support
-- validity ranges at all.
--
data ValidityNoUpperBoundSupportedInEra era where

     ValidityNoUpperBoundInByronEra   :: ValidityNoUpperBoundSupportedInEra ByronEra
     ValidityNoUpperBoundInAllegraEra :: ValidityNoUpperBoundSupportedInEra AllegraEra
     ValidityNoUpperBoundInMaryEra    :: ValidityNoUpperBoundSupportedInEra MaryEra

deriving instance Eq   (ValidityNoUpperBoundSupportedInEra era)
deriving instance Show (ValidityNoUpperBoundSupportedInEra era)

validityNoUpperBoundSupportedInEra :: CardanoEra era
                                   -> Maybe (ValidityNoUpperBoundSupportedInEra era)
validityNoUpperBoundSupportedInEra ByronEra   = Just ValidityNoUpperBoundInByronEra
validityNoUpperBoundSupportedInEra ShelleyEra = Nothing
validityNoUpperBoundSupportedInEra AllegraEra = Just ValidityNoUpperBoundInAllegraEra
validityNoUpperBoundSupportedInEra MaryEra    = Just ValidityNoUpperBoundInMaryEra


-- | A representation of whether the era supports transactions with a lower
-- bound on the range of slots in which they are valid.
--
-- The Allegra and subsequent eras support an optional lower bound on the
-- validity range. No equivalent of 'ValidityNoUpperBoundSupportedInEra' is
-- needed since all eras support having no lower bound.
--
data ValidityLowerBoundSupportedInEra era where

     ValidityLowerBoundInAllegraEra :: ValidityLowerBoundSupportedInEra AllegraEra
     ValidityLowerBoundInMaryEra    :: ValidityLowerBoundSupportedInEra MaryEra

deriving instance Eq   (ValidityLowerBoundSupportedInEra era)
deriving instance Show (ValidityLowerBoundSupportedInEra era)

validityLowerBoundSupportedInEra :: CardanoEra era
                                 -> Maybe (ValidityLowerBoundSupportedInEra era)
validityLowerBoundSupportedInEra ByronEra   = Nothing
validityLowerBoundSupportedInEra ShelleyEra = Nothing
validityLowerBoundSupportedInEra AllegraEra = Just ValidityLowerBoundInAllegraEra
validityLowerBoundSupportedInEra MaryEra    = Just ValidityLowerBoundInMaryEra


-- | A representation of whether the era supports transaction metadata.
--
-- Transaction metadata is supported from the Shelley era onwards.
--
data TxMetadataSupportedInEra era where

     TxMetadataInShelleyEra :: TxMetadataSupportedInEra ShelleyEra
     TxMetadataInAllegraEra :: TxMetadataSupportedInEra AllegraEra
     TxMetadataInMaryEra    :: TxMetadataSupportedInEra MaryEra

deriving instance Eq   (TxMetadataSupportedInEra era)
deriving instance Show (TxMetadataSupportedInEra era)

txMetadataSupportedInEra :: CardanoEra era
                         -> Maybe (TxMetadataSupportedInEra era)
txMetadataSupportedInEra ByronEra   = Nothing
txMetadataSupportedInEra ShelleyEra = Just TxMetadataInShelleyEra
txMetadataSupportedInEra AllegraEra = Just TxMetadataInAllegraEra
txMetadataSupportedInEra MaryEra    = Just TxMetadataInMaryEra


-- | A representation of whether the era supports auxiliary scripts in
-- transactions.
--
-- Auxiliary scripts are supported from the Allegra era onwards.
--
data AuxScriptsSupportedInEra era where

     AuxScriptsInAllegraEra :: AuxScriptsSupportedInEra AllegraEra
     AuxScriptsInMaryEra    :: AuxScriptsSupportedInEra MaryEra

deriving instance Eq   (AuxScriptsSupportedInEra era)
deriving instance Show (AuxScriptsSupportedInEra era)

auxScriptsSupportedInEra :: CardanoEra era
                         -> Maybe (AuxScriptsSupportedInEra era)
auxScriptsSupportedInEra ByronEra   = Nothing
auxScriptsSupportedInEra ShelleyEra = Nothing
auxScriptsSupportedInEra AllegraEra = Just AuxScriptsInAllegraEra
auxScriptsSupportedInEra MaryEra    = Just AuxScriptsInMaryEra


-- | A representation of whether the era supports withdrawals from reward
-- accounts.
--
-- The Shelley and subsequent eras support stake addresses, their associated
-- reward accounts and support for withdrawals from them.
--
data WithdrawalsSupportedInEra era where

     WithdrawalsInShelleyEra :: WithdrawalsSupportedInEra ShelleyEra
     WithdrawalsInAllegraEra :: WithdrawalsSupportedInEra AllegraEra
     WithdrawalsInMaryEra    :: WithdrawalsSupportedInEra MaryEra

deriving instance Eq   (WithdrawalsSupportedInEra era)
deriving instance Show (WithdrawalsSupportedInEra era)

withdrawalsSupportedInEra :: CardanoEra era
                          -> Maybe (WithdrawalsSupportedInEra era)
withdrawalsSupportedInEra ByronEra   = Nothing
withdrawalsSupportedInEra ShelleyEra = Just WithdrawalsInShelleyEra
withdrawalsSupportedInEra AllegraEra = Just WithdrawalsInAllegraEra
withdrawalsSupportedInEra MaryEra    = Just WithdrawalsInMaryEra


-- | A representation of whether the era supports 'Certificate's embedded in
-- transactions.
--
-- The Shelley and subsequent eras support such certificates.
--
data CertificatesSupportedInEra era where

     CertificatesInShelleyEra :: CertificatesSupportedInEra ShelleyEra
     CertificatesInAllegraEra :: CertificatesSupportedInEra AllegraEra
     CertificatesInMaryEra    :: CertificatesSupportedInEra MaryEra

deriving instance Eq   (CertificatesSupportedInEra era)
deriving instance Show (CertificatesSupportedInEra era)

certificatesSupportedInEra :: CardanoEra era
                           -> Maybe (CertificatesSupportedInEra era)
certificatesSupportedInEra ByronEra   = Nothing
certificatesSupportedInEra ShelleyEra = Just CertificatesInShelleyEra
certificatesSupportedInEra AllegraEra = Just CertificatesInAllegraEra
certificatesSupportedInEra MaryEra    = Just CertificatesInMaryEra


-- | A representation of whether the era supports 'UpdateProposal's embedded in
-- transactions.
--
-- The Shelley and subsequent eras support such update proposals. They Byron
-- era has a notion of an update proposal, but it is a standalone chain object
-- and not embedded in a transaction.
--
data UpdateProposalSupportedInEra era where

     UpdateProposalInShelleyEra :: UpdateProposalSupportedInEra ShelleyEra
     UpdateProposalInAllegraEra :: UpdateProposalSupportedInEra AllegraEra
     UpdateProposalInMaryEra    :: UpdateProposalSupportedInEra MaryEra

deriving instance Eq   (UpdateProposalSupportedInEra era)
deriving instance Show (UpdateProposalSupportedInEra era)

updateProposalSupportedInEra :: CardanoEra era
                             -> Maybe (UpdateProposalSupportedInEra era)
updateProposalSupportedInEra ByronEra   = Nothing
updateProposalSupportedInEra ShelleyEra = Just UpdateProposalInShelleyEra
updateProposalSupportedInEra AllegraEra = Just UpdateProposalInAllegraEra
updateProposalSupportedInEra MaryEra    = Just UpdateProposalInMaryEra


-- ----------------------------------------------------------------------------
-- Transaction output values (era-dependent)
--

data TxOutValue era where

     TxOutAdaOnly :: OnlyAdaSupportedInEra era -> Lovelace -> TxOutValue era

     TxOutValue   :: MultiAssetSupportedInEra era -> Value -> TxOutValue era

deriving instance Eq   (TxOutValue era)
deriving instance Show (TxOutValue era)


-- ----------------------------------------------------------------------------
-- Transaction fees
--

data TxFee era where

     TxFeeImplicit :: TxFeesImplicitInEra era -> TxFee era

     TxFeeExplicit :: TxFeesExplicitInEra era -> Lovelace -> TxFee era

deriving instance Eq   (TxFee era)
deriving instance Show (TxFee era)


-- ----------------------------------------------------------------------------
-- Transaction validity range
--

-- | This was formerly known as the TTL.
--
data TxValidityUpperBound era where

     TxValidityNoUpperBound :: ValidityNoUpperBoundSupportedInEra era
                            -> TxValidityUpperBound era

     TxValidityUpperBound   :: ValidityUpperBoundSupportedInEra era
                            -> SlotNo
                            -> TxValidityUpperBound era

deriving instance Eq   (TxValidityUpperBound era)
deriving instance Show (TxValidityUpperBound era)


data TxValidityLowerBound era where

     TxValidityNoLowerBound :: TxValidityLowerBound era

     TxValidityLowerBound   :: ValidityLowerBoundSupportedInEra era
                            -> SlotNo
                            -> TxValidityLowerBound era

deriving instance Eq   (TxValidityLowerBound era)
deriving instance Show (TxValidityLowerBound era)


-- ----------------------------------------------------------------------------
-- Transaction metadata (era-dependent)
--

data TxMetadataInEra era where

     TxMetadataNone  :: TxMetadataInEra era

     TxMetadataInEra :: TxMetadataSupportedInEra era
                     -> TxMetadata
                     -> TxMetadataInEra era

deriving instance Eq   (TxMetadataInEra era)
deriving instance Show (TxMetadataInEra era)


-- ----------------------------------------------------------------------------
-- Auxiliary scripts (era-dependent)
--

data TxAuxScripts era where

     TxAuxScriptsNone :: TxAuxScripts era

     TxAuxScripts     :: AuxScriptsSupportedInEra era
                      -> [ScriptInEra era]
                      -> TxAuxScripts era

deriving instance Eq   (TxAuxScripts era)
deriving instance Show (TxAuxScripts era)


-- ----------------------------------------------------------------------------
-- Withdrawals within transactions (era-dependent)
--

data TxWithdrawals era where

     TxWithdrawalsNone :: TxWithdrawals era

     TxWithdrawals     :: WithdrawalsSupportedInEra era
                       -> [(StakeAddress, Lovelace)]
                       -> TxWithdrawals era

deriving instance Eq   (TxWithdrawals era)
deriving instance Show (TxWithdrawals era)


-- ----------------------------------------------------------------------------
-- Certificates within transactions (era-dependent)
--

data TxCertificates era where

     TxCertificatesNone :: TxCertificates era

     TxCertificates     :: CertificatesSupportedInEra era
                        -> [Certificate]
                        -> TxCertificates era

deriving instance Eq   (TxCertificates era)
deriving instance Show (TxCertificates era)


-- ----------------------------------------------------------------------------
-- Transaction metadata (era-dependent)
--

data TxUpdateProposal era where

     TxUpdateProposalNone :: TxUpdateProposal era

     TxUpdateProposal     :: UpdateProposalSupportedInEra era
                          -> UpdateProposal
                          -> TxUpdateProposal era

deriving instance Eq   (TxUpdateProposal era)
deriving instance Show (TxUpdateProposal era)


-- ----------------------------------------------------------------------------
-- Value minting within transactions (era-dependent)
--

data TxMintValue era where

     TxMintNone  :: TxMintValue era

     TxMintValue :: MultiAssetSupportedInEra era -> Value -> TxMintValue era

deriving instance Eq   (TxMintValue era)
deriving instance Show (TxMintValue era)


-- ----------------------------------------------------------------------------
-- Transaction body content
--

data TxBodyContent era =
     TxBodyContent {
       txIns            :: [TxIn],
       txOuts           :: [TxOut era],
       txFee            :: TxFee era,
       txValidityRange  :: (TxValidityLowerBound era,
                            TxValidityUpperBound era),
       txMetadata       :: TxMetadataInEra era,
       txAuxScripts     :: TxAuxScripts era,
       txWithdrawals    :: TxWithdrawals era,
       txCertificates   :: TxCertificates era,
       txUpdateProposal :: TxUpdateProposal era,
       txMintValue      :: TxMintValue era
     }


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

          -- The 'Ledger.Metadata' is really the /auxiliary/ data, it consists
          -- of one or several things, depending on era:
          -- + transaction metadata  (in Shelley and later)
          -- + auxiliary scripts     (in Allegra and later)
          -- + auxiliary script data (in Allonzo and later)
       -> Maybe (Ledger.Metadata (ShelleyLedgerEra era))

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
         case era of
           ShelleyBasedEraShelley -> txmetadataA == txmetadataB
           ShelleyBasedEraAllegra -> txmetadataA == txmetadataB
           ShelleyBasedEraMary    -> txmetadataA == txmetadataB
      && case era of
           ShelleyBasedEraShelley -> txbodyA == txbodyB
           ShelleyBasedEraAllegra -> txbodyA == txbodyB
           ShelleyBasedEraMary    -> txbodyA == txbodyB

    (==) ByronTxBody{} (ShelleyTxBody era _ _) = case era of {}


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

    serialiseToCBOR (ShelleyTxBody era txbody txmetadata) =
      case era of
        -- Use the same serialisation impl, but at different types:
        ShelleyBasedEraShelley -> serialiseShelleyBasedTxBody txbody txmetadata
        ShelleyBasedEraAllegra -> serialiseShelleyBasedTxBody txbody txmetadata
        ShelleyBasedEraMary    -> serialiseShelleyBasedTxBody txbody txmetadata

    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronTxBody <$>
            CBOR.decodeFullAnnotatedBytes
              "Byron TxBody"
              CBOR.fromCBORAnnotated
              (LBS.fromStrict bs)

        -- Use the same derialisation impl, but at different types:
        ShelleyEra -> deserialiseShelleyBasedTxBody
                        (ShelleyTxBody ShelleyBasedEraShelley) bs
        AllegraEra -> deserialiseShelleyBasedTxBody
                        (ShelleyTxBody ShelleyBasedEraAllegra) bs
        MaryEra    -> deserialiseShelleyBasedTxBody
                        (ShelleyTxBody ShelleyBasedEraMary) bs

-- | The serialisation format for the different Shelley-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
--
serialiseShelleyBasedTxBody :: forall txbody metadata.
                                (ToCBOR txbody, ToCBOR metadata)
                            => txbody -> Maybe metadata -> ByteString
serialiseShelleyBasedTxBody txbody txmetadata =
    CBOR.serializeEncoding' $
        CBOR.encodeListLen 2
     <> CBOR.toCBOR txbody
     <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata

deserialiseShelleyBasedTxBody :: forall txbody metadata pair.
                                (FromCBOR (CBOR.Annotator txbody),
                                 FromCBOR (CBOR.Annotator metadata))
                              => (txbody -> Maybe metadata -> pair)
                              -> ByteString
                              -> Either CBOR.DecoderError pair
deserialiseShelleyBasedTxBody mkTxBody bs =
    CBOR.decodeAnnotator
      "Shelley TxBody"
      decodeAnnotatedPair
      (LBS.fromStrict bs)
  where
    decodeAnnotatedPair :: CBOR.Decoder s (CBOR.Annotator pair)
    decodeAnnotatedPair =  do
      CBOR.decodeListLenOf 2
      txbody     <- fromCBOR
      txmetadata <- CBOR.decodeNullMaybe fromCBOR
      return $ CBOR.Annotator $ \fbs ->
        mkTxBody
          (CBOR.runAnnotator txbody fbs)
          (CBOR.runAnnotator <$> txmetadata <*> pure fbs)

instance IsCardanoEra era => HasTextEnvelope (TxBody era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> "TxUnsignedByron"
        ShelleyEra -> "TxUnsignedShelley"
        AllegraEra -> "TxBodyAllegra"
        MaryEra    -> "TxBodyMary"


-- ----------------------------------------------------------------------------
-- Constructing transaction bodies
--

data TxBodyError era =
       TxBodyEmptyTxIns
     | TxBodyEmptyTxOuts
     | TxBodyLovelaceOverflow (TxOut era)
     | TxBodyMetadataError [(Word64, TxMetadataRangeError)]
     | TxBodyMintAdaError
     deriving Show

instance Error (TxBodyError era) where
    displayError TxBodyEmptyTxIns  = "Transaction body has no inputs"
    displayError TxBodyEmptyTxOuts = "Transaction body has no outputs"
    displayError (TxBodyLovelaceOverflow txout) =
      "Lovelace greater than 2^64 in transaction output " <> show txout
    displayError (TxBodyMetadataError [(k, err)]) =
      "Error in metadata entry " ++ show k ++ ": " ++ displayError err
    displayError (TxBodyMetadataError errs) =
      "Error in metadata entries: " ++
      intercalate "; "
        [ show k ++ ": " ++ displayError err
        | (k, err) <- errs ]
    displayError TxBodyMintAdaError =
      "Transaction cannot mint ada, only non-ada assets"


makeTransactionBody :: forall era.
                       IsCardanoEra era
                    => TxBodyContent era
                    -> Either (TxBodyError era) (TxBody era)
makeTransactionBody =
    case cardanoEraStyle (cardanoEra :: CardanoEra era) of
      LegacyByronEra      -> makeByronTransactionBody
      ShelleyBasedEra era -> makeShelleyTransactionBody era


makeByronTransactionBody :: TxBodyContent ByronEra
                         -> Either (TxBodyError ByronEra) (TxBody ByronEra)
makeByronTransactionBody TxBodyContent { txIns, txOuts } = do
    ins'  <- NonEmpty.nonEmpty txIns      ?! TxBodyEmptyTxIns
    let ins'' = NonEmpty.map toByronTxIn ins'

    outs'  <- NonEmpty.nonEmpty txOuts    ?! TxBodyEmptyTxOuts
    outs'' <- traverse
                (\out -> toByronTxOut out ?! TxBodyLovelaceOverflow out)
                outs'
    return $
      ByronTxBody $
        reAnnotate $
          Annotated
            (Byron.UnsafeTx ins'' outs'' (Byron.mkAttributes ()))
            ()

makeShelleyTransactionBody :: ShelleyBasedEra era
                           -> TxBodyContent era
                           -> Either (TxBodyError era) (TxBody era)
makeShelleyTransactionBody era@ShelleyBasedEraShelley
                           TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (_, upperBound),
                             txMetadata,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ guard (v >= 0) ?! TxBodyLovelaceOverflow txout
      | txout@(TxOut _ (TxOutAdaOnly AdaOnlyInShelleyEra v)) <- txOuts ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> first TxBodyMetadataError (validateTxMetadata m)

    return $
      ShelleyTxBody era
        (Shelley.TxBody
          (Set.fromList (map toShelleyTxIn  txIns))
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone  -> Seq.empty
             TxCertificates _ cs -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (case upperBound of
             TxValidityNoUpperBound era' -> case era' of {}
             TxValidityUpperBound _ ttl  -> ttl)
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toShelleyUpdate p))
          (maybeToStrictMaybe (hashAuxiliaryData <$> txAuxData)))
        txAuxData
  where
    txAuxData :: Maybe (Ledger.Metadata StandardShelley)
    txAuxData
      | Map.null ms = Nothing
      | otherwise   = Just (toShelleyAuxiliaryData ms)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'

makeShelleyTransactionBody era@ShelleyBasedEraAllegra
                           TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ guard (v >= 0) ?! TxBodyLovelaceOverflow txout
      | txout@(TxOut _ (TxOutAdaOnly AdaOnlyInAllegraEra v)) <- txOuts ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError

    return $
      ShelleyTxBody era
        (Allegra.TxBody
          (Set.fromList (map toShelleyTxIn  txIns))
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone  -> Seq.empty
             TxCertificates _ cs -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             Allegra.validFrom = case lowerBound of
                                   TxValidityNoLowerBound   -> SNothing
                                   TxValidityLowerBound _ s -> SJust s,
             Allegra.validTo   = case upperBound of
                                   TxValidityNoUpperBound _ -> SNothing
                                   TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toShelleyUpdate p))
          (maybeToStrictMaybe (hashAuxiliaryData <$> txAuxData))
          mempty) -- No minting in Allegra, only Mary
        txAuxData
  where
    txAuxData :: Maybe (Ledger.Metadata StandardAllegra)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toAllegraAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'

makeShelleyTransactionBody era@ShelleyBasedEraMary
                           TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ guard allPositive ?! TxBodyLovelaceOverflow txout
      | txout@(TxOut _ (TxOutValue MultiAssetInMaryEra v)) <- txOuts
      , let allPositive = and [ q >= 0 | (_,q) <- valueToList v ]
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError
    case txMintValue of
      TxMintNone      -> return ()
      TxMintValue _ v -> guard (selectLovelace v == 0) ?! TxBodyMintAdaError

    return $
      ShelleyTxBody era
        (Allegra.TxBody
          (Set.fromList (map toShelleyTxIn  txIns))
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone  -> Seq.empty
             TxCertificates _ cs -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             Allegra.validFrom = case lowerBound of
                                   TxValidityNoLowerBound   -> SNothing
                                   TxValidityLowerBound _ s -> SJust s,
             Allegra.validTo   = case upperBound of
                                   TxValidityNoUpperBound _ -> SNothing
                                   TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toShelleyUpdate p))
          (maybeToStrictMaybe (hashAuxiliaryData <$> txAuxData))
          (case txMintValue of
             TxMintNone      -> mempty
             TxMintValue _ v -> toMaryValue v))
        txAuxData
  where
    txAuxData :: Maybe (Ledger.Metadata StandardMary)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toAllegraAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'


toShelleyWithdrawal :: [(StakeAddress, Lovelace)] -> Shelley.Wdrl ledgerera
toShelleyWithdrawal withdrawals =
    Shelley.Wdrl $
      Map.fromList
        [ (toShelleyStakeAddr stakeAddr, toShelleyLovelace value)
        | (stakeAddr, value) <- withdrawals ]

-- | In the Shelley era the auxiliary data consists only of the tx metadata
toShelleyAuxiliaryData :: Map Word64 TxMetadataValue
                       -> Ledger.Metadata StandardShelley
toShelleyAuxiliaryData m =
    Shelley.Metadata
      (toShelleyMetadata m)

-- | In the Allegra and Mary eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts.
--
toAllegraAuxiliaryData :: forall era ledgeera.
                          ShelleyLedgerEra era ~ ledgeera
                       => Ledger.Metadata ledgeera ~ Allegra.Metadata ledgeera
                       => Ledger.AnnotatedData (Ledger.Script ledgeera)
                       => Ord (Ledger.Script ledgeera)
                       => Map Word64 TxMetadataValue
                       -> [ScriptInEra era]
                       -> Ledger.Metadata ledgeera
toAllegraAuxiliaryData m ss =
    Allegra.Metadata
      (toShelleyMetadata m)
      (Seq.fromList (map toShelleyScript ss))

hashAuxiliaryData :: Shelley.ValidateMetadata ledgeera
                  => Ledger.Metadata ledgeera -> Ledger.MetadataHash ledgeera
hashAuxiliaryData = Ledger.hashMetadata


-- ----------------------------------------------------------------------------
-- Transitional utility functions for making transaction bodies
--

-- | Transitional function to help the CLI move to the updated TxBody API.
--
makeByronTransaction :: [TxIn]
                     -> [TxOut ByronEra]
                     -> Either (TxBodyError ByronEra) (TxBody ByronEra)
makeByronTransaction txIns txOuts =
    makeTransactionBody $
      TxBodyContent {
        txIns,
        txOuts,
        txFee            = TxFeeImplicit TxFeesImplicitInByronEra,
        txValidityRange  = (TxValidityNoLowerBound,
                            TxValidityNoUpperBound
                              ValidityNoUpperBoundInByronEra),
        txMetadata       = TxMetadataNone,
        txAuxScripts     = TxAuxScriptsNone,
        txWithdrawals    = TxWithdrawalsNone,
        txCertificates   = TxCertificatesNone,
        txUpdateProposal = TxUpdateProposalNone,
        txMintValue      = TxMintNone
      }

-- | Transitional function to help the CLI move to the updated TxBody API.
--
makeShelleyTransaction :: [TxIn]
                       -> [TxOut ShelleyEra]
                       -> SlotNo
                       -> Lovelace
                       -> [Certificate]
                       -> [(StakeAddress, Lovelace)]
                       -> Maybe TxMetadata
                       -> Maybe UpdateProposal
                       -> Either (TxBodyError ShelleyEra) (TxBody ShelleyEra)
makeShelleyTransaction txIns txOuts ttl fee
                       certs withdrawals mMetadata mUpdateProp =
    makeTransactionBody $
      TxBodyContent {
        txIns,
        txOuts,
        txFee            = TxFeeExplicit TxFeesExplicitInShelleyEra fee,
        txValidityRange  = (TxValidityNoLowerBound,
                            TxValidityUpperBound
                              ValidityUpperBoundInShelleyEra ttl),
        txMetadata       = case mMetadata of
                             Nothing -> TxMetadataNone
                             Just md -> TxMetadataInEra
                                          TxMetadataInShelleyEra md,
        txAuxScripts     = TxAuxScriptsNone,
        txWithdrawals    = TxWithdrawals WithdrawalsInShelleyEra withdrawals,
        txCertificates   = TxCertificates CertificatesInShelleyEra certs,
        txUpdateProposal = case mUpdateProp of
                             Nothing -> TxUpdateProposalNone
                             Just up -> TxUpdateProposal
                                          UpdateProposalInShelleyEra up,
        txMintValue      = TxMintNone
      }


-- ----------------------------------------------------------------------------
-- Other utilities helpful with making transaction bodies
--

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
