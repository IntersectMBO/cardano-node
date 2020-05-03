{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Cardano.Api
  ( module X

    -- * Keys
  , SigningKey (..)
  , VerificationKey (..)
  , getVerificationKey
  , byronGenSigningKey
  , shelleyGenSigningKey

    -- * Addresses
  , Address (..)
  , Network (..)
  , byronVerificationKeyAddress
  , shelleyVerificationKeyAddress
  , addressToHex
  , addressFromHex

    -- * Transactions
  , TxSigned (..)
  , TxUnsigned (..)
  , TxWitness (..)
  , TxIn (..)
  , TxOut (..)
  , TxId
  , TxIx
  , Lovelace
  , SlotNo

  , getTxSignedBody
  , getTxSignedHash
  , getTxSignedWitnesses
  , getTxUnsignedBody
  , getTxUnsignedHash

  , buildByronTransaction
  , buildShelleyTransaction
  , signTransaction
  , witnessTransaction
  , signTransactionWithWitness
  , submitTransaction
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Sequence.Strict as Seq
import qualified Data.Text.Encoding as Text

import qualified Cardano.Binary as Binary

import           Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))

import qualified Cardano.Crypto.Hashing as Crypto
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import           Cardano.Crypto.Random (runSecureRandom)
import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.Api.Types
import           Cardano.Api.CBOR as X
import           Cardano.Api.Error as X
import           Cardano.Api.View as X
import           Cardano.Api.TxSubmit

import qualified Cardano.Chain.Common  as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO    as Byron

import qualified Shelley.Spec.Ledger.Address   as Shelley
import qualified Shelley.Spec.Ledger.Keys      as Shelley
import qualified Shelley.Spec.Ledger.TxData    as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley


addressFromHex :: Text -> Maybe Address
addressFromHex txt =
  case Base16.decode (Text.encodeUtf8 txt) of
    (raw, _) ->
      case Shelley.deserialiseAddr raw of
        Just addr -> Just $ AddressShelley addr
        Nothing -> either (const Nothing) (Just . AddressByron) $ Binary.decodeFull' raw

addressToHex :: Address -> Text
addressToHex addr =
  -- Text.decodeUtf8 theoretically can throw an exception but should never
  -- do so on Base16 encoded data.
  Text.decodeUtf8 . Base16.encode $
    case addr of
      AddressByron ba -> Binary.serialize' ba
      AddressShelley sa -> Shelley.serialiseAddr sa


byronGenSigningKey :: IO SigningKey
byronGenSigningKey =
    SigningKeyByron . snd <$> runSecureRandom Crypto.keyGen

shelleyGenSigningKey :: IO SigningKey
shelleyGenSigningKey =
    SigningKeyShelley . Shelley.SKey <$> runSecureRandom genKeyDSIGN

-- Given key information (public key, and other network parameters), generate an Address.
-- Originally: mkAddress :: Network -> VerificationKey -> VerificationKeyInfo -> Address
-- but since VerificationKeyInfo already has the VerificationKey and Network, it can be simplified.
-- This is true for Byron, but for Shelley there’s also an optional StakeAddressRef as input to
-- Address generation

byronVerificationKeyAddress :: VerificationKey -> Network -> Address
byronVerificationKeyAddress vkey nw =
  case vkey of
    VerificationKeyByron vk -> AddressByron $ Byron.makeVerKeyAddress (byronNetworkMagic nw) vk
    VerificationKeyShelley _ -> panic "Cardano.Api.byronVerificationKeyAddress: VerificationKeyInfoShelley"

shelleyVerificationKeyAddress :: VerificationKey -> Network -> Address
shelleyVerificationKeyAddress vkey _nw =
  case vkey of
    VerificationKeyByron _ -> panic "Cardano.Api.shelleyVerificationKeyAddress: VerificationKeyByron"
    VerificationKeyShelley vk ->
      AddressShelley $
        --TODO: we cannot use toAddr or toCred here because they unnecessarily
        -- require a full key pair, when only the pub key is needed, and that
        -- is all we have here
        Shelley.Addr (Shelley.KeyHashObj (Shelley.hashKey vk))
                     Shelley.StakeRefNull

getVerificationKey :: SigningKey -> VerificationKey
getVerificationKey kp =
  case kp of
    SigningKeyByron sk -> VerificationKeyByron vk
      where
        vk = Crypto.toVerification sk

    SigningKeyShelley (Shelley.SKey sk) -> VerificationKeyShelley (Shelley.VKey vk)
      where
        vk = deriveVerKeyDSIGN sk

byronNetworkMagic :: Network -> Byron.NetworkMagic
byronNetworkMagic nw =
  case nw of
    Mainnet -> Byron.NetworkMainOrStage
    Testnet pid -> Byron.NetworkTestnet $ unProtocolMagicId pid

-- Create new Transaction
-- ledger creates transaction and serialises it as CBOR - txBuilder
-- fine for Byron

-- Currently this is only for Byron transactions.
-- Shelly transactions will take lots of extra parameters.
-- For Shelley, transactions can be constructed from any combination of Byron and Shelley
-- transactions.
-- Any set ot inputs/outputs that only contain Byron versions should generate a Byron transaction.
-- Any set ot inputs/outputs that contain any Shelley versions should generate a Shelley transaction.
buildByronTransaction :: [TxIn] -> [TxOut] -> TxUnsigned
buildByronTransaction ins outs =
    TxUnsignedByron bTx bTxCbor bTxHash
  where
    bTx :: Byron.Tx
    bTx = Byron.UnsafeTx (NonEmpty.fromList (map toByronTxIn ins))
                         (NonEmpty.fromList (map toByronTxOut outs))
                         (Byron.mkAttributes ())
                         --TODO: handle partial conversions (non-empty)

    bTxCbor :: ByteString
    bTxCbor = Binary.serialize' bTx

    bTxHash :: Crypto.Hash Byron.Tx
    bTxHash = coerce $ Crypto.hashRaw (LBS.fromStrict bTxCbor)


buildShelleyTransaction :: [TxIn] -> [TxOut] -> SlotNo -> Lovelace -> TxUnsigned
buildShelleyTransaction txins txouts ttl fee =
    TxUnsignedShelley $
      Shelley.TxBody
        (Set.fromList (map toShelleyTxIn  txins))
        (Seq.fromList (map toShelleyTxOut txouts))
        Seq.empty                -- certificates
        (Shelley.Wdrl Map.empty) -- withdrawals
        (toShelleyLovelace fee)
        ttl
        Shelley.SNothing         -- update proposals
        Shelley.SNothing         -- metadata hash


{-
inputs outputs, attributes:
ATxAux { Tx TxWiness Annotation }

Unsigned is just a Tx

no representation difference for Signed and Checked

mkTxAux

node: signTxId

cardano-node/cardano-node/src/Cardano/CLI/Tx.hs:txSpendUTxOByronPBFT
txSpendUTxOByronPBFT (PBFT is void)
  which calls signTxId

cardano-node/cardano-node/src/Cardano/CLI/Tx.hs:txSpendGenesisUTxOByronPBFT ???

cardano-ledger/crypto/src/Cardano/Crypto/Signing/Signature.hs:sign

dont need support Redeem, do need to support Proposal and Votes (possibly Del Certs)




-}


-- Use the private key to give one witness to a transaction
-- (TxInWirtness is fine for Byron on shelley, need a TxWitness type with Byron/Shelley ctors)
witnessTransaction :: TxUnsigned -> Network -> Crypto.SigningKey -> TxWitness
witnessTransaction txu nw signKey =
    case txu of
      TxUnsignedByron _tx _txcbor txHash -> TxWitByron $ byronWitnessTransaction txHash nw signKey
      TxUnsignedShelley _tx -> panic "Cardano.Api.witnessTransaction: TxUnsignedShelley"

byronWitnessTransaction :: Crypto.Hash Byron.Tx -> Network -> Crypto.SigningKey -> Byron.TxInWitness
byronWitnessTransaction txHash nw signKey =
    Byron.VKWitness
      (Crypto.toVerification signKey)
      (Crypto.sign protocolMagic Crypto.SignTx signKey (Byron.TxSigData txHash))
  where
    -- This is unlikely to be specific to Byron or Shelley
    protocolMagic :: ProtocolMagicId
    protocolMagic =
      case nw of
        Mainnet -> Byron.mainnetProtocolMagicId
        Testnet pm -> pm

-- Sign Transaction - signTransaction is built over witnesseTransaction/signTransactionWithWitness
-- we could have this fail if the wrong (or too many/few) keys are provided, in which case it’d
-- return Transaction Checked
-- either [PrivKey] have to be in the right order, or more usable we check and reorder them get
-- them to be the right ones, since in Byron txs, witnesses are a list that has match up with the
-- tx inputs, i.e same number and in the right order. In Shelley they’re a set, so don’t need to
-- provide duplicate sigs for multiple inputs that share the same input address.
signTransaction :: TxUnsigned -> Network -> [Crypto.SigningKey] -> TxSigned
signTransaction txu nw sks =
  case txu of
    TxUnsignedByron tx txcbor txHash ->
      TxSignedByron tx txcbor txHash (Vector.fromList $ map (byronWitnessTransaction txHash nw) sks)

    TxUnsignedShelley _tx ->
--      Shelley.sign makeWitnessVKey
      panic "Cardano.Api.signTransaction: TxUnsignedShelley"


-- Verify that the transaction has been fully witnessed
-- same decision about checking or not, that all witnesses are the right ones and in the right order etc
signTransactionWithWitness :: TxUnsigned -> [Byron.TxInWitness] -> TxSigned
signTransactionWithWitness txu ws =
  case txu of
    TxUnsignedByron tx txcbor txHash ->
      TxSignedByron tx txcbor txHash (Vector.fromList ws)
    TxUnsignedShelley _tx ->
      panic "Cardano.Api.signTransactionWithWitness: TxUnsignedShelley"


-- Verify that Transaction is Complete (fully signed)
-- part of TxBuilder
-- Or we might not have this separate step at all if we bundle checking into the earlier steps of
-- tx construction, it’s a choice we have
-- For Shelley, checking that we have provided the right set of witnesses is more complicated due
-- to multisig, involves evaluating the multisig scripts to see if the necessary sigs are present.
-- Would be more complicated to check there are not too many.
--
-- It is not actually possible to implement checkTransaction because that would
-- require access to the UTxO set.
--
-- checkTransaction :: Transaction TxSigned -> Maybe (Transaction TxChecked)
-- checkTransaction = panic "Cardano.Api.checkTransaction"



-- Extract transaction information - getTransactionId may be redundant
-- part of TxBuilder
getTxSignedBody :: TxSigned -> ByteString
getTxSignedBody txs =
  case txs of
    TxSignedByron _tx txCbor _txHash _txWit -> txCbor
    TxSignedShelley -> panic "Cardano.Api.getTxSignedBody: TxUnsignedShelley"

getTxSignedHash :: TxSigned -> Crypto.Hash TxSigned
getTxSignedHash txs =
  case txs of
    TxSignedByron _tx _txCbor txHash _txWit -> coerce txHash
    TxSignedShelley -> panic "Cardano.Api.getSignedHash: TxSignedShelley"

getTxSignedWitnesses :: TxSigned -> [TxWitness]
getTxSignedWitnesses txs =
  case txs of
    TxSignedByron _tx _txCbor _txHash txWit -> map TxWitByron (Vector.toList txWit)
    TxSignedShelley -> panic "Cardano.Api.getTxSignedWitnesses: TxUnsignedShelley"

getTxUnsignedHash :: TxUnsigned -> Crypto.Hash TxUnsigned
getTxUnsignedHash txu =
  case txu of
    TxUnsignedByron _tx _txCbor txHash -> coerce txHash
    TxUnsignedShelley _tx -> panic "Cardano.Api.getTxUnsignedHash: TxUnsignedShelley"

getTxUnsignedBody :: TxUnsigned -> ByteString
getTxUnsignedBody txu =
  case txu of
    TxUnsignedByron _tx txCbor _txHash -> txCbor
    TxUnsignedShelley _tx -> panic "Cardano.Api.getTxUnsignedHash: TxUnsignedShelley"


-- Separate functons for TxUnsigned/TxSigned etc


-- getTransactionBody
-- getTransactionWitnesses
-- or separate accessor functions
-- the txid should be cached, it might be already. There was a ticket about doing that in the ledger
-- so consensus doesn’t have to do it elsewhere
