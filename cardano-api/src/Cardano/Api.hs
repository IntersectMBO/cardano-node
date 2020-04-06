{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Api
  ( module X

  , Address (..)
  , KeyPair (..)
  , Network (..)
  , PublicKey (..)
  , Transaction (..)
  , TxSigned (..)
  , TxUnsigned (..)

  , buildTransaction
  , byronPubKeyAddress
  , byronGenKeyPair
  , getTransactionId
  , getTransactionInfo
  , mkPublicKey
  , signTransaction
  , witnessTransaction
  , signTransactionWithWitness
  , submitTransaction
  ) where

import           Cardano.Api.TxSubmit

import           Cardano.Prelude

import           Cardano.Api.Types
import           Cardano.Api.CBOR as X
import           Cardano.Api.Error as X
import           Cardano.Api.View as X

import qualified Cardano.Crypto.Hashing as Crypto
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import           Cardano.Crypto.Random (runSecureRandom)
import qualified Cardano.Crypto.Signing as Crypto

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO as Byron

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Vector as Vector


byronGenKeyPair :: IO KeyPair
byronGenKeyPair =
  -- Currently not possible to generate KeyPairShelley.
  uncurry KeyPairByron <$> runSecureRandom Crypto.keyGen

-- Given key information (public key, and other network parameters), generate an Address.
-- Originally: mkAddress :: Network -> PubKey -> PubKeyInfo -> Address
-- but since PubKeyInfo already has the PublicKey and Network, it can be simplified.
-- This is true for Byron, but for Shelley there’s also an optional StakeAddressRef as input to
-- Address generation
byronPubKeyAddress :: PublicKey -> Address
byronPubKeyAddress pk =
  case pk of
    PubKeyByron nw vk -> AddressByron $ Byron.makeVerKeyAddress (byronNetworkMagic nw) vk
    PubKeyShelley -> panic "Cardano.Api.byronPubKeyAddress: PubKeyInfoShelley"

mkPublicKey :: KeyPair -> Network -> PublicKey
mkPublicKey kp nw =
  case kp of
    KeyPairByron vk _ -> PubKeyByron nw vk
    KeyPairShelley -> PubKeyShelley

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
buildTransaction :: NonEmpty Byron.TxIn -> NonEmpty Byron.TxOut -> TxUnsigned
buildTransaction ins outs =
  TxUnsignedByron $ Byron.UnsafeTx ins outs (Byron.mkAttributes ())


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
witnessTransaction :: TxUnsigned -> Network -> Crypto.SigningKey -> Byron.TxInWitness -- (TxInWirtness is fine for Byrin on shelley, need a TxWitness type with Byron/Shelley ctors)
witnessTransaction txu nw signKey =
    case txu of
      TxUnsignedByron tx -> byronWitness tx
      ShelleyTxUnsigned -> panic "Cardano.Api.witnessTransaction: ShelleyTxUnsigned"
  where
    byronWitness :: Byron.Tx -> Byron.TxInWitness
    byronWitness tx =
      Byron.VKWitness
        (Crypto.toVerification signKey)
        (Crypto.sign protocolMagic Crypto.SignTx signKey (Byron.TxSigData $ Crypto.hash tx))

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
    TxUnsignedByron tx ->
      TxSignedByron tx (Vector.fromList $ map (witnessTransaction txu nw) sks)
    ShelleyTxUnsigned ->
      panic "Cardano.Api.witnessTransaction: ShelleyTxUnsigned"




-- Verify that the transaction has been fully witnessed
-- same decision about checking or not, that all witnesses are the right ones and in the right order etc
signTransactionWithWitness :: TxUnsigned -> [Byron.TxInWitness] -> TxSigned
signTransactionWithWitness txu ws =
  case txu of
    TxUnsignedByron tx ->
      TxSignedByron tx (Vector.fromList ws)
    ShelleyTxUnsigned ->
      panic "Cardano.Api.signTransactionWithWitness: ShelleyTxUnsigned"


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
getTransactionId :: Transaction status -> transactionId
getTransactionId = panic "Cardano.Api.getTransactionId"

-- Separate functons for TxUnsigned/TxSigned etc


-- getTransactionBody
-- getTransactionWitnesses
getTransactionInfo :: Transaction status -> (txBody {- unsigned -}, [Byron.TxWitness])
getTransactionInfo = panic "Cardano.Api.getTransactionInfo"
-- or separate accessor functions
-- the txid should be cached, it might be already. There was a ticket about doing that in the ledger
-- so consensus doesn’t have to do it elsewhere
