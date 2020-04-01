{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Api
  ( module X

  , Address (..)
  , KeyPair (..)
  , PubKeyInfo (..)
  , Transaction (..)
  , TxSigned (..)
  , TxUnsigned (..)

  , buildTransaction
  , genAddress
  , genByronKeyPair
  , mkPubKeyInfo
  , getTransactionId
  , getTransactionInfo
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


genByronKeyPair :: IO KeyPair
genByronKeyPair =
  -- Currently not possible to generate KeyPairShelley.
  uncurry KeyPairByron <$> runSecureRandom Crypto.keyGen

-- Obtain Key, Network and Address Information
--    - not needed for use in AWS by exchanges, can be done offline and then plugged in
--    key is just sequence of 64 bytes, any arbitrary sequence can be used
--    bit-shifting if needed for derivation
-- This was called 'getPubKey' but its type signature is the same as the 'PubKeyInfo' constructor.
mkPubKeyInfo :: KeyPair -> Network -> PubKeyInfo
mkPubKeyInfo kp nw =
  case kp of
    KeyPairByron bpub _ -> PubKeyInfoByron nw bpub
    KeyPairShelley -> panic "Cardano.Api.mkPubKeyInfo: KeyPairShelley"

-- Given key information (public key, and other network parameters), generate an Address.
-- Originally: mkAddress :: Network -> PubKey -> PubKeyInfo -> Address
-- but since PubKeyInfo already has the PublicKey and Network, it can be simplified.
-- This is true for Byron, but for Shelley there’s also an optional StakeAddressRef as input to
-- Address generation
genAddress :: PubKeyInfo -> Address
genAddress pki =
  case pki of
    PubKeyInfoByron _nw _bpk -> panic "Cardano.Api.genAddress: PubKeyInfoByron"
    PubKeyInfoShelley -> panic "Cardano.Api.genAddress: PubKeyInfoShelley"

-- Create new Transaction
-- ledger creates transaction and serialises it as CBOR - txBuilder
-- fine for Byron
-- lots of extra params for Shelley
-- Currently this is only for Byron transactions, but will probably need to be extended
-- for Shelley.
buildTransaction :: NonEmpty Byron.TxIn -> NonEmpty Byron.TxOut -> TxUnsigned
buildTransaction ins outs =
  ByronTxUnsigned $ Byron.UnsafeTx ins outs (Byron.mkAttributes ())


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
witnessTransaction :: TxUnsigned -> Network -> Crypto.SigningKey -> Byron.TxInWitness
witnessTransaction txu nw signKey =
    case txu of
      ByronTxUnsigned tx -> byronWitness tx
      ShelleyTxUnsigned -> panic "Cardano.Api.witnessTransaction: ShelleyTxUnsigned"
  where
    byronWitness :: Byron.Tx -> Byron.TxInWitness
    byronWitness tx =
      Byron.VKWitness
        (Crypto.toVerification signKey)
        (Crypto.sign protocolMagic Crypto.SignTx signKey (Byron.TxSigData $ Crypto.hash tx))

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
    ByronTxUnsigned tx ->
      ByronTxSigned tx (Vector.fromList $ map (witnessTransaction txu nw) sks)
    ShelleyTxUnsigned ->
      panic "Cardano.Api.witnessTransaction: ShelleyTxUnsigned"




-- Verify that the transaction has been fully witnessed
-- same decision about checking or not, that all witnesses are the right ones and in the right order etc
signTransactionWithWitness :: TxUnsigned -> [Byron.TxInWitness] -> TxSigned
signTransactionWithWitness txu ws =
  case txu of
    ByronTxUnsigned tx ->
      ByronTxSigned tx (Vector.fromList ws)
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

getTransactionInfo :: Transaction status -> (transactionId, txBody, [Byron.TxWitness])
getTransactionInfo = panic "Cardano.Api.getTransactionInfo"
-- or separate accessor functions
-- the txid should be cached, it might be already. There was a ticket about doing that in the ledger
-- so consensus doesn’t have to do it elsewhere









{-
Duncan Coutts:
> "Bulk" version that signs all in one go, ie making signature style witnesses
  for a list of private keys, and producing a signed tx (ie a fancy pair of tx
  body and the witnesses).

    signTransaction :: TxUnsigned -> Network -> [privKey] -> signedTx

  What should be the incremental interface that just signs for a single key, and
  returns not a signed tx, but a singular witness, detached from the tx body.

    witnessTransaction :: TxUnsigned -> Network -> SigningKey -> TxWitnessed

  So TxWitnessed there should be perhaps TxWitness, a singular witness for a tx.
  And then to actually make use of that incremental approach you need to be
  able to take the TxUnsigned and the [TxWitness] that you made using
  witnessTransaction, and bundle them together into a signed tx (the fancy pair
  of the tx body and the witnesses).

    signTransactionWithWitness :: Transaction TxUnsigned -> [TxWitness] -> Transaction TxSigned

> And "checking" a signed tx (ie tx body and witnesses) is making sure that:

  * the witnesses we have got are individually right (correspond to the ones
    needed by the tx inputs, which we can check using signature verification); and
  * are in the right order (for Byron); and
  * cover all the inputs in the tx body and no more;For Byron that's relatively
    simple since we just zip the witnesses and the inputs together and expect
    them to be the same length, and then do signature verification for them all.
    For Shelley with the set semantics it's a tad different, but same principle.

> Oh, hmm, I just realised something. :slightly_smiling_face:We cannot do the
  full checking without knowing the addresses for the inputs, which needs the
  UTxO. So we cannot do the final check which is that the pubkey for each input
  corresponds to the pubkey hash from the utxo. So without access to the utxo
  we're severely limited in the checking we can really do. Hmmmm.

> So actually we can only check that the signature is self-consistent (which it
  will be by construction if one uses our API), and that we have the right
  number of them. We cannot actually check the correspondence between the
  witnesses and the inputs without the utxo. So we cannot check the order is
  right.

> But of course this gets checked by the node when you submit it.

> So we might want to revise whether it's worth having a separate checked state
  if it's so limited.

-}
