{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.Shelley.Run.SingleAddressWallet
  ( SingleAddressWalletError (..)
  , buildSingleAddressWalletTransaction
  ) where

import           Cardano.Prelude

import           Cardano.Api (Address (..), LocalStateQueryError, Lovelace (..), Network (..),
                     PaymentVerificationKey (..), QueryFilter (..), SigningKey (..), TxId (..),
                     TxIn (..), TxOut (..), TxSigned (..), TxUnsigned (..),
                     buildDummyShelleyTxForFeeCalc, buildShelleyTransaction,
                     calculateShelleyMinFee, getLocalTip, queryPParamsFromLocalState,
                     queryUTxOFromLocalState, shelleyVerificationKeyAddress, signTransaction)

import           Cardano.Config.Shelley.ColdKeys (deriveVerKey)
import           Cardano.Config.Types (SocketPath)

import qualified Cardano.Crypto.Hash.Class   as Crypto

import           Cardano.Slotting.Slot (SlotNo (..), fromWithOrigin)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistMaybe)

import qualified Data.Map as Map
import qualified Data.Set as Set

import           Ouroboros.Consensus.Block (CodecConfig)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)

import           Ouroboros.Network.Block (getTipPoint, pointSlot)
import           Ouroboros.Network.NodeToClient (withIOManager)

import qualified Shelley.Spec.Ledger.Address as Shelley (Addr (..))
import qualified Shelley.Spec.Ledger.Coin as Shelley (Coin (..))
import qualified Shelley.Spec.Ledger.TxData as Shelley (TxId (..), TxIn (..), TxOut (..))
import qualified Shelley.Spec.Ledger.UTxO as Shelley (UTxO (..))

data SingleAddressWalletError
  = SingleAddressWalletError
  | SingleAddressWalletLocalStateQueryError !LocalStateQueryError
  deriving Show

-- | Build a basic transaction that consists of a single input and output
-- and covers its own fees.
--
-- For the input, we query the node's local state for a UTxO that is
-- sufficient to cover the min fee of the transaction that will be
-- constructed. For the output, we simply spend the change back to the sender.
buildSingleAddressWalletTransaction
  :: CodecConfig (ShelleyBlock TPraosStandardCrypto)
  -> Network
  -> SocketPath
  -> Maybe SlotNo
  -> SigningKey
  -> ExceptT SingleAddressWalletError IO TxSigned
buildSingleAddressWalletTransaction _ _ _ _ (SigningKeyByron _) =
  panic "buildSingleAddressWalletTransaction: SigningKeyByron"
buildSingleAddressWalletTransaction codecCfg network sockPath mbTxTtl
                                    skey@(SigningKeyShelley shelleySkey) = do
    -- Query the local tip from chain sync protocol
    tipPoint <- liftIO $ withIOManager $ \iocp ->
      getTipPoint <$> getLocalTip iocp codecCfg network sockPath

    -- Query the PParams from the node's local state
    pparams <- firstExceptT SingleAddressWalletLocalStateQueryError $
      queryPParamsFromLocalState network sockPath tipPoint

    -- Build a transaction containing one dummy input (representing an input
    -- that can cover the fee) and one dummy output (representing the change
    -- output).
    --
    -- We then use this dummy transaction to calculate the min fee which we'll
    -- utilize when constructing the real transaction.
    let txTtl = chooseTxTtl tipPoint
        dummyTxForFeeCalc = buildDummyShelleyTx txTtl
        minFee = calculateShelleyMinFee pparams dummyTxForFeeCalc

    -- Query the UTxO from the node's local state
    utxo <- firstExceptT SingleAddressWalletLocalStateQueryError $
      queryUTxOFromLocalState network sockPath (FilterByAddress $ Set.singleton addr) tipPoint

    -- Find an unspent transaction output that can be used to cover the fee.
    (txIn, TxOut _ outAmount) <- hoistMaybe SingleAddressWalletError $
      findUTxOToCoverFee utxo minFee

    -- Construct a transaction output to send the change back to the origin address
    let txOut = TxOut addr $ Lovelace (unLoveLace outAmount - unLoveLace minFee)

    -- Construct and return the legitimate transaction.
    pure $ buildAndSignShelleyTx
      [txIn]
      [txOut]
      txTtl
      minFee
      network
      [skey]
  where
    numDummyTxIns = 1
    numDummyTxOuts = 1
    vkey = PaymentVerificationKeyShelley $ deriveVerKey shelleySkey
    addr = shelleyVerificationKeyAddress vkey Nothing

    -- If the transaction TTL was provided, use that. Otherwise, use
    -- @tipPointSlotNo + 100@.
    chooseTxTtl tipPoint =
      fromMaybe
        ((fromWithOrigin (SlotNo 0) . pointSlot) tipPoint + 100)
        mbTxTtl

    buildDummyShelleyTx :: SlotNo -> TxSigned
    buildDummyShelleyTx txTtl =
      buildDummyShelleyTxForFeeCalc
        numDummyTxIns
        numDummyTxOuts
        txTtl
        network
        [skey]
        []

buildAndSignShelleyTx
  :: [TxIn]
  -> [TxOut]
  -> SlotNo
  -> Lovelace
  -> Network
  -> [SigningKey]
  -> TxSigned
buildAndSignShelleyTx txIns txOuts ttl fee network skeys =
    signTransaction unsignedTx network skeys
  where
    unsignedTx :: TxUnsigned
    unsignedTx = buildShelleyTransaction txIns txOuts ttl fee [] Nothing

fromShelleyAddress :: Shelley.Addr TPraosStandardCrypto -> Address
fromShelleyAddress = AddressShelley

fromShelleyCoin :: Shelley.Coin -> Lovelace
fromShelleyCoin (Shelley.Coin amount) = Lovelace amount

fromShelleyTxId :: Shelley.TxId crypto -> TxId
fromShelleyTxId (Shelley.TxId (Crypto.UnsafeHash h)) = TxId (Crypto.UnsafeHash h)

fromShelleyTxIn :: Shelley.TxIn crypto -> TxIn
fromShelleyTxIn (Shelley.TxIn txid ix) = TxIn (fromShelleyTxId txid) (fromIntegral ix)

fromShelleyTxOut :: Shelley.TxOut TPraosStandardCrypto -> TxOut
fromShelleyTxOut (Shelley.TxOut addr amount) =
  TxOut (fromShelleyAddress addr) (fromShelleyCoin amount)

fromShelleyTxInTxOutPair
  :: (Shelley.TxIn TPraosStandardCrypto, Shelley.TxOut TPraosStandardCrypto)
  -> (TxIn, TxOut)
fromShelleyTxInTxOutPair (txin, txout) = (fromShelleyTxIn txin, fromShelleyTxOut txout)

-- | From the provided UTxO collection, pick the first encountered UTxO that
-- can be used to cover the specified fee.
findUTxOToCoverFee
  :: Shelley.UTxO TPraosStandardCrypto
  -> Lovelace
  -> Maybe (TxIn, TxOut)
findUTxOToCoverFee (Shelley.UTxO utxoMap) (Lovelace fee) =
    fromShelleyTxInTxOutPair
      <$> headMay (Map.toList $ Map.filter feeLessThanOutAmount utxoMap)
  where
    feeLessThanOutAmount (Shelley.TxOut _ outAmount) = fee < fromIntegral outAmount
