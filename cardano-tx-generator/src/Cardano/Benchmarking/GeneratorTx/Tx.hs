{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Benchmarking.GeneratorTx.Tx
  ( toCborTxAux
  , normalByronTxToGenTx
  , txSpendGenesisUTxOByronPBFT
  )
where

import           Prelude (error)
import           Cardano.Prelude hiding (option, trace, (%))

import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting ((%), sformat)

import           Cardano.Chain.Common (Address)
import qualified Cardano.Chain.Common as Common
import           Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.UTxO ( mkTxAux, annotateTxAux
                                    , Tx(..), TxId, TxOut)
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.Crypto (SigningKey(..), ProtocolMagicId)
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Byron.Ledger (GenTx(..), ByronBlock)

toCborTxAux :: UTxO.ATxAux ByteString -> LB.ByteString
toCborTxAux = LB.fromStrict . UTxO.aTaAnnotation -- The ByteString anotation is the CBOR encoded version.

-- | The 'GenTx' is all the kinds of transactions that can be submitted
-- and \"normal\" Byron transactions are just one of the kinds.
normalByronTxToGenTx :: UTxO.ATxAux ByteString -> GenTx ByronBlock
normalByronTxToGenTx tx' = Byron.ByronTx (Byron.byronIdTx tx') tx'

-- | Generate a transaction spending genesis UTxO at a given address,
--   to given outputs, signed by the given key.
txSpendGenesisUTxOByronPBFT
  :: Genesis.Config
  -> SigningKey
  -> Address
  -> NonEmpty TxOut
  -> UTxO.ATxAux ByteString
txSpendGenesisUTxOByronPBFT gc sk genAddr outs =
    annotateTxAux $ mkTxAux tx (pure wit)
  where
    tx = UnsafeTx (pure txIn) outs txattrs

    wit = signTxId (configProtocolMagicId gc) sk (Crypto.hash tx)

    txIn :: UTxO.TxIn
    txIn  = genesisUTxOTxIn gc (Crypto.toVerification sk) genAddr

    txattrs = Common.mkAttributes ()

-- | Given a Tx id, produce a UTxO Tx input witness, by signing it
--   with respect to a given protocol magic.
signTxId :: ProtocolMagicId -> SigningKey -> TxId -> UTxO.TxInWitness
signTxId pmid sk txid =
  UTxO.VKWitness
  (Crypto.toVerification sk)
  (Crypto.sign
    pmid
    Crypto.SignTx
    sk
    (UTxO.TxSigData txid))

-- | Given a genesis, and a pair of a genesis public key and address,
--   reconstruct a TxIn corresponding to the genesis UTxO entry.
genesisUTxOTxIn :: Genesis.Config -> Crypto.VerificationKey -> Common.Address -> UTxO.TxIn
genesisUTxOTxIn gc vk genAddr =
  handleMissingAddr $ fst <$> Map.lookup genAddr initialUtxo
  where
    initialUtxo :: Map Common.Address (UTxO.TxIn, UTxO.TxOut)
    initialUtxo =
          Map.fromList
        . mapMaybe (\(inp, out) -> mkEntry inp genAddr <$> keyMatchesUTxO vk out)
        . fromCompactTxInTxOutList
        . Map.toList
        . UTxO.unUTxO
        . UTxO.genesisUtxo
        $ gc
      where
        mkEntry :: UTxO.TxIn
                -> Address
                -> UTxO.TxOut
                -> (Address, (UTxO.TxIn, UTxO.TxOut))
        mkEntry inp addr out = (addr, (inp, out))

    fromCompactTxInTxOutList :: [(UTxO.CompactTxIn, UTxO.CompactTxOut)]
                             -> [(UTxO.TxIn, UTxO.TxOut)]
    fromCompactTxInTxOutList =
        map (bimap UTxO.fromCompactTxIn UTxO.fromCompactTxOut)

    keyMatchesUTxO :: Crypto.VerificationKey -> UTxO.TxOut -> Maybe UTxO.TxOut
    keyMatchesUTxO key out =
      if Common.checkVerKeyAddress key (UTxO.txOutAddress out)
      then Just out else Nothing

    handleMissingAddr :: Maybe UTxO.TxIn -> UTxO.TxIn
    handleMissingAddr  = fromMaybe . error
      $  "\nGenesis UTxO has no address\n"
      <> (T.unpack $ prettyAddress genAddr)
      <> "\n\nIt has the following, though:\n\n"
      <> Cardano.Prelude.concat (T.unpack . prettyAddress <$> Map.keys initialUtxo)

    prettyAddress :: Common.Address -> Text
    prettyAddress addr = sformat
      (Common.addressF %"\n"%Common.addressDetailedF)
      addr addr
