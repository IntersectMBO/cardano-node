{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wextra #-}

module Cardano.Benchmarking.GeneratorTx.Tx.Byron
  ( normalByronTxToGenTx
  , byronGenesisUTxOTxIn
  )
where

import           Cardano.Prelude hiding (option, trace, (%))
import           Prelude (error)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Formatting (sformat, (%))

import           Cardano.Chain.Common (Address)
import qualified Cardano.Chain.Common as Common
import           Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.UTxO as UTxO
import qualified Cardano.Crypto.Signing as Crypto

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock, GenTx (..))
import qualified Ouroboros.Consensus.Byron.Ledger as Byron

-- | The 'GenTx' is all the kinds of transactions that can be submitted
-- and \"normal\" Byron transactions are just one of the kinds.
normalByronTxToGenTx :: UTxO.ATxAux ByteString -> GenTx ByronBlock
normalByronTxToGenTx tx' = Byron.ByronTx (Byron.byronIdTx tx') tx'

-- | Given a genesis, and a pair of a genesis public key and address,
--   reconstruct a TxIn corresponding to the genesis UTxO entry.
byronGenesisUTxOTxIn :: Genesis.Config -> Crypto.VerificationKey -> Common.Address -> UTxO.TxIn
byronGenesisUTxOTxIn gc vk genAddr =
  handleMissingAddr $ fst <$> Map.lookup genAddr initialUtxo
 where
  initialUtxo :: Map Common.Address (UTxO.TxIn, UTxO.TxOut)
  initialUtxo =
        Map.fromList
      . mapMaybe (\(inp, out) -> mkEntry inp genAddr <$> keyMatchesUTxO vk out)
      . map (bimap UTxO.fromCompactTxIn UTxO.fromCompactTxOut)
      . Map.toList
      . UTxO.unUTxO
      . UTxO.genesisUtxo
      $ gc

  mkEntry :: UTxO.TxIn
          -> Address
          -> UTxO.TxOut
          -> (Address, (UTxO.TxIn, UTxO.TxOut))
  mkEntry inp addr out = (addr, (inp, out))

  keyMatchesUTxO :: Crypto.VerificationKey -> UTxO.TxOut -> Maybe UTxO.TxOut
  keyMatchesUTxO key out =
    if Common.checkVerKeyAddress key (UTxO.txOutAddress out)
    then Just out else Nothing

  handleMissingAddr :: Maybe UTxO.TxIn -> UTxO.TxIn
  handleMissingAddr  = fromMaybe . error
    $  "\nGenesis UTxO has no address\n"
    <> T.unpack (prettyAddress genAddr)
    <> "\n\nIt has the following, though:\n\n"
    <> Cardano.Prelude.concat (T.unpack . prettyAddress <$> Map.keys initialUtxo)

  prettyAddress :: Common.Address -> Text
  prettyAddress addr = sformat
    (Common.addressF %"\n"%Common.addressDetailedF)
    addr addr
