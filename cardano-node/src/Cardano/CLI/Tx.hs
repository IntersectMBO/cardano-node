{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Tx
  ( prettyAddress
  , txSpendUTxOByronPBFT
  , txSpendGenesisUTxOByronPBFT
  )
where

import           Prelude (error)
import           Cardano.Prelude hiding (option, show, trace)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Formatting as F

import           Cardano.Binary (reAnnotate)
import           Cardano.Chain.Common
import qualified Cardano.Chain.Common as CC.Common
import           Cardano.Chain.Genesis as CC.Genesis
import           Cardano.Chain.UTxO
import qualified Cardano.Chain.UTxO as CC.UTxO
import           Cardano.Crypto (SigningKey(..), ProtocolMagicId)
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config


prettyAddress :: CC.Common.Address -> Text
prettyAddress addr = TL.toStrict
  $  F.format CC.Common.addressF         addr <> "\n"
  <> F.format CC.Common.addressDetailedF addr

txSpendUTxOByronPBFT
  :: CC.Genesis.Config
  -> SigningKey
  -> NonEmpty TxIn
  -> NonEmpty TxOut
  -> GenTx (ByronBlockOrEBB ByronConfig)
txSpendUTxOByronPBFT gc sk ins outs =
  let txattrs  = mkAttributes ()
      tx       = UnsafeTx ins outs txattrs
      wit      = signTxId (configProtocolMagicId gc) sk (Crypto.hash tx)
      ATxAux atx awit =
        mkTxAux tx . V.fromList . take (NE.length ins) $ repeat wit
  in mkByronTx $ ATxAux (reAnnotate atx) (reAnnotate awit)

txSpendGenesisUTxOByronPBFT
  :: CC.Genesis.Config
  -> SigningKey
  -> Address
  -> NonEmpty TxOut
  -> GenTx (ByronBlockOrEBB ByronConfig)
txSpendGenesisUTxOByronPBFT gc sk genAddr outs =
  let txattrs  = mkAttributes ()
      tx       = UnsafeTx (pure txIn) outs txattrs
      txIn    :: CC.UTxO.TxIn
      txIn     = genesisUTxOTxIn gc sk genAddr
      wit      = signTxId (configProtocolMagicId gc) sk (Crypto.hash tx) 
      ATxAux atx awit = mkTxAux tx . V.fromList . pure $ wit
  in mkByronTx $ ATxAux (reAnnotate atx) (reAnnotate awit)

signTxId :: ProtocolMagicId -> SigningKey -> TxId -> CC.UTxO.TxInWitness
signTxId pmid sk txid = CC.UTxO.VKWitness
  (Crypto.toVerification sk)
  (Crypto.sign
    pmid
    Crypto.SignTx
    sk
    (CC.UTxO.TxSigData txid))

-- | Given a genesis, and a pair of signing key and address, reconstruct a TxIn
--   corresponding to the genesis UTxO entry.
genesisUTxOTxIn :: CC.Genesis.Config -> SigningKey -> Address -> CC.UTxO.TxIn
genesisUTxOTxIn gc genSk genAddr =
  let vk         = Crypto.toVerification genSk
      handleMissingAddr :: Maybe CC.UTxO.TxIn -> CC.UTxO.TxIn
      handleMissingAddr  = fromMaybe . error
        $  "\nGenesis UTxO has no address\n"
        <> (T.unpack $ prettyAddress genAddr)
        <> "\n\nIt has the following, though:\n\n"
        <> Cardano.Prelude.concat (T.unpack . prettyAddress <$> Map.keys initialUtxo)

      initialUtxo :: Map Address (CC.UTxO.TxIn, CC.UTxO.TxOut)
      initialUtxo =
            Map.fromList
          . mapMaybe (\(inp, out) -> mkEntry inp genAddr <$> keyMatchesUTxO vk out)
          . fromCompactTxInTxOutList
          . Map.toList
          . CC.UTxO.unUTxO
          . CC.UTxO.genesisUtxo
          $ gc
        where
          mkEntry :: CC.UTxO.TxIn
                  -> Address
                  -> CC.UTxO.TxOut
                  -> (Address, (CC.UTxO.TxIn, CC.UTxO.TxOut))
          mkEntry inp addr out = (addr, (inp, out))

      keyMatchesUTxO :: Crypto.VerificationKey -> CC.UTxO.TxOut -> Maybe CC.UTxO.TxOut
      keyMatchesUTxO key out =
        if CC.Common.checkVerKeyAddress key (CC.UTxO.txOutAddress out)
        then Just out else Nothing

      fromCompactTxInTxOutList :: [(CC.UTxO.CompactTxIn, CC.UTxO.CompactTxOut)]
                               -> [(CC.UTxO.TxIn, CC.UTxO.TxOut)]
      fromCompactTxInTxOutList =
          map (bimap CC.UTxO.fromCompactTxIn CC.UTxO.fromCompactTxOut)
  in handleMissingAddr $ fst <$> Map.lookup genAddr initialUtxo
