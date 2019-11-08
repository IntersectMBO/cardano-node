{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Tx
  ( TxFile(..)
  , NewTxFile(..)
  , prettyAddress
  , readByronTx
  , txSpendGenesisUTxOByronPBFT
  , issueGenesisUTxOExpenditure
  , txSpendUTxOByronPBFT
  , issueUTxOExpenditure
  , nodeSubmitTx
  , withRealPBFT
  )
where

import           Prelude (error, show)
import           Cardano.Prelude hiding (option, show, trace, (%))

import           Codec.Serialise (deserialiseOrFail)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting ((%), sformat)

import           Control.Tracer (stdoutTracer)

import           Cardano.Chain.Common (Address)
import qualified Cardano.Chain.Common as Common
import           Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.MempoolPayload as CC.Mempool
import           Cardano.Chain.UTxO ( mkTxAux, annotateTxAux
                                    , Tx(..), TxId, TxIn, TxOut)
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.Crypto (SigningKey(..), ProtocolMagicId)
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import           Ouroboros.Consensus.Ledger.Byron (GenTx(..), ByronBlockOrEBB)
import           Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig)
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Node.Run (RunNode)

import           Cardano.CLI.Ops
import           Cardano.CLI.Tx.Submission
import           Cardano.Common.Orphans ()
import           Cardano.Config.Protocol
import           Cardano.Config.Types (NodeCLI, NodeConfiguration(..))
import           Cardano.Config.Topology


newtype TxFile =
  TxFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewTxFile =
  NewTxFile FilePath
  deriving (Eq, Ord, Show, IsString)


-- | Pretty-print an address in its Base58 form, and also
--   its full structure.
prettyAddress :: Common.Address -> Text
prettyAddress addr = sformat
  (Common.addressF %"\n"%Common.addressDetailedF)
  addr addr

readByronTx :: TxFile -> IO (GenTx (ByronBlockOrEBB ByronConfig))
readByronTx (TxFile fp) = do
  txBS <- LB.readFile fp
  case deserialiseOrFail txBS of
    Left e -> throwIO $ TxDeserialisationFailed fp e
    Right tx -> pure tx

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

-- | Perform an action that expects ProtocolInfo for Byron/PBFT,
--   with attendant configuration.
withRealPBFT
  :: NodeConfiguration
  -> NodeCLI
  -> (RunNode (ByronBlockOrEBB ByronConfig)
      => Consensus.Protocol (ByronBlockOrEBB ByronConfig)
      -> IO a)
  -> IO a
withRealPBFT nc nCli action = do
  SomeProtocol p <- fromProtocol nc nCli $ ncProtocol nc
  case p of
    proto@Consensus.ProtocolRealPBFT{} -> action proto
    _ -> throwIO $ ProtocolNotSupported (ncProtocol nc)

-- | Generate a transaction spending genesis UTxO at a given address,
--   to given outputs, signed by the given key.
txSpendGenesisUTxOByronPBFT
  :: Genesis.Config
  -> SigningKey
  -> Address
  -> NonEmpty TxOut
  -> GenTx (ByronBlockOrEBB ByronConfig)
txSpendGenesisUTxOByronPBFT gc sk genAddr outs =
    Byron.mkByronGenTx
      $ CC.Mempool.MempoolTx $ annotateTxAux $ mkTxAux tx (pure wit)
  where
    tx = UnsafeTx (pure txIn) outs txattrs

    wit = signTxId (configProtocolMagicId gc) sk (Crypto.hash tx)

    txIn :: UTxO.TxIn
    txIn  = genesisUTxOTxIn gc (Crypto.toVerification sk) genAddr

    txattrs = Common.mkAttributes ()

-- | Generate a transaction spending genesis UTxO at a given address,
--   to given outputs, signed by the given key.
issueGenesisUTxOExpenditure
  :: Address
  -> NonEmpty TxOut
  -> NodeConfiguration
  -> NodeCLI
  -> Crypto.SigningKey
  -> IO (GenTx (ByronBlockOrEBB ByronConfig))
issueGenesisUTxOExpenditure genRichAddr outs nc nCli sk = do
  withRealPBFT nc nCli $
    \(Consensus.ProtocolRealPBFT gc _ _ _ _)-> do
      case txSpendGenesisUTxOByronPBFT gc sk genRichAddr outs of
        tx@(ByronTx txid _) -> do
          putStrLn $ sformat ("TxId: "%Crypto.hashHexF) txid
          pure tx
        x ->
          throwIO $ InvariantViolation $
          "Invariant violation:  a non-ByronTx GenTx out of 'txSpendUTxOByronPBFT': " <> show x

-- | Generate a transaction from given Tx inputs to outputs,
--   signed by the given key.
txSpendUTxOByronPBFT
  :: Genesis.Config
  -> SigningKey
  -> NonEmpty TxIn
  -> NonEmpty TxOut
  -> GenTx (ByronBlockOrEBB ByronConfig)
txSpendUTxOByronPBFT gc sk ins outs =
    Byron.mkByronGenTx
      $ CC.Mempool.MempoolTx $ annotateTxAux $ mkTxAux tx (pure wit)
  where
    tx = UnsafeTx ins outs txattrs

    wit = signTxId (configProtocolMagicId gc) sk (Crypto.hash tx)

    txattrs = Common.mkAttributes ()

-- | Generate a transaction from given Tx inputs to outputs,
--   signed by the given key.
issueUTxOExpenditure
  :: NonEmpty TxIn
  -> NonEmpty TxOut
  -> NodeConfiguration
  -> NodeCLI
  -> Crypto.SigningKey
  -> IO (GenTx (ByronBlockOrEBB ByronConfig))
issueUTxOExpenditure ins outs nc nCli key = do
  withRealPBFT nc nCli $
    \(Consensus.ProtocolRealPBFT gc _ _ _ _)-> do
      case txSpendUTxOByronPBFT gc key ins outs of
        tx@(ByronTx txid _) -> do
          putStrLn $ sformat ("TxId: "%Crypto.hashHexF) txid
          pure tx
        x ->
          throwIO $ InvariantViolation $
          "Invariant violation:  a non-ByronTx GenTx out of 'txSpendUTxOByronPBFT': " <> show x

-- | Submit a transaction to a node specified by topology info.
nodeSubmitTx
  :: TopologyInfo
  -> NodeConfiguration
  -> NodeCLI
  -> GenTx (ByronBlockOrEBB ByronConfig)
  -> IO ()
nodeSubmitTx topology nc nCli gentx =
  withRealPBFT nc nCli $
  \p@Consensus.ProtocolRealPBFT{} -> do
    case gentx of
      ByronTx txid _ -> putStrLn $ sformat ("TxId: "%Crypto.hashHexF) txid
      _ -> pure ()
    handleTxSubmission nCli p topology gentx stdoutTracer
