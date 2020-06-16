{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.CLI.Byron.Tx
  ( ByronTxError(..)
  , TxFile(..)
  , NewTxFile(..)
  , prettyAddress
  , readByronTx
  , normalByronTxToGenTx
  , txSpendGenesisUTxOByronPBFT
  , txSpendUTxOByronPBFT
  , nodeSubmitTx
  , renderByronTxError

    --TODO: remove when they are exported from the ledger
  , fromCborTxAux
  , toCborTxAux
  )
where

import           Prelude (error)
import           Cardano.Prelude hiding (option, trace, (%))

import           Control.Monad.Trans.Except.Extra (firstExceptT, left)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import           Formatting ((%), sformat)

import           Control.Tracer (traceWith, nullTracer, stdoutTracer)

import           Cardano.Api (textShow, toByronProtocolMagic)
import qualified Cardano.Binary as Binary

import           Cardano.Chain.Common (Address)
import qualified Cardano.Chain.Common as Common
import           Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochSlots(..))
import           Cardano.Chain.UTxO ( mkTxAux, annotateTxAux
                                    , Tx(..), TxId, TxIn, TxOut)
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.Crypto (SigningKey(..), ProtocolMagicId)
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import           Ouroboros.Network.NodeToClient (IOManager)

import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Byron.Ledger (GenTx(..), ByronBlock)
import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Cardano
                   (protocolClientInfo, SecurityParam(..))

import           Cardano.Api (Network, submitGenTx)
import           Cardano.Api.Protocol.Byron (mkNodeClientProtocolByron)
import           Cardano.CLI.Environment


data ByronTxError
  = TxDeserialisationFailed !FilePath !Binary.DecoderError
  | EnvSocketError !EnvSocketError
  deriving Show

renderByronTxError :: ByronTxError -> Text
renderByronTxError err =
  case err of
    TxDeserialisationFailed txFp decErr ->
      "Transaction deserialisation failed at " <> textShow txFp <> " Error: " <> textShow decErr
    EnvSocketError envSockErr -> renderEnvSocketError envSockErr


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

readByronTx :: TxFile -> ExceptT ByronTxError IO (GenTx ByronBlock)
readByronTx (TxFile fp) = do
  txBS <- liftIO $ LB.readFile fp
  case fromCborTxAux txBS of
    Left e -> left $ TxDeserialisationFailed fp e
    Right tx -> pure (normalByronTxToGenTx tx)

-- | The 'GenTx' is all the kinds of transactions that can be submitted
-- and \"normal\" Byron transactions are just one of the kinds.
normalByronTxToGenTx :: UTxO.ATxAux ByteString -> GenTx ByronBlock
normalByronTxToGenTx tx' = Byron.ByronTx (Byron.byronIdTx tx') tx'

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

-- | Generate a transaction spending genesis UTxO at a given address,
--   to given outputs, signed by the given key.
txSpendGenesisUTxOByronPBFT
  :: Genesis.Config
  -> Network
  -> SigningKey
  -> Address
  -> NonEmpty TxOut
  -> UTxO.ATxAux ByteString
txSpendGenesisUTxOByronPBFT gc nw sk genAddr outs =
    annotateTxAux $ mkTxAux tx (pure wit)
  where
    tx = UnsafeTx (pure txIn) outs txattrs

    wit = signTxId (toByronProtocolMagic nw) sk (Crypto.serializeCborHash tx)

    txIn :: UTxO.TxIn
    txIn  = genesisUTxOTxIn gc (Crypto.toVerification sk) genAddr

    txattrs = Common.mkAttributes ()

-- | Generate a transaction from given Tx inputs to outputs,
--   signed by the given key.
txSpendUTxOByronPBFT
  :: Network
  -> SigningKey
  -> NonEmpty TxIn
  -> NonEmpty TxOut
  -> UTxO.ATxAux ByteString
txSpendUTxOByronPBFT nw sk ins outs =
    annotateTxAux $ mkTxAux tx (Vector.singleton wit)
  where
    tx = UnsafeTx ins outs txattrs

    wit = signTxId (toByronProtocolMagic nw) sk (Crypto.serializeCborHash tx)

    txattrs = Common.mkAttributes ()


-- | Submit a transaction to a node specified by topology info.
nodeSubmitTx
  :: IOManager
  -> Network
  -> GenTx ByronBlock
  -> ExceptT ByronTxError IO ()
nodeSubmitTx iomgr network gentx = do
    socketPath <- firstExceptT EnvSocketError readEnvSocketPath
    liftIO $ do
      traceWith stdoutTracer ("TxId: " ++ condense (txId gentx))
      _res <- submitGenTx
                nullTracer -- stdoutTracer
                iomgr
                (protocolClientInfo ptcl)
                network
                socketPath
                gentx
      --TODO: print failures
      return ()
  where
    ptcl = mkNodeClientProtocolByron
             (EpochSlots 21600)
             (SecurityParam 2160)


--TODO: remove these local definitions when the updated ledger lib is available
fromCborTxAux :: LB.ByteString ->  Either Binary.DecoderError (UTxO.ATxAux B.ByteString)
fromCborTxAux lbs =
    fmap (annotationBytes lbs)
      $ Binary.decodeFullDecoder "Cardano.Chain.UTxO.TxAux.fromCborTxAux"
                                 Binary.fromCBOR lbs
  where
    annotationBytes :: Functor f => LB.ByteString -> f Binary.ByteSpan -> f B.ByteString
    annotationBytes bytes = fmap (LB.toStrict . Binary.slice bytes)

toCborTxAux :: UTxO.ATxAux ByteString -> LB.ByteString
toCborTxAux = LB.fromStrict . UTxO.aTaAnnotation -- The ByteString anotation is the CBOR encoded version.
