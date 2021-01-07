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

import           Cardano.Prelude hiding (option, trace, (%))
import           Prelude (error)

import           Control.Monad.Trans.Except.Extra (firstExceptT, left)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Formatting (sformat, (%))

import qualified Cardano.Binary as Binary

import qualified Cardano.Chain.Common as Common
import           Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochSlots (..))
import qualified Cardano.Chain.UTxO as UTxO
import qualified Cardano.Crypto.Signing as Crypto

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock, GenTx (..))
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate (GenTx (DegenGenTx))

import           Cardano.Api (LocalNodeConnectInfo (..), NetworkId, TxBody, Witness,
                     makeByronTransaction, submitTxToNodeLocal)
import           Cardano.Api.Byron (Address (..), ByronAddr, ByronEra, ByronWitness (..),
                     NodeConsensusMode (ByronMode), Tx (..), TxIn, TxOut (..),
                     VerificationKey (..), fromByronTxIn, makeByronKeyWitness,
                     makeSignedTransaction)
import           Cardano.CLI.Byron.Key (byronWitnessToVerKey)
import           Cardano.CLI.Environment
import           Cardano.CLI.Helpers (textShow)
import           Cardano.CLI.Types (SocketPath (..))

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
prettyAddress :: Address ByronAddr -> Text
prettyAddress (ByronAddress addr) = sformat
  (Common.addressF %"\n"%Common.addressDetailedF)
  addr addr

readByronTx :: TxFile -> ExceptT ByronTxError IO (UTxO.ATxAux ByteString)
readByronTx (TxFile fp) = do
  txBS <- liftIO $ LB.readFile fp
  case fromCborTxAux txBS of
    Left e -> left $ TxDeserialisationFailed fp e
    Right tx -> pure tx

-- | The 'GenTx' is all the kinds of transactions that can be submitted
-- and \"normal\" Byron transactions are just one of the kinds.
normalByronTxToGenTx :: UTxO.ATxAux ByteString -> GenTx ByronBlock
normalByronTxToGenTx tx' = Byron.ByronTx (Byron.byronIdTx tx') tx'

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
                -> Common.Address
                -> UTxO.TxOut
                -> (Common.Address, (UTxO.TxIn, UTxO.TxOut))
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
      <> T.unpack (prettyAddress (ByronAddress genAddr))
      <> "\n\nIt has the following, though:\n\n"
      <> Cardano.Prelude.concat (T.unpack . prettyAddress <$> map ByronAddress (Map.keys initialUtxo))

-- | Generate a transaction spending genesis UTxO at a given address,
--   to given outputs, signed by the given key.
txSpendGenesisUTxOByronPBFT
  :: Genesis.Config
  -> NetworkId
  -> ByronWitness
  -> Address ByronAddr
  -> [TxOut ByronEra]
  -> Tx ByronEra
txSpendGenesisUTxOByronPBFT gc nId sk (ByronAddress bAddr) outs =
    case makeByronTransaction [fromByronTxIn txIn] outs of
      Left err -> error $ "Error occured while creating a Byron genesis based UTxO transaction: " <> show err
      Right txBody -> let bWit = fromByronWitness sk nId txBody
                      in makeSignedTransaction [bWit] txBody
  where
    ByronVerificationKey vKey = byronWitnessToVerKey sk

    txIn :: UTxO.TxIn
    txIn  = genesisUTxOTxIn gc vKey bAddr

-- | Generate a transaction from given Tx inputs to outputs,
--   signed by the given key.
txSpendUTxOByronPBFT
  :: NetworkId
  -> ByronWitness
  -> [TxIn]
  -> [TxOut ByronEra]
  -> Tx ByronEra
txSpendUTxOByronPBFT nId sk txIn outs =
    case makeByronTransaction txIn outs of
      Left err -> error $ "An error occurred while making a Byron era transaction: " <> show err
      Right txBody -> let bWit = fromByronWitness sk nId txBody
                      in makeSignedTransaction [bWit] txBody

fromByronWitness :: ByronWitness -> NetworkId -> TxBody ByronEra -> Witness ByronEra
fromByronWitness bw nId txBody =
  case bw of
    LegacyWitness sk -> makeByronKeyWitness nId txBody sk
    NonLegacyWitness sk' -> makeByronKeyWitness nId txBody sk'

-- | Submit a transaction to a node specified by topology info.
nodeSubmitTx
  :: NetworkId
  -> GenTx ByronBlock
  -> ExceptT ByronTxError IO ()
nodeSubmitTx network gentx = do
    SocketPath socketPath <- firstExceptT EnvSocketError readEnvSocketPath
    let connctInfo =
          LocalNodeConnectInfo {
            localNodeSocketPath    = socketPath,
            localNodeNetworkId     = network,
            localNodeConsensusMode = ByronMode (EpochSlots 21600)
          }
    _res <- liftIO $ submitTxToNodeLocal connctInfo (DegenGenTx gentx)
    --TODO: print failures
    return ()


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
