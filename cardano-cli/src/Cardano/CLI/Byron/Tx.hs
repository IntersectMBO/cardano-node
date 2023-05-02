{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Byron.Tx
  ( ByronTxError(..)
  , Tx
  , TxFile
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

  , ScriptValidity(..)
  )
where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)
import           Data.Bifunctor (Bifunctor (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Formatting (sformat, (%))

import           Cardano.Api

import qualified Cardano.Binary as Binary
import qualified Cardano.Ledger.Binary.Decoding as LedgerBinary

import qualified Cardano.Chain.Common as Common
import           Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.UTxO as UTxO
import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.Api.Byron
import           Cardano.Api.Pretty

import           Cardano.CLI.Byron.Key (byronWitnessToVerKey)
import           Cardano.CLI.Types (TxFile)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock, GenTx (..))
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

data ByronTxError
  = TxDeserialisationFailed !FilePath !Binary.DecoderError
  | ByronTxSubmitError !Text
  | ByronTxSubmitErrorEraMismatch !EraMismatch
  | EnvSocketError !EnvSocketError
  deriving Show

renderByronTxError :: ByronTxError -> Doc Ann
renderByronTxError err =
  case err of
    ByronTxSubmitError res -> "Error while submitting tx: " <> pretty res
    ByronTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
      "The era of the node and the tx do not match. " <>
      "The node is running in the " <> pretty ledgerEraName <>
      " era, but the transaction is for the " <> pretty otherEraName <> " era."
    TxDeserialisationFailed txFp decErr ->
      "Transaction deserialisation failed at " <> pretty txFp <> " Error: " <> pretty (show decErr)
    EnvSocketError envSockErr -> renderEnvSocketError envSockErr

newtype NewTxFile =
  NewTxFile FilePath
  deriving (Eq, Ord, Show, IsString)


-- | Pretty-print an address in its Base58 form, and also
--   its full structure.
prettyAddress :: Address ByronAddr -> Text
prettyAddress (ByronAddress addr) = sformat
  (Common.addressF % "\n" % Common.addressDetailedF)
  addr addr

readByronTx :: TxFile In -> ExceptT ByronTxError IO (UTxO.ATxAux ByteString)
readByronTx (File fp) = do
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
      <> Text.unpack (prettyAddress (ByronAddress genAddr))
      <> "\n\nIt has the following, though:\n\n"
      <> List.concatMap (Text.unpack . prettyAddress . ByronAddress) (Map.keys initialUtxo)

-- | Generate a transaction spending genesis UTxO at a given address,
--   to given outputs, signed by the given key.
txSpendGenesisUTxOByronPBFT
  :: Genesis.Config
  -> NetworkId
  -> SomeByronSigningKey
  -> Address ByronAddr
  -> [TxOut CtxTx ByronEra]
  -> Tx ByronEra
txSpendGenesisUTxOByronPBFT gc nId sk (ByronAddress bAddr) outs = do
    let txBodyCont =
          TxBodyContent
            [ (fromByronTxIn txIn
              , BuildTxWith (KeyWitness KeyWitnessForSpending))
            ]
            TxInsCollateralNone
            TxInsReferenceNone
            outs
            TxTotalCollateralNone
            TxReturnCollateralNone
            (TxFeeImplicit TxFeesImplicitInByronEra)
            ( TxValidityNoLowerBound
            , TxValidityNoUpperBound ValidityNoUpperBoundInByronEra
            )
            TxMetadataNone
            TxAuxScriptsNone
            TxExtraKeyWitnessesNone
            (BuildTxWith Nothing)
            TxWithdrawalsNone
            TxCertificatesNone
            TxUpdateProposalNone
            TxMintNone
            TxScriptValidityNone
    case createAndValidateTransactionBody txBodyCont of
      Left err -> error $ "Error occurred while creating a Byron genesis based UTxO transaction: " <> show err
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
  -> SomeByronSigningKey
  -> [TxIn]
  -> [TxOut CtxTx ByronEra]
  -> Tx ByronEra
txSpendUTxOByronPBFT nId sk txIns outs = do
  let txBodyCont = TxBodyContent
                     [ ( txIn
                       , BuildTxWith (KeyWitness KeyWitnessForSpending)
                       ) | txIn <- txIns
                     ]
                     TxInsCollateralNone
                     TxInsReferenceNone
                     outs
                     TxTotalCollateralNone
                     TxReturnCollateralNone
                     (TxFeeImplicit TxFeesImplicitInByronEra)
                     ( TxValidityNoLowerBound
                     , TxValidityNoUpperBound ValidityNoUpperBoundInByronEra
                     )
                     TxMetadataNone
                     TxAuxScriptsNone
                     TxExtraKeyWitnessesNone
                     (BuildTxWith Nothing)
                     TxWithdrawalsNone
                     TxCertificatesNone
                     TxUpdateProposalNone
                     TxMintNone
                     TxScriptValidityNone
  case createAndValidateTransactionBody txBodyCont of
    Left err -> error $ "Error occurred while creating a Byron genesis based UTxO transaction: " <> show err
    Right txBody -> let bWit = fromByronWitness sk nId txBody
                    in makeSignedTransaction [bWit] txBody

fromByronWitness :: SomeByronSigningKey -> NetworkId -> TxBody ByronEra -> KeyWitness ByronEra
fromByronWitness bw nId txBody =
  case bw of
    AByronSigningKeyLegacy sk -> makeByronKeyWitness nId txBody sk
    AByronSigningKey sk' -> makeByronKeyWitness nId txBody sk'

-- | Submit a transaction to a node specified by topology info.
nodeSubmitTx
  :: SocketPath
  -> NetworkId
  -> GenTx ByronBlock
  -> ExceptT ByronTxError IO ()
nodeSubmitTx nodeSocketPath network gentx = do
    let connctInfo =
          LocalNodeConnectInfo {
            localNodeSocketPath = unSocketPath nodeSocketPath,
            localNodeNetworkId = network,
            localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
          }
    res <- liftIO $ submitTxToNodeLocal connctInfo (TxInByronSpecial gentx ByronEraInCardanoMode)
    case res of
      Net.Tx.SubmitSuccess -> liftIO $ Text.putStrLn "Transaction successfully submitted."
      Net.Tx.SubmitFail reason ->
        case reason of
          TxValidationErrorInMode err _eraInMode -> left . ByronTxSubmitError . Text.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ ByronTxSubmitErrorEraMismatch mismatchErr

    return ()


--TODO: remove these local definitions when the updated ledger lib is available
fromCborTxAux :: LB.ByteString ->  Either Binary.DecoderError (UTxO.ATxAux B.ByteString)
fromCborTxAux lbs =
    annotationBytes lbs
      <$> Binary.decodeFullDecoder "Cardano.Chain.UTxO.TxAux.fromCborTxAux"
                                 Binary.fromCBOR lbs
  where
    annotationBytes :: Functor f => LB.ByteString -> f LedgerBinary.ByteSpan -> f B.ByteString
    annotationBytes bytes = fmap (LB.toStrict . LedgerBinary.slice bytes)

toCborTxAux :: UTxO.ATxAux ByteString -> LB.ByteString
toCborTxAux = LB.fromStrict . UTxO.aTaAnnotation -- The ByteString anotation is the CBOR encoded version.
