{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( ShelleyTxCmdError
  , renderShelleyTxCmdError
  , runTransactionCmd
  ) where

import           Cardano.Prelude hiding (All, Any)
import           Prelude (String)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                     newExceptT)

--TODO: do this nicely via the API too:
import qualified Cardano.Binary as CBOR

import qualified Shelley.Spec.Ledger.PParams as Shelley

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..),
                     HardForkApplyTxErr (ApplyTxErrByron, ApplyTxErrShelley, ApplyTxErrWrongEra))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardShelley)

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Shelley.Key (InputDecodeError, readSigningKeyFileAnyOf)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Types

import           Cardano.Api.MetaData
import           Cardano.Api.Protocol
import           Cardano.Api.TxSubmit as Api
import           Cardano.Api.Typed as Api

data ShelleyTxCmdError
  = ShelleyTxCmdAesonDecodeProtocolParamsError !FilePath !Text
  | ShelleyTxCmdReadFileError !(FileError ())
  | ShelleyTxCmdReadTextViewFileError !(FileError TextEnvelopeError)
  | ShelleyTxCmdReadWitnessSigningDataError !ReadWitnessSigningDataError
  | ShelleyTxCmdWriteFileError !(FileError ())
  | ShelleyTxCmdMetaDataJsonParseError !FilePath !String
  | ShelleyTxCmdMetaDataConversionError !FilePath !TxMetadataJsonError
  | ShelleyTxCmdMetaValidationError !FilePath !TxMetadataRangeError
  | ShelleyTxCmdMetaDecodeError !FilePath !CBOR.DecoderError
  | ShelleyTxCmdBootstrapWitnessError !ShelleyBootstrapWitnessError
  | ShelleyTxCmdSocketEnvError !EnvSocketError
  | ShelleyTxCmdTxSubmitErrorByron !(ApplyTxErr ByronBlock)
  | ShelleyTxCmdTxSubmitErrorShelley !(ApplyTxErr (ShelleyBlock StandardShelley))
  | ShelleyTxCmdTxSubmitErrorEraMismatch !EraMismatch
  deriving Show

renderShelleyTxCmdError :: ShelleyTxCmdError -> Text
renderShelleyTxCmdError err =
  case err of
    ShelleyTxCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadTextViewFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadWitnessSigningDataError witSignDataErr ->
      renderReadWitnessSigningDataError witSignDataErr
    ShelleyTxCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdMetaDataJsonParseError fp jsonErr ->
       "Invalid JSON format in file: " <> show fp
                <> "\nJSON parse error: " <> Text.pack jsonErr
    ShelleyTxCmdMetaDataConversionError fp metaDataErr ->
       "Error reading metadata at: " <> show fp
                             <> "\n" <> Text.pack (displayError metaDataErr)
    ShelleyTxCmdMetaDecodeError fp metaDataErr ->
       "Error decoding CBOR metadata at: " <> show fp
                             <> " Error: " <> show metaDataErr
    ShelleyTxCmdMetaValidationError fp valErr ->
      "Error validating transaction metadata at: " <> show fp
                                           <> "\n" <> Text.pack (displayError valErr)
    ShelleyTxCmdSocketEnvError envSockErr -> renderEnvSocketError envSockErr
    ShelleyTxCmdAesonDecodeProtocolParamsError fp decErr ->
      "Error while decoding the protocol parameters at: " <> show fp
                                            <> " Error: " <> show decErr
    ShelleyTxCmdTxSubmitErrorByron res ->
      "Error while submitting tx: " <> Text.pack (show res)
    ShelleyTxCmdTxSubmitErrorShelley res ->
      "Error while submitting tx: " <> Text.pack (show res)
    ShelleyTxCmdTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
      "The era of the node and the tx do not match. " <>
      "The node is running in the " <> ledgerEraName <>
      " era, but the transaction is for the " <> otherEraName <> " era."
    ShelleyTxCmdBootstrapWitnessError sbwErr ->
      renderShelleyBootstrapWitnessError sbwErr

runTransactionCmd :: TransactionCmd -> ExceptT ShelleyTxCmdError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee certs wdrls
               metadataSchema metadataFiles mUpProp out ->
      runTxBuildRaw txins txouts ttl fee certs wdrls
                    metadataSchema metadataFiles mUpProp out
    TxSign txinfile skfiles network txoutfile ->
      runTxSign txinfile skfiles network txoutfile
    TxSubmit protocol network txFp ->
      runTxSubmit protocol network txFp
    TxCalculateMinFee txbody mnw pParamsFile nInputs nOutputs
                      nShelleyKeyWitnesses nByronKeyWitnesses ->
      runTxCalculateMinFee txbody mnw pParamsFile nInputs nOutputs
                           nShelleyKeyWitnesses nByronKeyWitnesses
    TxGetTxId txinfile ->
      runTxGetTxId txinfile
    TxCreateWitness txBodyfile witSignData mbNw outFile ->
      runTxCreateWitness txBodyfile witSignData mbNw outFile
    TxAssembleTxBodyWitness txBodyFile witnessFile outFile ->
      runTxSignWitness txBodyFile witnessFile outFile

runTxBuildRaw
  :: [Api.TxIn]
  -> [Api.TxOut Api.Shelley]
  -> SlotNo
  -> Api.Lovelace
  -> [CertificateFile]
  -> [(Api.StakeAddress, Api.Lovelace)]
  -> TxMetadataJsonSchema
  -> [MetaDataFile]
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildRaw txins txouts ttl fee
              certFiles withdrawals
              metadataSchema metadataFiles
              mUpdatePropFile
              (TxBodyFile fpath) = do

    certs <- sequence
               [ firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                   Api.readFileTextEnvelope Api.AsCertificate certFile
               | CertificateFile certFile <- certFiles ]


    mMetaData <- case metadataFiles of
      []    -> return Nothing
      files -> Just . mconcat <$> mapM (readFileTxMetaData metadataSchema) files
               -- read all the files and merge their metadata maps
               -- in case of clashes earlier entries take precedence

    mUpdateProp <-
      case mUpdatePropFile of
        Nothing                        -> return Nothing
        Just (UpdateProposalFile file) ->
          fmap Just <$> firstExceptT ShelleyTxCmdReadTextViewFileError $ newExceptT $
            Api.readFileTextEnvelope Api.AsUpdateProposal file

    let txBody = Api.makeShelleyTransaction
                   Api.txExtraContentEmpty {
                     Api.txCertificates   = certs,
                     Api.txWithdrawals    = withdrawals,
                     Api.txMetadata       = mMetaData,
                     Api.txUpdateProposal = mUpdateProp
                   }
                   ttl
                   fee
                   txins
                   txouts

    firstExceptT ShelleyTxCmdWriteFileError
      . newExceptT
      $ Api.writeFileTextEnvelope fpath Nothing txBody


runTxSign :: TxBodyFile
          -> [WitnessSigningData]
          -> Maybe Api.NetworkId
          -> TxFile
          -> ExceptT ShelleyTxCmdError IO ()
runTxSign (TxBodyFile txbodyFile) witSigningData mnw (TxFile txFile) = do
  txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
              Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
  sks    <- firstExceptT ShelleyTxCmdReadWitnessSigningDataError $
              mapM readWitnessSigningData witSigningData

  let (sksByron, sksShelley, scsShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  -- Byron witnesses require the network ID. This can either be provided
  -- directly or derived from a provided Byron address.
  byronWitnesses <- firstExceptT ShelleyTxCmdBootstrapWitnessError
    . hoistEither
    $ mkShelleyBootstrapWitnesses mnw txbody sksByron

  let shelleyKeyWitnesses = map (Api.makeShelleyKeyWitness txbody) sksShelley
      shelleyScriptWitnesses = map (makeShelleyScriptWitness . makeMultiSigScript) scsShelley
      shelleyWitnesses = shelleyKeyWitnesses ++ shelleyScriptWitnesses
      tx = Api.makeSignedTransaction (byronWitnesses ++ shelleyWitnesses) txbody

  firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
    Api.writeFileTextEnvelope txFile Nothing tx

runTxSubmit :: Protocol -> NetworkId -> FilePath
            -> ExceptT ShelleyTxCmdError IO ()
runTxSubmit protocol network txFile = do
    SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError readEnvSocketPath
    tx <- firstExceptT ShelleyTxCmdReadTextViewFileError
      . newExceptT
      $ Api.readFileTextEnvelopeAnyOf
          [ Api.FromSomeType Api.AsByronTx   Left
          , Api.FromSomeType Api.AsShelleyTx Right ]
          txFile

    withlocalNodeConnectInfo protocol network sockPath $ \connectInfo ->
      case (localNodeConsensusMode connectInfo, tx) of
        (ByronMode{}, Left tx') -> do
          result <- liftIO $ Api.submitTx connectInfo (TxForByronMode tx')
          case result of
            TxSubmitSuccess -> return ()
            TxSubmitFailureByronMode err ->
              left (ShelleyTxCmdTxSubmitErrorByron err)

        (ByronMode{}, Right{}) ->
          left $ ShelleyTxCmdTxSubmitErrorEraMismatch EraMismatch {
                   ledgerEraName = "Byron",
                   otherEraName  = "Shelley"
                 }

        (ShelleyMode{}, Right tx') -> do
          result <- liftIO $ Api.submitTx connectInfo (TxForShelleyMode tx')
          case result of
            TxSubmitSuccess -> return ()
            TxSubmitFailureShelleyMode err ->
              left (ShelleyTxCmdTxSubmitErrorShelley err)

        (ShelleyMode{}, Left{}) ->
          left $ ShelleyTxCmdTxSubmitErrorEraMismatch EraMismatch {
                   ledgerEraName = "Shelley",
                   otherEraName  = "Byron"
                 }

        (CardanoMode{}, tx') -> do
          result <- liftIO $ Api.submitTx connectInfo (TxForCardanoMode tx')
          case result of
            TxSubmitSuccess -> return ()
            TxSubmitFailureCardanoMode (ApplyTxErrByron err) ->
              left (ShelleyTxCmdTxSubmitErrorByron err)
            TxSubmitFailureCardanoMode (ApplyTxErrShelley err) ->
              left (ShelleyTxCmdTxSubmitErrorShelley err)
            TxSubmitFailureCardanoMode (ApplyTxErrWrongEra mismatch) ->
              left (ShelleyTxCmdTxSubmitErrorEraMismatch mismatch)


runTxCalculateMinFee
  :: TxBodyFile
  -> Maybe Api.NetworkId
  -> ProtocolParamsFile
  -> TxInCount
  -> TxOutCount
  -> TxShelleyWitnessCount
  -> TxByronWitnessCount
  -> ExceptT ShelleyTxCmdError IO ()
runTxCalculateMinFee (TxBodyFile txbodyFile) nw pParamsFile
                     (TxInCount nInputs) (TxOutCount nOutputs)
                     (TxShelleyWitnessCount nShelleyKeyWitnesses)
                     (TxByronWitnessCount nByronKeyWitnesses) = do

    txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile

    pparams <- readProtocolParameters pParamsFile

    let tx = Api.makeSignedTransaction [] txbody
        Api.Lovelace fee = Api.estimateTransactionFee
                             (fromMaybe Api.Mainnet nw)
                             (Shelley._minfeeB pparams) --TODO: do this better
                             (Shelley._minfeeA pparams)
                             tx
                             nInputs nOutputs
                             nByronKeyWitnesses nShelleyKeyWitnesses

    liftIO $ putStrLn $ (show fee :: String) <> " Lovelace"

--TODO: eliminate this and get only the necessary params, and get them in a more
-- helpful way rather than requiring them as a local file.
readProtocolParameters :: ProtocolParamsFile
                       -> ExceptT ShelleyTxCmdError IO (Shelley.PParams StandardShelley)
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (ShelleyTxCmdAesonDecodeProtocolParamsError fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams

data SomeWitness
  = AByronSigningKey           (Api.SigningKey Api.ByronKey) (Maybe (Address Byron))
  | APaymentSigningKey         (Api.SigningKey Api.PaymentKey)
  | APaymentExtendedSigningKey (Api.SigningKey Api.PaymentExtendedKey)
  | AStakeSigningKey           (Api.SigningKey Api.StakeKey)
  | AStakeExtendedSigningKey   (Api.SigningKey Api.StakeExtendedKey)
  | AStakePoolSigningKey       (Api.SigningKey Api.StakePoolKey)
  | AGenesisSigningKey         (Api.SigningKey Api.GenesisKey)
  | AGenesisExtendedSigningKey (Api.SigningKey Api.GenesisExtendedKey)
  | AGenesisDelegateSigningKey (Api.SigningKey Api.GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningKey
                               (Api.SigningKey Api.GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningKey     (Api.SigningKey Api.GenesisUTxOKey)
  | AShelleyMultiSigScript     Api.MultiSigScript

-- | Error deserialising a JSON-encoded script.
newtype ScriptJsonDecodeError = ScriptJsonDecodeError String
  deriving Show

instance Error ScriptJsonDecodeError where
  displayError (ScriptJsonDecodeError errStr) = errStr

-- | Error reading the data required to construct a key witness.
data ReadWitnessSigningDataError
  = ReadWitnessSigningDataSigningKeyDecodeError !(FileError InputDecodeError)
  | ReadWitnessSigningDataScriptError !(FileError ScriptJsonDecodeError)
  | ReadWitnessSigningDataSigningKeyAndAddressMismatch
  -- ^ A Byron address was specified alongside a non-Byron signing key.
  deriving Show

-- | Render an error message for a 'ReadWitnessSigningDataError'.
renderReadWitnessSigningDataError :: ReadWitnessSigningDataError -> Text
renderReadWitnessSigningDataError err =
  case err of
    ReadWitnessSigningDataSigningKeyDecodeError fileErr ->
      "Error reading signing key: " <> Text.pack (displayError fileErr)
    ReadWitnessSigningDataScriptError fileErr ->
      "Error reading script: " <> Text.pack (displayError fileErr)
    ReadWitnessSigningDataSigningKeyAndAddressMismatch ->
      "Only a Byron signing key may be accompanied by a Byron address."

readWitnessSigningData
  :: WitnessSigningData
  -> ExceptT ReadWitnessSigningDataError IO SomeWitness
readWitnessSigningData (ScriptWitnessSigningData (ScriptFile fp)) = do
  msJson <- handleIOExceptT (ReadWitnessSigningDataScriptError . FileIOError fp)
    $ LBS.readFile fp

  hoistEither $ bimap
    (ReadWitnessSigningDataScriptError . FileError fp . ScriptJsonDecodeError)
    AShelleyMultiSigScript
    (Aeson.eitherDecode' msJson)

readWitnessSigningData (KeyWitnessSigningData skFile mbByronAddr) = do
    res <- firstExceptT ReadWitnessSigningDataSigningKeyDecodeError
      . newExceptT
      $ readSigningKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
    case (res, mbByronAddr) of
      (AByronSigningKey _ _, Just _) -> pure res
      (AByronSigningKey _ _, Nothing) -> pure res
      (_, Nothing) -> pure res
      (_, Just _) ->
        -- A Byron address should only be specified along with a Byron signing key.
        left ReadWitnessSigningDataSigningKeyAndAddressMismatch
  where
    textEnvFileTypes =
      [ Api.FromSomeType (Api.AsSigningKey Api.AsByronKey)
                          (`AByronSigningKey` mbByronAddr)
      , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey)
                          APaymentSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeKey)
                          AStakeSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakePoolKey)
                          AStakePoolSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisKey)
                          AGenesisSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisExtendedKey)
                          AGenesisExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisDelegateKey)
                          AGenesisDelegateSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisDelegateExtendedKey)
                          AGenesisDelegateExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisUTxOKey)
                          AGenesisUTxOSigningKey
      ]

    bech32FileTypes =
      [ Api.FromSomeType (Api.AsSigningKey Api.AsByronKey)
                          (`AByronSigningKey` mbByronAddr)
      , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey)
                          APaymentSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeKey)
                          AStakeSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakePoolKey)
                          AStakePoolSigningKey
      ]

partitionSomeWitnesses
  :: [ByronOrShelleyWitness]
  -> ( [ShelleyBootstrapWitnessSigningKeyData]
     , [Api.ShelleyWitnessSigningKey]
     , [Api.MultiSigScript]
     )
partitionSomeWitnesses = reversePartitionedWits . foldl' go mempty
  where
    reversePartitionedWits (bw, skw, ssw) =
      (reverse bw, reverse skw, reverse ssw)

    go (byronAcc, shelleyKeyAcc, shelleyScriptAcc) byronOrShelleyWit =
      case byronOrShelleyWit of
        AByronWitness byronWit ->
          (byronWit:byronAcc, shelleyKeyAcc, shelleyScriptAcc)
        AShelleyKeyWitness shelleyKeyWit ->
          (byronAcc, shelleyKeyWit:shelleyKeyAcc, shelleyScriptAcc)
        AShelleyScriptWitness shelleyScriptWit ->
          (byronAcc, shelleyKeyAcc, shelleyScriptWit:shelleyScriptAcc)


-- | Some kind of Byron or Shelley witness.
data ByronOrShelleyWitness
  = AByronWitness !ShelleyBootstrapWitnessSigningKeyData
  | AShelleyKeyWitness !Api.ShelleyWitnessSigningKey
  | AShelleyScriptWitness !Api.MultiSigScript

categoriseSomeWitness :: SomeWitness -> ByronOrShelleyWitness
categoriseSomeWitness swsk =
  case swsk of
    AByronSigningKey           sk addr -> AByronWitness (ShelleyBootstrapWitnessSigningKeyData sk addr)
    APaymentSigningKey         sk      -> AShelleyKeyWitness (Api.WitnessPaymentKey         sk)
    APaymentExtendedSigningKey sk      -> AShelleyKeyWitness (Api.WitnessPaymentExtendedKey sk)
    AStakeSigningKey           sk      -> AShelleyKeyWitness (Api.WitnessStakeKey           sk)
    AStakeExtendedSigningKey   sk      -> AShelleyKeyWitness (Api.WitnessStakeExtendedKey   sk)
    AStakePoolSigningKey       sk      -> AShelleyKeyWitness (Api.WitnessStakePoolKey       sk)
    AGenesisSigningKey         sk      -> AShelleyKeyWitness (Api.WitnessGenesisKey sk)
    AGenesisExtendedSigningKey sk      -> AShelleyKeyWitness (Api.WitnessGenesisExtendedKey sk)
    AGenesisDelegateSigningKey sk      -> AShelleyKeyWitness (Api.WitnessGenesisDelegateKey sk)
    AGenesisDelegateExtendedSigningKey sk
                                       -> AShelleyKeyWitness (Api.WitnessGenesisDelegateExtendedKey sk)
    AGenesisUTxOSigningKey     sk      -> AShelleyKeyWitness (Api.WitnessGenesisUTxOKey     sk)
    AShelleyMultiSigScript scr         -> AShelleyScriptWitness scr

-- | Data required for constructing a Shelley bootstrap witness.
data ShelleyBootstrapWitnessSigningKeyData
  = ShelleyBootstrapWitnessSigningKeyData
      !(SigningKey ByronKey)
      -- ^ Byron signing key.
      !(Maybe (Address Byron))
      -- ^ An optionally specified Byron address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Byron witness.

-- | Error constructing a Shelley bootstrap witness (i.e. a Byron key witness
-- in the Shelley era).
data ShelleyBootstrapWitnessError
  = MissingNetworkIdOrByronAddressError
  -- ^ Neither a network ID nor a Byron address were provided to construct the
  -- Shelley bootstrap witness. One or the other is required.
  deriving Show

-- | Render an error message for a 'ShelleyBootstrapWitnessError'.
renderShelleyBootstrapWitnessError :: ShelleyBootstrapWitnessError -> Text
renderShelleyBootstrapWitnessError MissingNetworkIdOrByronAddressError =
  "Transactions witnessed by a Byron signing key must be accompanied by a "
    <> "network ID. Either provide a network ID or provide a Byron "
    <> "address with each Byron signing key (network IDs can be derived "
    <> "from Byron addresses)."

-- | Construct a Shelley bootstrap witness (i.e. a Byron key witness in the
-- Shelley era).
mkShelleyBootstrapWitness
  :: Maybe NetworkId
  -> TxBody Shelley
  -> ShelleyBootstrapWitnessSigningKeyData
  -> Either ShelleyBootstrapWitnessError (Witness Shelley)
mkShelleyBootstrapWitness Nothing _ (ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrByronAddressError
mkShelleyBootstrapWitness (Just nw) txBody (ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ makeShelleyBootstrapWitness (WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ makeShelleyBootstrapWitness (WitnessByronAddress addr) txBody skey

-- | Attempt to construct Shelley bootstrap witnesses until an error is
-- encountered.
mkShelleyBootstrapWitnesses
  :: Maybe NetworkId
  -> TxBody Shelley
  -> [ShelleyBootstrapWitnessSigningKeyData]
  -> Either ShelleyBootstrapWitnessError [Witness Shelley]
mkShelleyBootstrapWitnesses mnw txBody =
  mapM (mkShelleyBootstrapWitness mnw txBody)


runTxGetTxId :: TxBodyFile -> ExceptT ShelleyTxCmdError IO ()
runTxGetTxId (TxBodyFile txbodyFile) = do
  txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
              Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
  liftIO $ BS.putStrLn $ Api.serialiseToRawBytesHex (Api.getTxId txbody)

runTxCreateWitness
  :: TxBodyFile
  -> WitnessSigningData
  -> Maybe NetworkId
  -> OutputFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxCreateWitness (TxBodyFile txbodyFile) witSignData mbNw (OutputFile oFile) = do
  txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError
    . newExceptT
    $ Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
  someWit <- firstExceptT ShelleyTxCmdReadWitnessSigningDataError
    $ readWitnessSigningData witSignData

  witness <-
    case categoriseSomeWitness someWit of
      -- Byron witnesses require the network ID. This can either be provided
      -- directly or derived from a provided Byron address.
      AByronWitness bootstrapWitData ->
        firstExceptT ShelleyTxCmdBootstrapWitnessError
          . hoistEither
          $ mkShelleyBootstrapWitness mbNw txbody bootstrapWitData
      AShelleyKeyWitness skShelley ->
        pure $ makeShelleyKeyWitness txbody skShelley
      AShelleyScriptWitness scShelley ->
        pure $ makeShelleyScriptWitness (makeMultiSigScript scShelley)

  firstExceptT ShelleyTxCmdWriteFileError
    . newExceptT
    $ Api.writeFileTextEnvelope oFile Nothing witness

runTxSignWitness
  :: TxBodyFile
  -> [WitnessFile]
  -> OutputFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxSignWitness (TxBodyFile txBodyFile) witnessFiles (OutputFile oFp) = do
    txBody <- firstExceptT ShelleyTxCmdReadTextViewFileError
      . newExceptT
      $ Api.readFileTextEnvelope Api.AsShelleyTxBody txBodyFile
    witnesses <- mapM readWitnessFile witnessFiles
    let tx = Api.makeSignedTransaction witnesses txBody
    firstExceptT ShelleyTxCmdWriteFileError
      . newExceptT
      $ Api.writeFileTextEnvelope oFp Nothing tx

readWitnessFile :: WitnessFile -> ExceptT ShelleyTxCmdError IO (Witness Shelley)
readWitnessFile (WitnessFile fp) =
  firstExceptT ShelleyTxCmdReadTextViewFileError $ newExceptT (Api.readFileTextEnvelope AsShelleyWitness fp)

-- ----------------------------------------------------------------------------
-- Transaction metadata
--

readFileTxMetaData :: TxMetadataJsonSchema -> MetaDataFile
                   -> ExceptT ShelleyTxCmdError IO Api.TxMetadata
readFileTxMetaData mapping (MetaDataFileJSON fp) = do
    bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $
          LBS.readFile fp
    v  <- firstExceptT (ShelleyTxCmdMetaDataJsonParseError fp) $
          hoistEither $
            Aeson.eitherDecode' bs
    firstExceptT (ShelleyTxCmdMetaDataConversionError fp) $ hoistEither $
      metadataFromJson mapping v
readFileTxMetaData _ (MetaDataFileCBOR fp) = do
    bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $
          BS.readFile fp
    txMetadata <- firstExceptT (ShelleyTxCmdMetaDecodeError fp) $ hoistEither $
      Api.deserialiseFromCBOR Api.AsTxMetadata bs
    firstExceptT (ShelleyTxCmdMetaValidationError fp . NE.head) $ hoistEither $
      validateTxMetadata txMetadata
