{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( ShelleyTxCmdError
  , renderShelleyTxCmdError
  , runTransactionCmd
  ) where

import           Cardano.Prelude
import           Prelude (String)

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Scientific as Scientific
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

import qualified Cardano.Api as OldApi
import           Cardano.Api.Protocol (ProtocolData (..))
import qualified Cardano.Api.Typed as Api
import           Cardano.Api.Typed (SlotNo)

--TODO: do this nicely via the API too:
import qualified Cardano.Binary as CBOR

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath,
                   renderEnvSocketError)

import           Cardano.Config.Types

import           Cardano.CLI.Shelley.Parsers
import           Cardano.Config.Types (CertificateFile (..))

import qualified Shelley.Spec.Ledger.PParams as Shelley

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, left, newExceptT, hoistEither, handleIOExceptT)


data ShelleyTxCmdError
  = ShelleyTxAesonDecodeProtocolParamsError !FilePath !Text
  | ShelleyTxMetaDataFileError !FilePath !IOException
  | ShelleyTxMetaDataConversionError !FilePath !MetaDataJsonConversionError
  | ShelleyTxMetaDecodeError !FilePath !CBOR.DecoderError
  | ShelleyTxMissingNetworkId
  | ShelleyTxSocketEnvError !EnvSocketError
  | ShelleyTxReadProtocolParamsError !FilePath !IOException
  | ShelleyTxReadSignedTxError !OldApi.ApiError
  | ShelleyTxReadUpdateError !(Api.FileError Api.TextEnvelopeError)
  | ShelleyTxReadUnsignedTxError !(Api.FileError Api.TextEnvelopeError)
  | ShelleyTxCertReadError !(Api.FileError Api.TextEnvelopeError)
  | ShelleyTxWriteSignedTxError !(Api.FileError ())
  | ShelleyTxWriteUnsignedTxError !(Api.FileError ())
  | ShelleyTxSubmitError !OldApi.TxSubmitResult
  | ShelleyTxReadFileError !(Api.FileError Api.TextEnvelopeError)
  deriving Show

renderShelleyTxCmdError :: ShelleyTxCmdError -> Text
renderShelleyTxCmdError err =
  case err of
    ShelleyTxReadProtocolParamsError fp ioException ->
      "Error while reading protocol parameters at: " <> OldApi.textShow fp <> " Error: " <> OldApi.textShow ioException
    ShelleyTxMetaDataFileError fp ioException ->
       "Error reading metadata at: " <> OldApi.textShow fp <> " Error: " <> OldApi.textShow ioException
    ShelleyTxMetaDataConversionError fp metaDataErr ->
       "Error reading metadata at: " <> OldApi.textShow fp
                       <> " Error: " <> renderMetaDataJsonConversionError metaDataErr
    ShelleyTxMetaDecodeError fp metaDataErr ->
       "Error decoding CBOR metadata at: " <> OldApi.textShow fp
                             <> " Error: " <> OldApi.textShow metaDataErr
    ShelleyTxReadUnsignedTxError err' ->
      "Error while reading unsigned shelley tx: " <> Text.pack (Api.displayError err')
    ShelleyTxReadSignedTxError apiError ->
      "Error while reading signed shelley tx: " <> OldApi.renderApiError apiError
    ShelleyTxReadUpdateError apiError ->
      "Error while reading shelley update proposal: " <> Text.pack (Api.displayError apiError)
    ShelleyTxSocketEnvError envSockErr -> renderEnvSocketError envSockErr
    ShelleyTxAesonDecodeProtocolParamsError fp decErr ->
      "Error while decoding the protocol parameters at: " <> OldApi.textShow fp <> " Error: " <> OldApi.textShow decErr
    ShelleyTxCertReadError err' ->
      "Error reading shelley certificate at: " <> Text.pack (Api.displayError err')
    ShelleyTxWriteSignedTxError err' ->
      "Error while writing signed shelley tx: " <> Text.pack (Api.displayError err')
    ShelleyTxWriteUnsignedTxError err' ->
      "Error while writing unsigned shelley tx: " <> Text.pack (Api.displayError err')
    ShelleyTxSubmitError res ->
      "Error while submitting tx: " <> OldApi.renderTxSubmitResult res
    ShelleyTxReadFileError fileErr -> Text.pack (Api.displayError fileErr)
    ShelleyTxMissingNetworkId -> "Please enter network id with your byron transaction"

runTransactionCmd :: TransactionCmd -> ExceptT ShelleyTxCmdError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee certs wdrls mMetaData mUpProp out ->
      runTxBuildRaw txins txouts ttl fee certs wdrls mMetaData mUpProp out
    TxSign txinfile skfiles network txoutfile ->
      runTxSign txinfile skfiles network txoutfile
    TxSubmit txFp network protocolData ->
      runTxSubmit txFp network protocolData
    TxCalculateMinFee txbody mnw pParamsFile nInputs nOutputs
                      nShelleyKeyWitnesses nByronKeyWitnesses ->
      runTxCalculateMinFee txbody mnw pParamsFile nInputs nOutputs
                           nShelleyKeyWitnesses nByronKeyWitnesses
    TxGetTxId txinfile ->
      runTxGetTxId txinfile

    _ -> liftIO $ putStrLn $ "runTransactionCmd: " ++ show cmd

runTxBuildRaw
  :: [Api.TxIn]
  -> [Api.TxOut Api.Shelley]
  -> SlotNo
  -> Api.Lovelace
  -> [CertificateFile]
  -> [(Api.StakeAddress, Api.Lovelace)]
  -> [MetaDataFile]
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildRaw txins txouts ttl fee
              certFiles withdrawals metaDataFiles mUpdatePropFile
              (TxBodyFile fpath) = do

    certs <- sequence
               [ firstExceptT ShelleyTxCertReadError . newExceptT $
                   Api.readFileTextEnvelope Api.AsCertificate certFile
               | CertificateFile certFile <- certFiles ]


    mMetaData <- case metaDataFiles of
      []    -> return Nothing
      files -> Just . mconcat <$> mapM readFileTxMetaData files
               -- read all the files and merge their metadata maps
               -- in case of clashes earlier entries take precedence

    mUpdateProp <-
      case mUpdatePropFile of
        Nothing                        -> return Nothing
        Just (UpdateProposalFile file) ->
          fmap Just <$> firstExceptT ShelleyTxReadUpdateError $ newExceptT $
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

    firstExceptT ShelleyTxWriteUnsignedTxError
      . newExceptT
      $ Api.writeFileTextEnvelope fpath Nothing txBody


runTxSign :: TxBodyFile
          -> [SigningKeyFile]
          -> Maybe Api.NetworkId
          -> TxFile
          -> ExceptT ShelleyTxCmdError IO ()
runTxSign (TxBodyFile txbodyFile) skFiles mnw (TxFile txFile) = do
    txbody <- firstExceptT ShelleyTxReadUnsignedTxError . newExceptT $
                Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
    sks    <- firstExceptT ShelleyTxReadFileError $
                mapM readSigningKeyFile skFiles

    -- We have to handle Byron and Shelley key witnesses slightly differently
    let (sksByron, sksShelley) = partitionEithers (map categoriseSigningKey sks)

    -- Byron witnesses need the network id
    witnessesByron <-
      case (sksByron, mnw) of
        ([], Nothing) -> return []
        (_,  Nothing) -> throwError ShelleyTxMissingNetworkId
        (_,  Just nw) ->
          return $ map (Api.makeShelleyBootstrapWitness nw txbody) sksByron

    let witnesses :: [Api.Witness Api.Shelley]
        witnesses = witnessesByron
                 ++ map (Api.makeShelleyKeyWitness txbody) sksShelley

        tx        :: Api.Tx Api.Shelley
        tx        = Api.makeSignedTransaction witnesses txbody

    firstExceptT ShelleyTxWriteSignedTxError . newExceptT $
      Api.writeFileTextEnvelope txFile Nothing tx
  where
    categoriseSigningKey :: SomeWitnessSigningKey
                         -> Either (Api.SigningKey Api.ByronKey)
                                    Api.ShelleyWitnessSigningKey
    categoriseSigningKey swsk =
      case swsk of
        AByronSigningKey           sk -> Left sk
        APaymentSigningKey         sk -> Right (Api.WitnessPaymentKey         sk)
        AStakeSigningKey           sk -> Right (Api.WitnessStakeKey           sk)
        AStakePoolSigningKey       sk -> Right (Api.WitnessStakePoolKey       sk)
        AGenesisDelegateSigningKey sk -> Right (Api.WitnessGenesisDelegateKey sk)
        AGenesisUTxOSigningKey     sk -> Right (Api.WitnessGenesisUTxOKey     sk)

runTxSubmit :: FilePath -> OldApi.Network -> ProtocolData -> ExceptT ShelleyTxCmdError IO ()
runTxSubmit txFp network protocolData = do
    sktFp <- firstExceptT ShelleyTxSocketEnvError $ readEnvSocketPath
    signedTx <- firstExceptT ShelleyTxReadFileError
      . newExceptT
      $ Api.readFileTextEnvelope Api.AsShelleyTx txFp
    result <- liftIO $ OldApi.submitTx network protocolData sktFp (toOldApiSignedTx signedTx)
    case result of
      OldApi.TxSubmitSuccess                          -> return ()
      OldApi.TxSubmitFailureShelley _                 -> left (ShelleyTxSubmitError result)
      OldApi.TxSubmitFailureByron   _                 -> left (ShelleyTxSubmitError result)
      OldApi.TxSubmitFailureCardano _                 -> left (ShelleyTxSubmitError result)
      OldApi.TxSubmitFailureProtocolAndTxMismatch _ _ -> left (ShelleyTxSubmitError result)
  where
    toOldApiSignedTx :: Api.Tx Api.Shelley -> OldApi.TxSigned
    toOldApiSignedTx (Api.ShelleyTx tx) = OldApi.TxSignedShelley tx


runTxCalculateMinFee
  :: TxBodyFile
  -> Maybe Api.NetworkId
  -> ProtocolParamsFile
  -> TxInCount
  -> TxOutCount
  -> TxShelleyWinessCount
  -> TxByronWinessCount
  -> ExceptT ShelleyTxCmdError IO ()
runTxCalculateMinFee (TxBodyFile txbodyFile) nw pParamsFile
                     (TxInCount nInputs) (TxOutCount nOutputs)
                     (TxShelleyWinessCount nShelleyKeyWitnesses)
                     (TxByronWinessCount nByronKeyWitnesses) = do

    txbody <- firstExceptT ShelleyTxReadUnsignedTxError . newExceptT $
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
                       -> ExceptT ShelleyTxCmdError IO Shelley.PParams
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ShelleyTxReadProtocolParamsError fpath) $ LBS.readFile fpath
  firstExceptT (ShelleyTxAesonDecodeProtocolParamsError fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams

data SomeWitnessSigningKey
  = AByronSigningKey           (Api.SigningKey Api.ByronKey)
  | APaymentSigningKey         (Api.SigningKey Api.PaymentKey)
  | AStakeSigningKey           (Api.SigningKey Api.StakeKey)
  | AStakePoolSigningKey       (Api.SigningKey Api.StakePoolKey)
  | AGenesisDelegateSigningKey (Api.SigningKey Api.GenesisDelegateKey)
  | AGenesisUTxOSigningKey     (Api.SigningKey Api.GenesisUTxOKey)

readSigningKeyFile
  :: SigningKeyFile
  -> ExceptT (Api.FileError Api.TextEnvelopeError) IO SomeWitnessSigningKey
readSigningKeyFile (SigningKeyFile skfile) =
    newExceptT $
      Api.readFileTextEnvelopeAnyOf fileTypes skfile
  where
    fileTypes =
      [ Api.FromSomeType (Api.AsSigningKey Api.AsByronKey)
                          AByronSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey)
                          APaymentSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeKey)
                          AStakeSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakePoolKey)
                          AStakePoolSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisDelegateKey)
                          AGenesisDelegateSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisUTxOKey)
                          AGenesisUTxOSigningKey
      ]

runTxGetTxId :: TxBodyFile -> ExceptT ShelleyTxCmdError IO ()
runTxGetTxId (TxBodyFile txbodyFile) = do
    txbody <- firstExceptT ShelleyTxReadUnsignedTxError . newExceptT $
                Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
    liftIO $ BS.putStrLn $ Api.serialiseToRawBytesHex (Api.getTxId txbody)


-- ----------------------------------------------------------------------------
-- Transaction metadata
--

data MetaDataJsonConversionError
  = ConversionErrDecodeJSON !String
  | ConversionErrToplevelNotMap
  | ConversionErrToplevelBadKey
  | ConversionErrBoolNotAllowed
  | ConversionErrNullNotAllowed
  | ConversionErrNumberNotInteger Double
  | ConversionErrLongerThan64Bytes
  deriving (Eq, Ord, Show)

renderMetaDataJsonConversionError :: MetaDataJsonConversionError -> Text
renderMetaDataJsonConversionError err =
  case err of
    ConversionErrDecodeJSON decErr -> "Error decoding JSON: " <> OldApi.textShow decErr
    ConversionErrToplevelNotMap -> "The JSON metadata top level must be a map (object) from word to value"
    ConversionErrToplevelBadKey -> "The JSON metadata top level must be a map with unsigned integer keys"
    ConversionErrBoolNotAllowed -> "JSON Bool value is not allowed in MetaData"
    ConversionErrNullNotAllowed -> "JSON Null value is not allowed in MetaData"
    ConversionErrNumberNotInteger _ -> "Only integers are allowed in MetaData"
    ConversionErrLongerThan64Bytes -> "JSON string is longer than 64 bytes"


readFileTxMetaData :: MetaDataFile
                   -> ExceptT ShelleyTxCmdError IO Api.TxMetadata
readFileTxMetaData (MetaDataFileJSON fp) = do
    bs <- handleIOExceptT (ShelleyTxMetaDataFileError fp) $
          LBS.readFile fp
    v  <- firstExceptT (ShelleyTxMetaDataConversionError fp . ConversionErrDecodeJSON) $
          hoistEither $
            Aeson.eitherDecode' bs
    firstExceptT (ShelleyTxMetaDataConversionError fp) $ hoistEither $
      jsonToMetadata v
readFileTxMetaData (MetaDataFileCBOR fp) = do
    bs <- handleIOExceptT (ShelleyTxMetaDataFileError fp) $
          BS.readFile fp
    firstExceptT (ShelleyTxMetaDecodeError fp) $ hoistEither $
      Api.deserialiseFromCBOR Api.AsTxMetadata bs


jsonToMetadata :: Aeson.Value
               -> Either MetaDataJsonConversionError Api.TxMetadata
jsonToMetadata (Aeson.Object kvs) =
    fmap (Api.makeTransactionMetadata . Map.fromList)
  . mapM (\(k,v) -> (,) <$> expectWord64 k <*> jsonToMetadataValue v)
  . HashMap.toList
  $ kvs
  where
    expectWord64 :: Text -> Either MetaDataJsonConversionError Word64
    expectWord64 =
        first (const ConversionErrToplevelBadKey)
      . Atto.parseOnly ((Atto.decimal <|> Atto.hexadecimal) <* Atto.endOfInput)

jsonToMetadata _ = Left ConversionErrToplevelNotMap


jsonToMetadataValue :: Aeson.Value
                    -> Either MetaDataJsonConversionError Api.TxMetadataValue
jsonToMetadataValue  Aeson.Null    = Left ConversionErrNullNotAllowed
jsonToMetadataValue (Aeson.Bool _) = Left ConversionErrBoolNotAllowed

jsonToMetadataValue (Aeson.Number sci) =
    case Scientific.floatingOrInteger sci :: Either Double Integer of
      Left  n -> Left (ConversionErrNumberNotInteger n)
      Right n -> Right (Api.TxMetaNumber n)

jsonToMetadataValue (Aeson.String txt)
    -- If the text is encoded in hex, we convert it to a byte string.
  | BS.take 2 utf8 == "0x"
  , let (raw, trailing) = Base16.decode (BS.drop 2 utf8)
  , BS.null trailing
  = if BS.length raw > 64
      then Left ConversionErrLongerThan64Bytes
      else Right (Api.TxMetaBytes raw)

  | otherwise
  = if BS.length utf8 > 64
            then Left ConversionErrLongerThan64Bytes
            else Right (Api.TxMetaText txt)
  where
    utf8 = encodeUtf8 txt

jsonToMetadataValue (Aeson.Array vs) =
    Api.TxMetaList <$> mapM jsonToMetadataValue (Vector.toList vs)

jsonToMetadataValue (Aeson.Object kvs) =
    Api.TxMetaMap <$> mapM (\(k,v) -> (,) <$> jsonToMetadataValue (Aeson.String k)
                                          <*> jsonToMetadataValue v)
                           (HashMap.toList kvs)
