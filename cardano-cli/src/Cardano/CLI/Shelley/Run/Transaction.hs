{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( ShelleyTxCmdError
  , renderShelleyTxCmdError
  , runTransactionCmd
  ) where

import           Cardano.Prelude

import           Cardano.Api hiding (textShow)
import qualified Cardano.Api.Typed as Typed
import           Cardano.Api.TextView
import qualified Cardano.Api.Typed as Api
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath,
                   renderEnvSocketError)

import           Cardano.Config.Types

import           Cardano.CLI.Shelley.Parsers
import           Cardano.Config.Types (CertificateFile (..))

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (bimapExceptT, firstExceptT, handleIOExceptT, hoistEither,
                    left, newExceptT, right)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Ouroboros.Consensus.Shelley.Protocol.Crypto as Shelley
import           Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.PParams as Shelley


data ShelleyTxCmdError
  = ShelleyTxAesonDecodeProtocolParamsError !FilePath !Text
  | ShelleyTxMetaDataError !FilePath !Typed.MetaDataError
  | ShelleyTxMissingNetworkId
  | ShelleyTxSocketEnvError !EnvSocketError
  | ShelleyTxReadProtocolParamsError !FilePath !IOException
  | ShelleyTxReadSignedTxError !TextViewFileError
  | ShelleyTxReadUpdateError !ApiError
  | ShelleyTxReadUnsignedTxError !TextViewFileError
  | ShelleyTxCertReadError !FilePath !ApiError
  | ShelleyTxWriteSignedTxError !TextViewFileError
  | ShelleyTxWriteUnsignedTxError !TextViewFileError
  | ShelleyTxSubmitError !TxSubmitResult
  | ShelleyTxReadFileError !(Api.FileError Api.TextEnvelopeError)
  deriving Show

renderShelleyTxCmdError :: ShelleyTxCmdError -> Text
renderShelleyTxCmdError err =
  case err of
    ShelleyTxReadProtocolParamsError fp ioException ->
      "Error while reading protocol parameters at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyTxMetaDataError fp metaDataErr ->
       "Error reading metadata at: " <> textShow fp <> " Error: " <> Typed.renderMetaDataError metaDataErr
    ShelleyTxReadUnsignedTxError tvfError ->
      "Error while reading unsigned shelley tx: " <> renderTextViewFileError tvfError
    ShelleyTxReadSignedTxError tvfError ->
      "Error while reading signed shelley tx: " <> renderTextViewFileError tvfError
    ShelleyTxReadUpdateError apiError ->
      "Error while reading shelley update: " <> renderApiError apiError
    ShelleyTxSocketEnvError envSockErr -> renderEnvSocketError envSockErr
    ShelleyTxAesonDecodeProtocolParamsError fp decErr ->
      "Error while decoding the protocol parameters at: " <> textShow fp <> " Error: " <> textShow decErr
    ShelleyTxCertReadError fp apiErr ->
      "Error reading shelley certificate at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyTxWriteSignedTxError tvfError ->
      "Error while writing signed shelley tx: " <> renderTextViewFileError tvfError
    ShelleyTxWriteUnsignedTxError tvfError ->
      "Error while writing unsigned shelley tx: " <> renderTextViewFileError tvfError
    ShelleyTxSubmitError res ->
      "Error while submitting tx: " <> renderTxSubmitResult res
    ShelleyTxReadFileError fileErr -> Text.pack (Api.displayError fileErr)
    ShelleyTxMissingNetworkId -> "Please enter network id with your byron transaction"

runTransactionCmd :: TransactionCmd -> ExceptT ShelleyTxCmdError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee certs wdrls mMetaData mUpProp out ->
      runTxBuildRaw txins txouts ttl fee certs wdrls mMetaData mUpProp out
    TxSign txinfile skfiles network txoutfile ->
      runTxSign txinfile skfiles network txoutfile
    TxSubmit txFp network ->
      runTxSubmit txFp network
    TxCalculateMinFee txInCount txOutCount ttl network skFiles certFiles wdrls hasMD pParamsFile ->
      runTxCalculateMinFee txInCount txOutCount ttl network skFiles certFiles wdrls hasMD pParamsFile
    TxGetTxId txinfile ->
      runTxGetTxId txinfile

    _ -> liftIO $ putStrLn $ "runTransactionCmd: " ++ show cmd

runTxBuildRaw
  :: [Typed.TxIn]
  -> [TxOut]
  -> SlotNo
  -> Lovelace
  -> [CertificateFile]
  -> Withdrawals
  -> Maybe MetaDataFile
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildRaw txins txouts ttl fee certFps (WithdrawalsShelley wdrls) mMetaData mUpdateProp (TxBodyFile fpath) = do
  _certs <- mapM readShelleyCert certFps
  mUpProp <- maybeUpdate mUpdateProp
  txMetadata <- maybeMetaData mMetaData

  let txExtraContent = Typed.TxExtraContent
                         { Typed.txMetadata = txMetadata
                         , Typed.txWithdrawals = withDrls
                         , Typed.txCertificates = []
                         , Typed.txProtocolUpdates = Typed.ShelleyProtocolUpdate <$> mUpProp
                         }
      txBody = Typed.makeShelleyTransaction txExtraContent ttl (toEnum $ fromEnum fee) txins txOuts

  firstExceptT ShelleyTxWriteUnsignedTxError
    . newExceptT
    $ writeTxUnsigned Typed.AsShelleyTxBody fpath txBody
  where
    withDrls :: [(Typed.StakeAddress, Typed.Lovelace)]
    withDrls = let w = Map.toList $ Map.mapKeys Typed.rwdToStakeAddress (Typed.unWdrl wdrls)
               in map (\(addr, coin) -> (addr, toEnum $ fromEnum coin)) w

    txOuts :: [Typed.TxOut Typed.Shelley]
    txOuts = map convTxOut txouts

    maybeMetaData :: Maybe MetaDataFile -> ExceptT ShelleyTxCmdError IO (Maybe Typed.TxMetadata)
    maybeMetaData (Just (MetaDataFile mFp)) =
      bimapExceptT (ShelleyTxMetaDataError mFp) Just . newExceptT $ Typed.readJSONTxMetaData mFp
    maybeMetaData Nothing = right Nothing

    maybeUpdate :: Maybe UpdateProposalFile -> ExceptT ShelleyTxCmdError IO (Maybe (Shelley.Update Shelley.TPraosStandardCrypto))
    maybeUpdate (Just (UpdateProposalFile uFp )) = do
      mUpdate <- bimapExceptT ShelleyTxReadUpdateError Just . newExceptT $ readUpdate uFp
      case mUpdate of
        Just (ShelleyUpdate update) -> right $ Just update
        Nothing -> right Nothing
    maybeUpdate Nothing = right Nothing

convTxOut :: TxOut -> Typed.TxOut Typed.Shelley
convTxOut (TxOut addr lLace) =
  case addr of
    AddressByron ba ->
      Typed.TxOut (Typed.ByronAddress ba) (toEnum $ fromEnum lLace)
    AddressShelley (Shelley.Addr nw pc scr) ->
      Typed.TxOut (Typed.ShelleyAddress nw pc scr) (toEnum $ fromEnum lLace)
    AddressShelley (Shelley.AddrBootstrap (Shelley.BootstrapAddress bootStrapAddr)) ->
      Typed.TxOut (Typed.ByronAddress bootStrapAddr) (toEnum $ fromEnum lLace)
    AddressShelleyReward _ -> panic "Cardano.CLI.Shelley.Run.Transaction.convTxOut: Reward addresses are not valid tx outputs."

runTxSign :: TxBodyFile -> [SigningKeyFile] -> (Maybe Typed.NetworkId) -> TxFile -> ExceptT ShelleyTxCmdError IO ()
runTxSign (TxBodyFile infile) skfiles mNid (TxFile outfile) = do
    txu <- firstExceptT ShelleyTxReadUnsignedTxError . newExceptT $ readTxUnsigned Typed.AsShelleyTxBody infile
    sks <- readSigningKeyFiles skfiles


    witnessSigningkeys <- hoistEither $ mapM (toWitnessSigningKey mNid) sks

    let witnesses = map (Typed.makeKeyWitness txu) witnessSigningkeys

    firstExceptT ShelleyTxWriteSignedTxError
      . newExceptT
      . writeTxSigned Typed.AsShelleyTx outfile
      $ Typed.makeSignedTransaction witnesses txu

runTxSubmit :: FilePath -> Network -> ExceptT ShelleyTxCmdError IO ()
runTxSubmit txFp network = do
  sktFp <- firstExceptT ShelleyTxSocketEnvError $ readEnvSocketPath
  signedTx <- firstExceptT ShelleyTxReadSignedTxError . newExceptT $ readTxSigned Typed.AsShelleyTx txFp
  result   <- liftIO $ submitTx network sktFp signedTx
  case result of
    TxSubmitSuccess          -> return ()
    TxSubmitFailureShelley _ -> left (ShelleyTxSubmitError result)
    TxSubmitFailureByron   _ -> left (ShelleyTxSubmitError result)

runTxCalculateMinFee
  :: TxInCount
  -> TxOutCount
  -> SlotNo
  -> Network
  -> [SigningKeyFile]
  -> [CertificateFile]
  -> Withdrawals
  -> HasMetaData
  -> ProtocolParamsFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxCalculateMinFee (TxInCount txInCount) (TxOutCount txOutCount)
                     txTtl network skFiles certFiles wdrls hasMD
                     pParamsFile = do
    skeys <- readSigningKeyFiles skFiles
    certs <- mapM readShelleyCert certFiles
    pparams <- readProtocolParameters pParamsFile
    liftIO $ putStrLn
      $  "runTxCalculateMinFee: "
      ++ show (calculateShelleyMinFee pparams (dummyShelleyTxForFeeCalc skeys certs))
  where
    dummyShelleyTxForFeeCalc skeys certs =
      buildDummyShelleyTxForFeeCalc
        txInCount
        txOutCount
        txTtl
        network
        (map toOldApiSigningKey skeys)
        certs
        wdrls
        hasMD

readProtocolParameters :: ProtocolParamsFile -> ExceptT ShelleyTxCmdError IO PParams
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ShelleyTxReadProtocolParamsError fpath) $ LBS.readFile fpath
  firstExceptT (ShelleyTxAesonDecodeProtocolParamsError fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams

-- TODO: This should exist in its own module along with
-- a custom error type and an error rendering function.
readShelleyCert :: CertificateFile -> ExceptT ShelleyTxCmdError IO Certificate
readShelleyCert (CertificateFile fp) =
  firstExceptT (ShelleyTxCertReadError fp) . newExceptT $ readCertificate fp


data SomeSigningKey
  = ApiPaymentSigningKey !(Api.SigningKey Api.PaymentKey)
  | ApiGenesisUTxOSigningKey !(Api.SigningKey Api.GenesisUTxOKey)
  | ApiStakePoolSigningKey !(Api.SigningKey Api.StakePoolKey)
  | ApiByronSigningKey !(Api.SigningKey Api.ByronKey)
  | ApiStakeSigningKey !(Api.SigningKey Api.StakeKey)
  | ApiGenesisDelegateSigningKey !(Api.SigningKey Api.GenesisDelegateKey)

readSigningKeyFiles :: [SigningKeyFile] -> ExceptT ShelleyTxCmdError IO [SomeSigningKey]
readSigningKeyFiles files =
  newExceptT $ do
    xs <- mapM readSigningKeyFile files
    case partitionEithers xs of
      -- TODO: Would be nice to also provide a filepath to the signing key
      -- resulting in the error.
      (e:_, _) -> pure $ Left (ShelleyTxReadFileError e)
      ([], ys) -> pure $ Right ys

readSigningKeyFile
  :: SigningKeyFile
  -> IO (Either (Api.FileError Api.TextEnvelopeError) SomeSigningKey)
readSigningKeyFile (SigningKeyFile skfile) =
    Api.readFileTextEnvelopeAnyOf fileTypes skfile
  where
    fileTypes =
      [ Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey) ApiPaymentSigningKey
      -- TODO: Byron signing key
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisUTxOKey) ApiGenesisUTxOSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakePoolKey) ApiStakePoolSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsByronKey) ApiByronSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeKey) ApiStakeSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisDelegateKey) ApiGenesisDelegateSigningKey
      ]

-- | Convert a 'SomeSigningKey' (which wraps a new-style typed API signing
-- key) to a 'SigningKey' (old-style API signing key).
toOldApiSigningKey :: SomeSigningKey -> SigningKey
toOldApiSigningKey someSKey =
  case someSKey of
    ApiPaymentSigningKey (Api.PaymentSigningKey sk) ->
      SigningKeyShelley sk
    ApiGenesisUTxOSigningKey (Api.GenesisUTxOSigningKey sk) ->
      SigningKeyShelley sk
    ApiStakePoolSigningKey (Api.StakePoolSigningKey sk) ->
      SigningKeyShelley sk
    _ -> panic "toOldApiSigningKey: This will be deprecated"

toWitnessSigningKey :: Maybe Typed.NetworkId -> SomeSigningKey -> Either ShelleyTxCmdError (Typed.WitnessSigningKey Typed.Shelley)
toWitnessSigningKey mNid sss =
  case sss of
    ApiPaymentSigningKey sKey -> Right $ Typed.WitnessPaymentKey sKey
    ApiGenesisUTxOSigningKey sKey -> Right $ Typed.WitnessGenesisUTxOKey sKey
    ApiStakePoolSigningKey sKey -> Right $ Typed.WitnessStakePoolKey sKey
    ApiStakeSigningKey sKey -> Right $ Typed.WitnessStakeKey sKey
    ApiGenesisDelegateSigningKey sKey -> Right $ Typed.WitnessGenesisDelegateKey sKey
    ApiByronSigningKey sKey -> case mNid of
                                 Just nid -> Right $ Typed.WitnessByronKey sKey nid
                                 Nothing -> Left ShelleyTxMissingNetworkId

runTxGetTxId :: TxBodyFile -> ExceptT ShelleyTxCmdError IO ()
runTxGetTxId (TxBodyFile infile) = do
    tx <- firstExceptT ShelleyTxReadUnsignedTxError . newExceptT $
      readTxUnsigned Typed.AsShelleyTxBody infile
    liftIO . putStrLn . renderTxId $ Typed.getTxId tx
