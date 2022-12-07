{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Read
  ( -- * Metadata
    MetadataError(..)
  , renderMetadataError
  , readFileTxMetadata
  , readTxMetadata

    -- * Script
  , ScriptWitnessError(..)
  , renderScriptWitnessError
  , readScriptDataOrFile
  , readScriptWitness
  , readScriptWitnessFiles
  , readScriptWitnessFilesThruple
  , ScriptDecodeError (..)
  , deserialiseScriptInAnyLang
  , readFileScriptInAnyLang

  -- * Script data (datums and redeemers)
  , ScriptDataError(..)
  , readScriptDatumOrFile
  , readScriptRedeemerOrFile
  , renderScriptDataError

  -- * Tx
  , CddlError
  , CddlTx(..)
  , IncompleteTx(..)
  , readFileTx
  , readFileTxBody
  , renderCddlError
  , readCddlTx -- For testing purposes

  -- * Tx witnesses
  , ReadWitnessSigningDataError(..)
  , renderReadWitnessSigningDataError
  , SomeWitness(..)
  , ByronOrShelleyWitness(..)
  , ShelleyBootstrapWitnessSigningKeyData(..)
  , CddlWitnessError(..)
  , readFileTxKeyWitness
  , readWitnessSigningData

  -- * Required signer
  , RequiredSignerError(..)
  , categoriseSomeWitness
  , readRequiredSigner
  ) where

import           Prelude

import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Word


import           Cardano.Api
import           Cardano.Api.Shelley

--TODO: do this nicely via the API too:
import qualified Cardano.Binary as CBOR
import           Data.Text (Text)

import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Types

-- Metadata

data MetadataError
  = MetadataErrorFile (FileError ())
  | MetadataErrorJsonParseError !FilePath !String
  | MetadataErrorConversionError !FilePath !TxMetadataJsonError
  | MetadataErrorValidationError !FilePath ![(Word64, TxMetadataRangeError)]
  | MetadataErrorDecodeError !FilePath !CBOR.DecoderError
  | MetadataErrorNotAvailableInEra AnyCardanoEra

renderMetadataError :: MetadataError -> Text
renderMetadataError (MetadataErrorFile fileErr) =
  Text.pack $ displayError fileErr
renderMetadataError (MetadataErrorJsonParseError fp jsonErr) =
  Text.pack $ "Invalid JSON format in file: " <> show fp <>
              "\nJSON parse error: " <> jsonErr
renderMetadataError (MetadataErrorConversionError fp metadataErr) =
  Text.pack $ "Error reading metadata at: " <> show fp <>
              "\n" <> displayError metadataErr
renderMetadataError (MetadataErrorValidationError fp errs) =
  Text.pack $ "Error validating transaction metadata at: " <> fp <> "\n" <>
      List.intercalate "\n"
        [ "key " <> show k <> ":" <> displayError valErr
        | (k, valErr) <- errs ]
renderMetadataError (MetadataErrorDecodeError fp metadataErr) =
  Text.pack $ "Error decoding CBOR metadata at: " <> show fp <>
              " Error: " <> show metadataErr
renderMetadataError (MetadataErrorNotAvailableInEra e) =
  "Transaction metadata not supported in " <> renderEra e

readTxMetadata :: CardanoEra era
               -> TxMetadataJsonSchema
               -> [MetadataFile]
               -> IO (Either MetadataError (TxMetadataInEra era))
readTxMetadata _ _ [] = return $ Right TxMetadataNone
readTxMetadata era' schema files =
  case txMetadataSupportedInEra era' of
    Nothing ->
      return . Left
        . MetadataErrorNotAvailableInEra
        $ getIsCardanoEraConstraint era' $ AnyCardanoEra era'
    Just supported -> do
      let exceptAllTxMetadata = mapM (readFileTxMetadata schema) files
      eAllTxMetaData <- runExceptT exceptAllTxMetadata
      return $ do
        metaData <- eAllTxMetaData
        Right $ TxMetadataInEra supported $ mconcat metaData

readFileTxMetadata
  :: TxMetadataJsonSchema
  -> MetadataFile
  -> ExceptT MetadataError IO TxMetadata
readFileTxMetadata mapping (MetadataFileJSON fp) = do
  bs <- handleIOExceptT (MetadataErrorFile . FileIOError fp)
          $ LBS.readFile fp
  v <- firstExceptT (MetadataErrorJsonParseError fp)
          $ hoistEither $ Aeson.eitherDecode' bs
  txMetadata' <- firstExceptT (MetadataErrorConversionError fp)
                  . hoistEither $ metadataFromJson mapping v
  firstExceptT (MetadataErrorValidationError fp)
    . hoistEither $ do
      validateTxMetadata txMetadata'
      return txMetadata'
readFileTxMetadata _ (MetadataFileCBOR fp) = do
  bs <- handleIOExceptT (MetadataErrorFile . FileIOError fp)
          $ BS.readFile fp
  txMetadata' <- firstExceptT (MetadataErrorDecodeError fp)
                  . hoistEither $ deserialiseFromCBOR AsTxMetadata bs
  firstExceptT (MetadataErrorValidationError fp)
    . hoistEither $ do
      validateTxMetadata txMetadata'
      return txMetadata'

-- Script witnesses/ Scripts

data ScriptWitnessError
  = ScriptWitnessErrorFile (FileError ScriptDecodeError)
  | ScriptWitnessErrorScriptLanguageNotSupportedInEra AnyScriptLanguage AnyCardanoEra
  | ScriptWitnessErrorExpectedSimple !FilePath !AnyScriptLanguage
  | ScriptWitnessErrorExpectedPlutus !FilePath !AnyScriptLanguage
  | ScriptWitnessErrorReferenceScriptsNotSupportedInEra !AnyCardanoEra
  | ScriptWitnessErrorScriptData ScriptDataError

renderScriptWitnessError :: ScriptWitnessError -> Text
renderScriptWitnessError (ScriptWitnessErrorFile err) =
  Text.pack $ displayError err
renderScriptWitnessError (ScriptWitnessErrorScriptLanguageNotSupportedInEra (AnyScriptLanguage lang) anyEra) =
  "The script language " <> Text.pack (show lang) <> " is not supported in the " <>
  renderEra anyEra <> " era."
renderScriptWitnessError (ScriptWitnessErrorExpectedSimple file (AnyScriptLanguage lang)) =
  Text.pack $ file <> ": expected a script in the simple script language, " <>
  "but it is actually using " <> show lang <> ". Alternatively, to use " <>
  "a Plutus script, you must also specify the redeemer " <>
  "(datum if appropriate) and script execution units."
renderScriptWitnessError (ScriptWitnessErrorExpectedPlutus file (AnyScriptLanguage lang)) =
  Text.pack $ file <> ": expected a script in the Plutus script language, " <>
  "but it is actually using " <> show lang <> "."
renderScriptWitnessError (ScriptWitnessErrorReferenceScriptsNotSupportedInEra anyEra) =
  "Reference scripts not supported in era': " <> renderEra anyEra
renderScriptWitnessError (ScriptWitnessErrorScriptData sDataError) =
  renderScriptDataError sDataError

readScriptWitnessFiles
  :: CardanoEra era
  -> [(a, Maybe (ScriptWitnessFiles ctx))]
  -> ExceptT ScriptWitnessError IO [(a, Maybe (ScriptWitness ctx era))]
readScriptWitnessFiles era' = mapM readSwitFile
 where
  readSwitFile (tIn, Just switFile) = do
      sWit <- readScriptWitness era' switFile
      return (tIn, Just sWit)
  readSwitFile (tIn, Nothing) = return (tIn, Nothing)

readScriptWitnessFilesThruple
  :: CardanoEra era
  -> [(a, b, Maybe (ScriptWitnessFiles ctx))]
  -> ExceptT ScriptWitnessError IO [(a, b, Maybe (ScriptWitness ctx era))]
readScriptWitnessFilesThruple era' = mapM readSwitFile
 where
  readSwitFile (tIn, b, Just switFile) = do
      sWit <- readScriptWitness era' switFile
      return (tIn, b, Just sWit)
  readSwitFile (tIn, b, Nothing) = return (tIn, b, Nothing)

readScriptWitness
  :: CardanoEra era
  -> ScriptWitnessFiles witctx
  -> ExceptT ScriptWitnessError IO (ScriptWitness witctx era)
readScriptWitness era' (SimpleScriptWitnessFile (ScriptFile scriptFile)) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT ScriptWitnessErrorFile $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era' script
    case script' of
      SimpleScript version sscript ->
        return . SimpleScriptWitness
                   langInEra version $ SScript sscript

      -- If the supplied cli flags were for a simple script (i.e. the user did
      -- not supply the datum, redeemer or ex units), but the script file turns
      -- out to be a valid plutus script, then we must fail.
      PlutusScript{} ->
        left $ ScriptWitnessErrorExpectedSimple
                 scriptFile
                 (AnyScriptLanguage lang)

readScriptWitness era' (PlutusScriptWitnessFiles
                          (ScriptFile scriptFile)
                          datumOrFile
                          redeemerOrFile
                          execUnits) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT ScriptWitnessErrorFile $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era' script
    case script' of
      PlutusScript version pscript -> do
        datum <- firstExceptT ScriptWitnessErrorScriptData
                   $ readScriptDatumOrFile    datumOrFile
        redeemer <- firstExceptT ScriptWitnessErrorScriptData
                      $ readScriptRedeemerOrFile redeemerOrFile
        return $ PlutusScriptWitness
                   langInEra version (PScript pscript)
                   datum
                   redeemer
                   execUnits

      -- If the supplied cli flags were for a plutus script (i.e. the user did
      -- supply the datum, redeemer and ex units), but the script file turns
      -- out to be a valid simple script, then we must fail.
      SimpleScript{} ->
        left $ ScriptWitnessErrorExpectedPlutus
                 scriptFile
                 (AnyScriptLanguage lang)

readScriptWitness era' (PlutusReferenceScriptWitnessFiles refTxIn
                          anyScrLang@(AnyScriptLanguage anyScriptLanguage)
                          datumOrFile redeemerOrFile execUnits mPid) = do
  case refInsScriptsAndInlineDatsSupportedInEra era' of
    Nothing -> left $ ScriptWitnessErrorReferenceScriptsNotSupportedInEra
                    $ getIsCardanoEraConstraint era' (AnyCardanoEra era')
    Just _ -> do

      case scriptLanguageSupportedInEra era' anyScriptLanguage of
        Just sLangInEra ->
          case languageOfScriptLanguageInEra sLangInEra of
            SimpleScriptLanguage _v ->
              -- TODO: We likely need another datatype eg data ReferenceScriptWitness lang
              -- in order to make this branch unrepresentable.
              error "readScriptWitness: Should not be possible to specify a simple script"
            PlutusScriptLanguage version -> do
              datum <- firstExceptT ScriptWitnessErrorScriptData
                         $ readScriptDatumOrFile    datumOrFile
              redeemer <- firstExceptT ScriptWitnessErrorScriptData
                            $ readScriptRedeemerOrFile redeemerOrFile
              return $ PlutusScriptWitness
                         sLangInEra
                         version
                         (PReferenceScript refTxIn (unPolicyId <$> mPid))
                         datum redeemer execUnits
        Nothing ->
          left $ ScriptWitnessErrorScriptLanguageNotSupportedInEra anyScrLang (anyCardanoEra era')
readScriptWitness era' (SimpleReferenceScriptWitnessFiles refTxIn
                         anyScrLang@(AnyScriptLanguage anyScriptLanguage) mPid) = do
  case refInsScriptsAndInlineDatsSupportedInEra era' of
    Nothing -> left $ ScriptWitnessErrorReferenceScriptsNotSupportedInEra
                    $ getIsCardanoEraConstraint era' (AnyCardanoEra era')
    Just _ -> do
      case scriptLanguageSupportedInEra era' anyScriptLanguage of
        Just sLangInEra ->
          case languageOfScriptLanguageInEra sLangInEra of
            SimpleScriptLanguage v ->
              return . SimpleScriptWitness sLangInEra v
                     $ SReferenceScript refTxIn (unPolicyId <$> mPid)
            PlutusScriptLanguage{} ->
              error "readScriptWitness: Should not be possible to specify a plutus script"
        Nothing ->
          left $ ScriptWitnessErrorScriptLanguageNotSupportedInEra anyScrLang (anyCardanoEra era')

validateScriptSupportedInEra :: CardanoEra era
                             -> ScriptInAnyLang
                             -> ExceptT ScriptWitnessError IO (ScriptInEra era)
validateScriptSupportedInEra era' script@(ScriptInAnyLang lang _) =
    case toScriptInEra era' script of
      Nothing -> left $ ScriptWitnessErrorScriptLanguageNotSupportedInEra
                          (AnyScriptLanguage lang) (anyCardanoEra era')
      Just script' -> pure script'

data ScriptDataError =
    ScriptDataErrorFile (FileError ())
  | ScriptDataErrorJsonParse !FilePath !String
  | ScriptDataErrorConversion !FilePath !ScriptDataJsonError
  | ScriptDataErrorValidation !FilePath !ScriptDataRangeError
  | ScriptDataErrorMetadataDecode !FilePath !CBOR.DecoderError

renderScriptDataError :: ScriptDataError -> Text
renderScriptDataError (ScriptDataErrorFile err) =
  Text.pack $ displayError err
renderScriptDataError (ScriptDataErrorJsonParse fp jsonErr) =
  Text.pack $ "Invalid JSON format in file: " <> show fp <>
              "\nJSON parse error: " <> jsonErr
renderScriptDataError (ScriptDataErrorConversion fp sDataJsonErr) =
  Text.pack $ "Error reading metadata at: " <> show fp <>
              "\n" <> displayError sDataJsonErr
renderScriptDataError (ScriptDataErrorValidation fp sDataRangeErr) =
  Text.pack $ "Error validating script data at: " <> show fp <> ":\n" <>
              displayError sDataRangeErr
renderScriptDataError (ScriptDataErrorMetadataDecode fp decoderErr) =
  Text.pack $ "Error decoding CBOR metadata at: " <> show fp <>
              " Error: " <> show decoderErr

readScriptDatumOrFile :: ScriptDatumOrFile witctx
                      -> ExceptT ScriptDataError IO (ScriptDatum witctx)
readScriptDatumOrFile (ScriptDatumOrFileForTxIn df) = ScriptDatumForTxIn <$>
                                                        readScriptDataOrFile df
readScriptDatumOrFile InlineDatumPresentAtTxIn      = pure InlineScriptDatum
readScriptDatumOrFile NoScriptDatumOrFileForMint    = pure NoScriptDatumForMint
readScriptDatumOrFile NoScriptDatumOrFileForStake   = pure NoScriptDatumForStake

readScriptRedeemerOrFile :: ScriptRedeemerOrFile
                         -> ExceptT ScriptDataError IO ScriptRedeemer
readScriptRedeemerOrFile = readScriptDataOrFile

readScriptDataOrFile :: ScriptDataOrFile
                     -> ExceptT ScriptDataError IO ScriptData
readScriptDataOrFile (ScriptDataValue d) = return d
readScriptDataOrFile (ScriptDataJsonFile fp) = do
  bs <- handleIOExceptT (ScriptDataErrorFile . FileIOError fp) $ LBS.readFile fp
  v  <- firstExceptT (ScriptDataErrorJsonParse fp)
          $ hoistEither $ Aeson.eitherDecode' bs
  sd <- firstExceptT (ScriptDataErrorConversion fp)
          $ hoistEither $ scriptDataFromJson ScriptDataJsonDetailedSchema v
  firstExceptT (ScriptDataErrorValidation fp)
          $ hoistEither $ validateScriptData sd
  return sd
readScriptDataOrFile (ScriptDataCborFile fp) = do
  bs <- handleIOExceptT (ScriptDataErrorFile . FileIOError fp)
          $ BS.readFile fp
  sd <- firstExceptT (ScriptDataErrorMetadataDecode fp)
          $ hoistEither $ deserialiseFromCBOR AsScriptData bs
  firstExceptT (ScriptDataErrorValidation fp)
          $ hoistEither $ validateScriptData sd
  return sd


--
-- Handling decoding the variety of script languages and formats
--

data ScriptDecodeError =
       ScriptDecodeTextEnvelopeError TextEnvelopeError
     | ScriptDecodeSimpleScriptError JsonDecodeError
  deriving Show

instance Error ScriptDecodeError where
  displayError (ScriptDecodeTextEnvelopeError err) =
    "Error decoding script: " ++ displayError err
  displayError (ScriptDecodeSimpleScriptError err) =
    "Syntax error in script: " ++ displayError err


-- | Read a script file. The file can either be in the text envelope format
-- wrapping the binary representation of any of the supported script languages,
-- or alternatively it can be a JSON format file for one of the simple script
-- language versions.
--
readFileScriptInAnyLang :: FilePath
                        -> ExceptT (FileError ScriptDecodeError) IO
                                   ScriptInAnyLang
readFileScriptInAnyLang file = do
  scriptBytes <- handleIOExceptT (FileIOError file) $ BS.readFile file
  firstExceptT (FileError file) $ hoistEither $
    deserialiseScriptInAnyLang scriptBytes


deserialiseScriptInAnyLang :: BS.ByteString
                           -> Either ScriptDecodeError ScriptInAnyLang
deserialiseScriptInAnyLang bs =
    -- Accept either the text envelope format wrapping the binary serialisation,
    -- or accept the simple script language in its JSON format.
    --
    case deserialiseFromJSON AsTextEnvelope bs of
      Left _   ->
        -- The SimpleScript language has the property that it is backwards
        -- compatible, so we can parse as the latest version and then downgrade
        -- to the minimum version that has all the features actually used.
        case deserialiseFromJSON (AsSimpleScript AsSimpleScriptV2) bs of
          Left  err    -> Left (ScriptDecodeSimpleScriptError err)
          Right script -> Right (toMinimumSimpleScriptVersion script)

      Right te ->
        case deserialiseFromTextEnvelopeAnyOf textEnvTypes te of
          Left  err    -> Left (ScriptDecodeTextEnvelopeError err)
          Right script -> Right script

  where
    -- TODO: Think of a way to get type checker to warn when there is a missing
    -- script version.
    textEnvTypes :: [FromSomeType HasTextEnvelope ScriptInAnyLang]
    textEnvTypes =
      [ FromSomeType (AsScript AsSimpleScriptV1)
                     (ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1))

      , FromSomeType (AsScript AsSimpleScriptV2)
                     (ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2))

      , FromSomeType (AsScript AsPlutusScriptV1)
                     (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1))

      , FromSomeType (AsScript AsPlutusScriptV2)
                     (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2))
      ]

    toMinimumSimpleScriptVersion :: SimpleScript SimpleScriptV2
                                 -> ScriptInAnyLang
    toMinimumSimpleScriptVersion s =
      -- TODO alonzo: this will need to be adjusted when more versions are added
      -- with appropriate helper functions it can probably be done in an
      -- era-generic style
      case adjustSimpleScriptVersion SimpleScriptV1 s of
        Nothing -> ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2)
                                   (SimpleScript SimpleScriptV2 s)
        Just s' -> ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1)
                                   (SimpleScript SimpleScriptV1 s')


-- Tx & TxBody

newtype CddlTx = CddlTx {unCddlTx :: InAnyCardanoEra Tx} deriving (Show, Eq)

readFileTx :: FilePath -> IO (Either CddlError (InAnyCardanoEra Tx))
readFileTx fp = do
  eAnyTx <- readFileInAnyCardanoEra AsTx fp
  case eAnyTx of
    Left e -> fmap unCddlTx <$> acceptTxCDDLSerialisation e
    Right tx -> return $ Right tx

-- IncompleteCddlFormattedTx is an CDDL formatted tx or partial tx
-- (respectively needs additional witnesses or totally unwitnessed)
-- while UnwitnessedCliFormattedTxBody is CLI formatted TxBody and
-- needs to be key witnessed.

data IncompleteTx
  = UnwitnessedCliFormattedTxBody (InAnyCardanoEra TxBody)
  | IncompleteCddlFormattedTx (InAnyCardanoEra Tx)

readFileTxBody :: FilePath -> IO (Either CddlError IncompleteTx)
readFileTxBody fp = do
  eTxBody <- readFileInAnyCardanoEra AsTxBody fp
  case eTxBody of
    Left e -> fmap (IncompleteCddlFormattedTx . unCddlTx) <$> acceptTxCDDLSerialisation e
    Right txBody -> return $ Right $ UnwitnessedCliFormattedTxBody txBody

data CddlError = CddlErrorTextEnv
                   !(FileError TextEnvelopeError)
                   !(FileError TextEnvelopeCddlError)
               | CddlIOError (FileError TextEnvelopeError)

renderCddlError :: CddlError -> Text
renderCddlError (CddlErrorTextEnv textEnvErr cddlErr) =
  "Failed to decode neither the cli's serialisation format nor the ledger's \
  \CDDL serialisation format. TextEnvelope error: " <> Text.pack (displayError textEnvErr) <> "\n" <>
  "TextEnvelopeCddl error: " <> Text.pack (displayError cddlErr)
renderCddlError (CddlIOError e) = Text.pack $ displayError e

acceptTxCDDLSerialisation
  :: FileError TextEnvelopeError
  -> IO (Either CddlError CddlTx)
acceptTxCDDLSerialisation err =
  case err of
   e@(FileError fp (TextEnvelopeDecodeError _)) ->
      first (CddlErrorTextEnv e) <$> readCddlTx fp
   e@(FileError fp (TextEnvelopeAesonDecodeError _)) ->
      first (CddlErrorTextEnv e) <$> readCddlTx fp
   e@(FileError fp (TextEnvelopeTypeError _ _)) ->
      first (CddlErrorTextEnv e) <$> readCddlTx fp
   e@FileErrorTempFile{} -> return . Left $ CddlIOError e
   e@FileIOError{} -> return . Left $ CddlIOError e

readCddlTx :: FilePath -> IO (Either (FileError TextEnvelopeCddlError) CddlTx)
readCddlTx = readFileTextEnvelopeCddlAnyOf teTypes
 where
    teTypes = [ FromCDDLTx "Witnessed Tx ByronEra" CddlTx
              , FromCDDLTx "Witnessed Tx ShelleyEra" CddlTx
              , FromCDDLTx "Witnessed Tx AllegraEra" CddlTx
              , FromCDDLTx "Witnessed Tx MaryEra" CddlTx
              , FromCDDLTx "Witnessed Tx AlonzoEra" CddlTx
              , FromCDDLTx "Witnessed Tx BabbageEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx ByronEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx ShelleyEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx AllegraEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx MaryEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx AlonzoEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx BabbageEra" CddlTx
              ]

-- Tx witnesses

newtype CddlWitness = CddlWitness { unCddlWitness :: InAnyCardanoEra KeyWitness}

readFileTxKeyWitness :: FilePath
                -> IO (Either CddlWitnessError (InAnyCardanoEra KeyWitness))
readFileTxKeyWitness fp = do
  eWitness <- readFileInAnyCardanoEra AsKeyWitness fp
  case eWitness of
    Left e -> fmap unCddlWitness <$> acceptKeyWitnessCDDLSerialisation e
    Right keyWit -> return $ Right keyWit

data CddlWitnessError
  = CddlWitnessErrorTextEnv
      (FileError TextEnvelopeError)
      (FileError TextEnvelopeCddlError)
  | CddlWitnessIOError (FileError TextEnvelopeError)

-- TODO: This is a stop gap to avoid modifying the TextEnvelope
-- related functions. We intend to remove this after fully deprecating
-- the cli's serialisation format
acceptKeyWitnessCDDLSerialisation
  :: FileError TextEnvelopeError
  -> IO (Either CddlWitnessError CddlWitness)
acceptKeyWitnessCDDLSerialisation err =
  case err of
    e@(FileError fp (TextEnvelopeDecodeError _)) ->
      first (CddlWitnessErrorTextEnv e) <$> readCddlWitness fp
    e@(FileError fp (TextEnvelopeAesonDecodeError _)) ->
      first (CddlWitnessErrorTextEnv e) <$> readCddlWitness fp
    e@(FileError fp (TextEnvelopeTypeError _ _)) ->
      first (CddlWitnessErrorTextEnv e) <$> readCddlWitness fp
    e@FileErrorTempFile{} -> return . Left $ CddlWitnessIOError e
    e@FileIOError{} -> return . Left $ CddlWitnessIOError e

readCddlWitness
  :: FilePath
  -> IO (Either (FileError TextEnvelopeCddlError) CddlWitness)
readCddlWitness fp = do
  readFileTextEnvelopeCddlAnyOf teTypes fp
 where
  teTypes = [ FromCDDLWitness "TxWitness ShelleyEra" CddlWitness
            , FromCDDLWitness "TxWitness AllegraEra" CddlWitness
            , FromCDDLWitness "TxWitness MaryEra" CddlWitness
            , FromCDDLWitness "TxWitness AlonzoEra" CddlWitness
            , FromCDDLWitness "TxWitness BabbageEra" CddlWitness
            ]

-- Witness handling

data SomeWitness
  = AByronSigningKey           (SigningKey ByronKey) (Maybe (Address ByronAddr))
  | APaymentSigningKey         (SigningKey PaymentKey)
  | APaymentExtendedSigningKey (SigningKey PaymentExtendedKey)
  | AStakeSigningKey           (SigningKey StakeKey)
  | AStakeExtendedSigningKey   (SigningKey StakeExtendedKey)
  | AStakePoolSigningKey       (SigningKey StakePoolKey)
  | AGenesisSigningKey         (SigningKey GenesisKey)
  | AGenesisExtendedSigningKey (SigningKey GenesisExtendedKey)
  | AGenesisDelegateSigningKey (SigningKey GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningKey
                               (SigningKey GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningKey     (SigningKey GenesisUTxOKey)


-- | Data required for constructing a Shelley bootstrap witness.
data ShelleyBootstrapWitnessSigningKeyData
  = ShelleyBootstrapWitnessSigningKeyData
      !(SigningKey ByronKey)
      -- ^ Byron signing key.
      !(Maybe (Address ByronAddr))
      -- ^ An optionally specified Byron address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Byron witness.

-- | Some kind of Byron or Shelley witness.
data ByronOrShelleyWitness
  = AByronWitness !ShelleyBootstrapWitnessSigningKeyData
  | AShelleyKeyWitness !ShelleyWitnessSigningKey

categoriseSomeWitness :: SomeWitness -> ByronOrShelleyWitness
categoriseSomeWitness swsk =
  case swsk of
    AByronSigningKey           sk addr -> AByronWitness (ShelleyBootstrapWitnessSigningKeyData sk addr)
    APaymentSigningKey         sk      -> AShelleyKeyWitness (WitnessPaymentKey         sk)
    APaymentExtendedSigningKey sk      -> AShelleyKeyWitness (WitnessPaymentExtendedKey sk)
    AStakeSigningKey           sk      -> AShelleyKeyWitness (WitnessStakeKey           sk)
    AStakeExtendedSigningKey   sk      -> AShelleyKeyWitness (WitnessStakeExtendedKey   sk)
    AStakePoolSigningKey       sk      -> AShelleyKeyWitness (WitnessStakePoolKey       sk)
    AGenesisSigningKey         sk      -> AShelleyKeyWitness (WitnessGenesisKey sk)
    AGenesisExtendedSigningKey sk      -> AShelleyKeyWitness (WitnessGenesisExtendedKey sk)
    AGenesisDelegateSigningKey sk      -> AShelleyKeyWitness (WitnessGenesisDelegateKey sk)
    AGenesisDelegateExtendedSigningKey sk
                                       -> AShelleyKeyWitness (WitnessGenesisDelegateExtendedKey sk)
    AGenesisUTxOSigningKey     sk      -> AShelleyKeyWitness (WitnessGenesisUTxOKey     sk)

data ReadWitnessSigningDataError
  = ReadWitnessSigningDataSigningKeyDecodeError !(FileError InputDecodeError)
  | ReadWitnessSigningDataScriptError !(FileError JsonDecodeError)
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
  -> IO (Either ReadWitnessSigningDataError SomeWitness)
readWitnessSigningData (KeyWitnessSigningData (SigningKeyFile skFile) mbByronAddr) = do
    eRes <- first ReadWitnessSigningDataSigningKeyDecodeError
             <$> readKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
    return $ do
      res <- eRes
      case (res, mbByronAddr) of
        (AByronSigningKey _ _, Just _) -> pure res
        (AByronSigningKey _ _, Nothing) -> pure res
        (_, Nothing) -> pure res
        (_, Just _) ->
          -- A Byron address should only be specified along with a Byron signing key.
          Left ReadWitnessSigningDataSigningKeyAndAddressMismatch
  where
    textEnvFileTypes =
      [ FromSomeType (AsSigningKey AsByronKey)
                          (`AByronSigningKey` mbByronAddr)
      , FromSomeType (AsSigningKey AsPaymentKey)
                          APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                          AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                          AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsGenesisKey)
                          AGenesisSigningKey
      , FromSomeType (AsSigningKey AsGenesisExtendedKey)
                          AGenesisExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateKey)
                          AGenesisDelegateSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey)
                          AGenesisDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisUTxOKey)
                          AGenesisUTxOSigningKey
      ]

    bech32FileTypes =
      [ FromSomeType (AsSigningKey AsPaymentKey)
                          APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                          AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                          AStakePoolSigningKey
      ]

-- Required signers

data RequiredSignerError
  = RequiredSignerErrorFile (FileError InputDecodeError)
  | RequiredSignerErrorByronKey SigningKeyFile

readRequiredSigner :: RequiredSigner -> IO (Either RequiredSignerError (Hash PaymentKey))
readRequiredSigner (RequiredSignerHash h) = return $ Right h
readRequiredSigner (RequiredSignerSkeyFile skFile@(SigningKeyFile skFp)) = do
  eKeyWit <- first RequiredSignerErrorFile <$> readKeyFileAnyOf bech32FileTypes textEnvFileTypes skFp
  return $ do
    keyWit <- eKeyWit
    case categoriseSomeWitness keyWit of
      AByronWitness _ ->
        Left $ RequiredSignerErrorByronKey skFile
      AShelleyKeyWitness skey ->
        return . getHash $ toShelleySigningKey skey
 where
   textEnvFileTypes =
     [ FromSomeType (AsSigningKey AsPaymentKey) APaymentSigningKey
     , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
     ]
   bech32FileTypes = []

   getHash :: ShelleySigningKey -> Hash PaymentKey
   getHash (ShelleyExtendedSigningKey sk) =
     let extSKey = PaymentExtendedSigningKey sk
         payVKey = castVerificationKey $ getVerificationKey extSKey
     in verificationKeyHash payVKey
   getHash (ShelleyNormalSigningKey sk) =
     verificationKeyHash . getVerificationKey $ PaymentSigningKey sk

-- Misc

readFileInAnyCardanoEra
  :: ( HasTextEnvelope (thing ByronEra)
     , HasTextEnvelope (thing ShelleyEra)
     , HasTextEnvelope (thing AllegraEra)
     , HasTextEnvelope (thing MaryEra)
     , HasTextEnvelope (thing AlonzoEra)
     , HasTextEnvelope (thing BabbageEra)
     )
  => (forall era. AsType era -> AsType (thing era))
  -> FilePath
  -> IO (Either (FileError TextEnvelopeError) (InAnyCardanoEra thing))
readFileInAnyCardanoEra asThing =
 readFileTextEnvelopeAnyOf
   [ FromSomeType (asThing AsByronEra)   (InAnyCardanoEra ByronEra)
   , FromSomeType (asThing AsShelleyEra) (InAnyCardanoEra ShelleyEra)
   , FromSomeType (asThing AsAllegraEra) (InAnyCardanoEra AllegraEra)
   , FromSomeType (asThing AsMaryEra)    (InAnyCardanoEra MaryEra)
   , FromSomeType (asThing AsAlonzoEra)  (InAnyCardanoEra AlonzoEra)
   , FromSomeType (asThing AsBabbageEra) (InAnyCardanoEra BabbageEra)
   ]
