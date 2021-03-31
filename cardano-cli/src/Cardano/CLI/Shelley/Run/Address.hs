{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Shelley.Run.Address
  ( ShelleyAddressCmdError(..)
  , SomeAddressVerificationKey(..)
  , buildShelleyAddress
  , renderShelleyAddressCmdError
  , runAddressCmd
  , runAddressKeyGen
  , readAddressVerificationKeyTextOrFile
  ) where

import           Cardano.Prelude hiding (putStrLn)

import           Data.Aeson as Aeson
import           System.Console.ANSI

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Console.ANSI as ANSI
import qualified System.IO as IO

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   newExceptT)

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (InputDecodeError, PaymentSource (..),
                   VerificationKeyOrFile, VerificationKeyTextOrFile,
                   VerificationKeyTextOrFileError (..), readVerificationKeyOrFile,
                   readVerificationKeyTextOrFileAnyOf, renderVerificationKeyTextOrFileError)
import           Cardano.CLI.Shelley.Parsers (AddressCmd (..), AddressKeyType (..), OutputFile (..))
import           Cardano.CLI.Shelley.Run.Address.Info (ShelleyAddressInfoError, runAddressInfo)
import           Cardano.CLI.Types

data ShelleyAddressCmdError
  = ShelleyAddressCmdAddressInfoError !ShelleyAddressInfoError
  | ShelleyAddressCmdAesonDecodeError !FilePath !Text
  | ShelleyAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyAddressCmdReadFileException !(FileError ())
  | ShelleyAddressCmdVerificationKeyTextOrFileError !VerificationKeyTextOrFileError
  | ShelleyAddressCmdWriteFileError !(FileError ())
  deriving Show

renderShelleyAddressCmdError :: ShelleyAddressCmdError -> Text
renderShelleyAddressCmdError err =
  case err of
    ShelleyAddressCmdAddressInfoError addrInfoErr ->
      Text.pack (displayError addrInfoErr)
    ShelleyAddressCmdReadKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    ShelleyAddressCmdVerificationKeyTextOrFileError vkTextOrFileErr ->
      renderVerificationKeyTextOrFileError vkTextOrFileErr
    ShelleyAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyAddressCmdAesonDecodeError fp decErr -> "Error decoding multisignature JSON object at: "
                                                   <> Text.pack fp <> " Error: " <> decErr
    ShelleyAddressCmdReadFileException fileErr -> Text.pack (displayError fileErr)

runAddressCmd :: AddressCmd -> ExceptT ShelleyAddressCmdError IO ()
runAddressCmd cmd =
  case cmd of
    AddressKeyGen kt vkf skf -> runAddressKeyGen kt vkf skf
    AddressKeyHash vkf mOFp -> runAddressKeyHash vkf mOFp
    AddressBuild paymentSource stkVk nw mOutFp -> runAddressBuild paymentSource stkVk nw mOutFp
    AddressBuildMultiSig sFp nId mOutFp -> runAddressBuildScript sFp nId mOutFp
    AddressInfo txt mOFp -> firstExceptT ShelleyAddressCmdAddressInfoError $ runAddressInfo txt mOFp

runAddressKeyGen :: AddressKeyType
                 -> VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyGen kt (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    case kt of
      AddressKeyShelley         -> generateAndWriteKeyFiles AsPaymentKey
      AddressKeyShelleyExtended -> generateAndWriteKeyFiles AsPaymentExtendedKey
      AddressKeyByron           -> generateAndWriteKeyFiles AsByronKey
  where
    generateAndWriteKeyFiles asType = do
      skey <- liftIO $ generateSigningKey asType
      let vkey = getVerificationKey skey
      firstExceptT ShelleyAddressCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
      firstExceptT ShelleyAddressCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey

    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Payment Signing Key"
    vkeyDesc = "Payment Verification Key"


runAddressKeyHash :: VerificationKeyTextOrFile
                  -> Maybe OutputFile
                  -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyHash vkeyTextOrFile mOutputFp = do
  vkey <- firstExceptT ShelleyAddressCmdVerificationKeyTextOrFileError $
            readAddressVerificationKeyTextOrFile vkeyTextOrFile

  let hexKeyHash = foldSomeAddressVerificationKey
                     (serialiseToRawBytesHex . verificationKeyHash) vkey

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runAddressBuild :: PaymentSource
                -> Maybe (VerificationKeyOrFile StakeKey)
                -> NetworkId
                -> Maybe OutputFile
                -> ExceptT ShelleyAddressCmdError IO ()
runAddressBuild paymentSource mbStkVkeyOrFile nw mOutFp = do
  outText <- case paymentSource of
    SourcePaymentKey payVkeyTextOrFile -> do
      payVKey <- firstExceptT ShelleyAddressCmdVerificationKeyTextOrFileError $
        readAddressVerificationKeyTextOrFile payVkeyTextOrFile

      addr <- case payVKey of
        AByronVerificationKey vk ->
          return (AddressByron (makeByronAddress nw vk))

        APaymentVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress vk mbStkVkeyOrFile nw

        APaymentExtendedVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress (castVerificationKey vk) mbStkVkeyOrFile nw

        AGenesisUTxOVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress (castVerificationKey vk) mbStkVkeyOrFile nw

      return $ serialiseAddress (addr :: AddressAny)

    SourcePaymentScript (ScriptFile fp) -> do
      scriptBytes <- handleIOExceptT (ShelleyAddressCmdReadFileException . FileIOError fp) $ LBS.readFile fp
      ScriptInAnyLang _lang script <-
        firstExceptT (ShelleyAddressCmdAesonDecodeError fp . Text.pack) $
        hoistEither $ Aeson.eitherDecode scriptBytes

      let payCred = PaymentCredentialByScript (hashScript script)

      serialiseAddress . makeShelleyAddress nw payCred <$> makeStakeAddressRef mbStkVkeyOrFile

  case mOutFp of
    Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath outText
    Nothing                 -> liftIO $ Text.putStr          outText

makeStakeAddressRef
  :: Maybe (VerificationKeyOrFile StakeKey)
  -> ExceptT ShelleyAddressCmdError IO StakeAddressReference
makeStakeAddressRef mbStkVkeyOrFile = do
  mstakeVKey <- case mbStkVkeyOrFile of
    Nothing -> pure Nothing
    Just stkVkeyOrFile -> firstExceptT ShelleyAddressCmdReadKeyFileError $
        fmap Just $ newExceptT $ readVerificationKeyOrFile AsStakeKey stkVkeyOrFile

  return $ maybe NoStakeAddress
    (StakeAddressByValue . StakeCredentialByKey . verificationKeyHash)
    mstakeVKey

buildShelleyAddress
  :: VerificationKey PaymentKey
  -> Maybe (VerificationKeyOrFile StakeKey)
  -> NetworkId
  -> ExceptT ShelleyAddressCmdError IO (Address ShelleyAddr)
buildShelleyAddress vkey mbStkVkeyOrFile nw =
  makeShelleyAddress nw (PaymentCredentialByKey (verificationKeyHash vkey)) <$> makeStakeAddressRef mbStkVkeyOrFile


--
-- Handling the variety of address key types
--

-- TODO: if we could make unions like this an instance of the Key class then
-- it would simplify some of the code above
data SomeAddressVerificationKey
  = AByronVerificationKey           (VerificationKey ByronKey)
  | APaymentVerificationKey         (VerificationKey PaymentKey)
  | APaymentExtendedVerificationKey (VerificationKey PaymentExtendedKey)
  | AGenesisUTxOVerificationKey     (VerificationKey GenesisUTxOKey)
  deriving (Show)

foldSomeAddressVerificationKey :: (forall keyrole. Key keyrole =>
                                   VerificationKey keyrole -> a)
                               -> SomeAddressVerificationKey -> a
foldSomeAddressVerificationKey f (AByronVerificationKey           vk) = f vk
foldSomeAddressVerificationKey f (APaymentVerificationKey         vk) = f vk
foldSomeAddressVerificationKey f (APaymentExtendedVerificationKey vk) = f vk
foldSomeAddressVerificationKey f (AGenesisUTxOVerificationKey     vk) = f vk

readAddressVerificationKeyTextOrFile
  :: VerificationKeyTextOrFile
  -> ExceptT VerificationKeyTextOrFileError IO SomeAddressVerificationKey
readAddressVerificationKeyTextOrFile vkTextOrFile =
    newExceptT $
      readVerificationKeyTextOrFileAnyOf bech32Types textEnvTypes vkTextOrFile
  where
    bech32Types =
      [ FromSomeType (AsVerificationKey AsByronKey)
                     AByronVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentKey)
                     APaymentVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                     APaymentExtendedVerificationKey
      ]

    textEnvTypes =
      [ FromSomeType (AsVerificationKey AsByronKey)
                     AByronVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentKey)
                     APaymentVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                     APaymentExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsGenesisUTxOKey)
                     AGenesisUTxOVerificationKey
      ]

--
-- Multisig addresses
--

runAddressBuildScript
  :: ScriptFile
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyAddressCmdError IO ()
runAddressBuildScript scriptFile networkId mOutputFile = do
  liftIO deprecationWarning
  runAddressBuild (SourcePaymentScript scriptFile) Nothing networkId mOutputFile

deprecationWarning :: IO ()
deprecationWarning = do
  ANSI.hSetSGR IO.stderr [SetColor Foreground Vivid Yellow]
  IO.hPutStrLn IO.stderr "WARNING: This CLI command is deprecated.  Please use 'address build' command instead."
  ANSI.hSetSGR IO.stderr [Reset]
