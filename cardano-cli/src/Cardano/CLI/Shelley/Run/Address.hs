{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Shelley.Run.Address
  ( ShelleyAddressCmdError(..)
  , SomeAddressVerificationKey(..)
  , buildShelleyAddress
  , renderShelleyAddressCmdError
  , runAddressCmd
  , runAddressKeyGenToFile
  , makeStakeAddressRef
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT)
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (PaymentVerifier (..), StakeIdentifier (..),
                   StakeVerifier (..), VerificationKeyTextOrFile,
                   VerificationKeyTextOrFileError (..), generateKeyPair, readVerificationKeyOrFile,
                   readVerificationKeyTextOrFileAnyOf, renderVerificationKeyTextOrFileError)
import           Cardano.CLI.Shelley.Parsers (AddressCmd (..), AddressKeyType (..))
import           Cardano.CLI.Shelley.Run.Address.Info (ShelleyAddressInfoError, runAddressInfo)
import           Cardano.CLI.Shelley.Run.Read
import           Cardano.CLI.Types

data ShelleyAddressCmdError
  = ShelleyAddressCmdAddressInfoError !ShelleyAddressInfoError
  | ShelleyAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | ShelleyAddressCmdVerificationKeyTextOrFileError !VerificationKeyTextOrFileError
  | ShelleyAddressCmdWriteFileError !(FileError ())
  | ShelleyAddressCmdExpectedPaymentVerificationKey SomeAddressVerificationKey
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
    ShelleyAddressCmdReadScriptFileError fileErr ->
      Text.pack (displayError fileErr)
    ShelleyAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyAddressCmdExpectedPaymentVerificationKey someAddress ->
      "Expected payment verification key but got: " <> renderSomeAddressVerificationKey someAddress

runAddressCmd :: AddressCmd -> ExceptT ShelleyAddressCmdError IO ()
runAddressCmd cmd =
  case cmd of
    AddressKeyGen fmt kt vkf skf -> runAddressKeyGenToFile fmt kt vkf skf
    AddressKeyHash vkf mOFp -> runAddressKeyHash vkf mOFp
    AddressBuild paymentVerifier mbStakeVerifier nw mOutFp -> runAddressBuild paymentVerifier mbStakeVerifier nw mOutFp
    AddressInfo txt mOFp -> firstExceptT ShelleyAddressCmdAddressInfoError $ runAddressInfo txt mOFp

runAddressKeyGenToFile
  :: KeyOutputFormat
  -> AddressKeyType
  -> VerificationKeyFile
  -> SigningKeyFile
  -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyGenToFile fmt kt vkf skf = case kt of
  AddressKeyShelley         -> generateAndWriteKeyFiles fmt  AsPaymentKey          vkf skf
  AddressKeyShelleyExtended -> generateAndWriteKeyFiles fmt  AsPaymentExtendedKey  vkf skf
  AddressKeyByron           -> generateAndWriteByronKeyFiles AsByronKey            vkf skf

generateAndWriteByronKeyFiles
  :: Key keyrole
  => AsType keyrole
  -> VerificationKeyFile
  -> SigningKeyFile
  -> ExceptT ShelleyAddressCmdError IO ()
generateAndWriteByronKeyFiles asType vkf skf = do
  uncurry (writeByronPaymentKeyFiles vkf skf) =<< liftIO (generateKeyPair asType)

generateAndWriteKeyFiles
  :: Key keyrole
  => SerialiseAsBech32 (SigningKey keyrole)
  => SerialiseAsBech32 (VerificationKey keyrole)
  => KeyOutputFormat
  -> AsType keyrole
  -> VerificationKeyFile
  -> SigningKeyFile
  -> ExceptT ShelleyAddressCmdError IO ()
generateAndWriteKeyFiles fmt asType vkf skf = do
  uncurry (writePaymentKeyFiles fmt vkf skf) =<< liftIO (generateKeyPair asType)

writePaymentKeyFiles
  :: Key keyrole
  => SerialiseAsBech32 (SigningKey keyrole)
  => SerialiseAsBech32 (VerificationKey keyrole)
  => KeyOutputFormat
  -> VerificationKeyFile
  -> SigningKeyFile
  -> VerificationKey keyrole
  -> SigningKey keyrole
  -> ExceptT ShelleyAddressCmdError IO ()
writePaymentKeyFiles fmt (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) vkey skey = do
  firstExceptT ShelleyAddressCmdWriteFileError $ do
    case fmt of
      KeyOutputFormatTextEnvelope ->
        newExceptT
          $ writeLazyByteStringFile skeyPath
          $ textEnvelopeToJSON (Just skeyDesc) skey
      KeyOutputFormatBech32 ->
        newExceptT
          $ writeTextFile skeyPath
          $ serialiseToBech32 skey

    case fmt of
      KeyOutputFormatTextEnvelope ->
        newExceptT
          $ writeLazyByteStringFile vkeyPath
          $ textEnvelopeToJSON (Just vkeyDesc) vkey
      KeyOutputFormatBech32 ->
        newExceptT
          $ writeTextFile vkeyPath
          $ serialiseToBech32 vkey

  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Payment Signing Key"
    vkeyDesc = "Payment Verification Key"

writeByronPaymentKeyFiles
   :: Key keyrole
   => VerificationKeyFile
   -> SigningKeyFile
   -> VerificationKey keyrole
   -> SigningKey keyrole
   -> ExceptT ShelleyAddressCmdError IO ()
writeByronPaymentKeyFiles (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) vkey skey = do
  firstExceptT ShelleyAddressCmdWriteFileError $ do
    -- No bech32 encoding for Byron keys
    newExceptT $ writeLazyByteStringFile skeyPath $ textEnvelopeToJSON (Just skeyDesc) skey
    newExceptT $ writeLazyByteStringFile vkeyPath $ textEnvelopeToJSON (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Payment Signing Key"
    vkeyDesc = "Payment Verification Key"

runAddressKeyHash :: VerificationKeyTextOrFile
                  -> Maybe OutputFile
                  -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyHash vkeyTextOrFile mOutputFp = do
  vkey <- firstExceptT ShelleyAddressCmdVerificationKeyTextOrFileError $
             newExceptT $ readVerificationKeyTextOrFileAnyOf vkeyTextOrFile

  let hexKeyHash = foldSomeAddressVerificationKey
                     (serialiseToRawBytesHex . verificationKeyHash) vkey

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runAddressBuild :: PaymentVerifier
                -> Maybe StakeIdentifier
                -> NetworkId
                -> Maybe OutputFile
                -> ExceptT ShelleyAddressCmdError IO ()
runAddressBuild paymentVerifier mbStakeVerifier nw mOutFp = do
  outText <- case paymentVerifier of
    PaymentVerifierKey payVkeyTextOrFile -> do
      payVKey <- firstExceptT ShelleyAddressCmdVerificationKeyTextOrFileError $
         newExceptT $ readVerificationKeyTextOrFileAnyOf payVkeyTextOrFile

      addr <- case payVKey of
        AByronVerificationKey vk ->
          return (AddressByron (makeByronAddress nw vk))

        APaymentVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress vk mbStakeVerifier nw

        APaymentExtendedVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress (castVerificationKey vk) mbStakeVerifier nw

        AGenesisUTxOVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress (castVerificationKey vk) mbStakeVerifier nw
        nonPaymentKey ->
          left $ ShelleyAddressCmdExpectedPaymentVerificationKey nonPaymentKey
      return $ serialiseAddress (addr :: AddressAny)

    PaymentVerifierScriptFile (ScriptFile fp) -> do
      ScriptInAnyLang _lang script <-
        firstExceptT ShelleyAddressCmdReadScriptFileError $
          readFileScriptInAnyLang fp

      let payCred = PaymentCredentialByScript (hashScript script)

      stakeAddressReference <- maybe (return NoStakeAddress) makeStakeAddressRef mbStakeVerifier

      return $ serialiseAddress . makeShelleyAddress nw payCred $ stakeAddressReference

  case mOutFp of
    Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath outText
    Nothing                 -> liftIO $ Text.putStr          outText

makeStakeAddressRef
  :: StakeIdentifier
  -> ExceptT ShelleyAddressCmdError IO StakeAddressReference
makeStakeAddressRef stakeIdentifier =
  case stakeIdentifier of
    StakeIdentifierVerifier stakeVerifier ->
      case stakeVerifier of
        StakeVerifierKey stkVkeyOrFile -> do
          stakeVKey <- firstExceptT ShelleyAddressCmdReadKeyFileError $
            newExceptT $ readVerificationKeyOrFile AsStakeKey stkVkeyOrFile

          return . StakeAddressByValue . StakeCredentialByKey . verificationKeyHash $ stakeVKey

        StakeVerifierScriptFile (ScriptFile fp) -> do
          ScriptInAnyLang _lang script <-
            firstExceptT ShelleyAddressCmdReadScriptFileError $
              readFileScriptInAnyLang fp

          let stakeCred = StakeCredentialByScript (hashScript script)
          return (StakeAddressByValue stakeCred)
    StakeIdentifierAddress stakeAddr ->
        pure $ StakeAddressByValue $ stakeAddressCredential stakeAddr

buildShelleyAddress
  :: VerificationKey PaymentKey
  -> Maybe StakeIdentifier
  -> NetworkId
  -> ExceptT ShelleyAddressCmdError IO (Address ShelleyAddr)
buildShelleyAddress vkey mbStakeVerifier nw =
  makeShelleyAddress nw (PaymentCredentialByKey (verificationKeyHash vkey)) <$> maybe (return NoStakeAddress) makeStakeAddressRef mbStakeVerifier


--
-- Handling the variety of address key types
--


foldSomeAddressVerificationKey :: (forall keyrole. Key keyrole =>
                                   VerificationKey keyrole -> a)
                               -> SomeAddressVerificationKey -> a
foldSomeAddressVerificationKey f (AByronVerificationKey           vk) = f vk
foldSomeAddressVerificationKey f (APaymentVerificationKey         vk) = f vk
foldSomeAddressVerificationKey f (APaymentExtendedVerificationKey vk) = f vk
foldSomeAddressVerificationKey f (AGenesisUTxOVerificationKey     vk) = f vk
foldSomeAddressVerificationKey f (AKesVerificationKey             vk) = f vk
foldSomeAddressVerificationKey f (AGenesisDelegateExtendedVerificationKey vk) = f vk
foldSomeAddressVerificationKey f (AGenesisExtendedVerificationKey vk) = f vk
foldSomeAddressVerificationKey f (AVrfVerificationKey             vk) = f vk
foldSomeAddressVerificationKey f (AStakeVerificationKey           vk) = f vk
foldSomeAddressVerificationKey f (AStakeExtendedVerificationKey   vk) = f vk
