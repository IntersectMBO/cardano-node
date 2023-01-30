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

import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (PaymentVerifier (..), StakeVerifier (..),
                   VerificationKeyTextOrFile, VerificationKeyTextOrFileError (..), generateKeyPair,
                   readVerificationKeyOrFile, readVerificationKeyTextOrFileAnyOf,
                   renderVerificationKeyTextOrFileError)
import           Cardano.CLI.Shelley.Parsers (AddressCmd (..), AddressKeyType (..), OutputFile (..))
import           Cardano.CLI.Shelley.Run.Address.Info (ShelleyAddressInfoError, runAddressInfo)
import           Cardano.CLI.Shelley.Run.Read
import           Cardano.CLI.Types
import           Cardano.Prelude (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Text (Text)

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
    AddressKeyGen kt vkf skf -> runAddressKeyGenToFile kt vkf skf
    AddressKeyHash vkf mOFp -> runAddressKeyHash vkf mOFp
    AddressBuild paymentVerifier mbStakeVerifier nw mOutFp -> runAddressBuild paymentVerifier mbStakeVerifier nw mOutFp
    AddressInfo txt mOFp -> firstExceptT ShelleyAddressCmdAddressInfoError $ runAddressInfo txt mOFp

runAddressKeyGenToFile
  :: AddressKeyType
  -> VerificationKeyFile
  -> SigningKeyFile
  -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyGenToFile kt vkf skf = case kt of
  AddressKeyShelley         -> generateAndWriteKeyFiles AsPaymentKey          vkf skf
  AddressKeyShelleyExtended -> generateAndWriteKeyFiles AsPaymentExtendedKey  vkf skf
  AddressKeyByron           -> generateAndWriteKeyFiles AsByronKey            vkf skf

generateAndWriteKeyFiles
  :: Key keyrole
  => AsType keyrole
  -> VerificationKeyFile
  -> SigningKeyFile
  -> ExceptT ShelleyAddressCmdError IO ()
generateAndWriteKeyFiles asType vkf skf = do
  uncurry (writePaymentKeyFiles vkf skf) =<< liftIO (generateKeyPair asType)

writePaymentKeyFiles
  :: Key keyrole
  => VerificationKeyFile
  -> SigningKeyFile
  -> VerificationKey keyrole
  -> SigningKey keyrole
  -> ExceptT ShelleyAddressCmdError IO ()
writePaymentKeyFiles (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) vkey skey = do
  firstExceptT ShelleyAddressCmdWriteFileError $ do
    newExceptT $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    newExceptT $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
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
                -> Maybe StakeVerifier
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
  :: StakeVerifier
  -> ExceptT ShelleyAddressCmdError IO StakeAddressReference
makeStakeAddressRef stakeVerifier = case stakeVerifier of
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

buildShelleyAddress
  :: VerificationKey PaymentKey
  -> Maybe StakeVerifier
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
