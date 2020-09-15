{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Shelley.Run.Address
  ( ShelleyAddressCmdError
  , renderShelleyAddressCmdError
  , runAddressCmd
  ) where

import           Cardano.Prelude hiding (putStrLn)
import           Prelude (String)

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, left, newExceptT)

import           Cardano.Api.TextView (TextViewDescription (..))
import           Cardano.Api.Typed

import           Cardano.CLI.Shelley.Key (InputDecodeError, VerificationKeyOrFile,
                     VerificationKeyTextOrFile, VerificationKeyTextOrFileError (..),
                     readVerificationKeyOrFile, readVerificationKeyTextOrFileAnyOf,
                     renderVerificationKeyTextOrFileError)
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
    AddressBuild payVk stkVk nw mOutFp -> runAddressBuild payVk stkVk nw mOutFp
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

    skeyDesc, vkeyDesc :: TextViewDescription
    skeyDesc = TextViewDescription "Payment Signing Key"
    vkeyDesc = TextViewDescription "Payment Verification Key"


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


runAddressBuild :: VerificationKeyTextOrFile
                -> Maybe (VerificationKeyOrFile StakeKey)
                -> NetworkId
                -> Maybe OutputFile
                -> ExceptT ShelleyAddressCmdError IO ()
runAddressBuild payVkeyTextOrFile mbStkVkeyOrFile nw mOutFp = do
    payVKey <- firstExceptT ShelleyAddressCmdVerificationKeyTextOrFileError $
                 readAddressVerificationKeyTextOrFile payVkeyTextOrFile

    addr <- case payVKey of
              AByronVerificationKey vk ->
                return (makeByronAddress nw vk)

              APaymentVerificationKey vk ->
                buildShelleyAddress vk

              APaymentExtendedVerificationKey vk ->
                buildShelleyAddress (castVerificationKey vk)

              AGenesisUTxOVerificationKey vk ->
                buildShelleyAddress (castVerificationKey vk)

    let addrText = serialiseAddress addr

    case mOutFp of
      Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath addrText
      Nothing                 -> liftIO $ Text.putStrLn        addrText

  where
    buildShelleyAddress vkey = do
      mstakeVKey <-
        case mbStkVkeyOrFile of
          Nothing -> pure Nothing
          Just stkVkeyOrFile ->
            firstExceptT ShelleyAddressCmdReadKeyFileError $
              fmap Just $ newExceptT $
                readVerificationKeyOrFile AsStakeKey stkVkeyOrFile

      let paymentCred  = PaymentCredentialByKey (verificationKeyHash vkey)
          stakeAddrRef = maybe NoStakeAddress
                               (StakeAddressByValue . StakeCredentialByKey
                                                    . verificationKeyHash)
                               mstakeVKey
          address      = makeShelleyAddress nw paymentCred stakeAddrRef

      return address


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
runAddressBuildScript (ScriptFile fp) nId mOutFp = do
  scriptLB <- handleIOExceptT (ShelleyAddressCmdReadFileException . FileIOError fp)
                $ LB.readFile fp
  script <- case eitherDecode scriptLB :: Either String MultiSigScript of
               Right mss -> return $ makeMultiSigScript mss
               Left err -> left . ShelleyAddressCmdAesonDecodeError fp $ Text.pack err
  let payCred = PaymentCredentialByScript $ scriptHash script
      scriptAddr = serialiseAddress $ makeShelleyAddress nId payCred NoStakeAddress
  case mOutFp of
    Just (OutputFile oFp) -> liftIO $ Text.writeFile oFp scriptAddr
    Nothing -> liftIO $ Text.putStr scriptAddr
