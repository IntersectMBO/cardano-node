{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Shelley.Run.Address
  ( ShelleyAddressCmdError,
    renderShelleyAddressCmdError,
    runAddressCmd,
  )
where

import Cardano.Api.TextView (TextViewDescription (..))
import Cardano.Api.Typed
import Cardano.CLI.Shelley.Parsers
  ( AddressCmd (..),
    AddressKeyType (..),
    OutputFile (..),
    SigningKeyFile (..),
    VerificationKeyFile (..),
  )
import Cardano.CLI.Shelley.Run.Address.Info
  ( ShelleyAddressInfoError,
    runAddressInfo,
  )
import Cardano.Prelude hiding (putStrLn)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Prelude (putStrLn)

data ShelleyAddressCmdError
  = ShelleyAddressCmdAddressInfoError !ShelleyAddressInfoError
  | ShelleyAddressCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyAddressCmdWriteFileError !(FileError ())
  deriving (Show)

renderShelleyAddressCmdError :: ShelleyAddressCmdError -> Text
renderShelleyAddressCmdError err =
  case err of
    ShelleyAddressCmdAddressInfoError addrInfoErr ->
      Text.pack (displayError addrInfoErr)
    ShelleyAddressCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

runAddressCmd :: AddressCmd -> ExceptT ShelleyAddressCmdError IO ()
runAddressCmd cmd =
  case cmd of
    AddressKeyGen kt vkf skf -> runAddressKeyGen kt vkf skf
    AddressKeyHash vkf mOFp -> runAddressKeyHash vkf mOFp
    AddressBuild payVk stkVk nw mOutFp -> runAddressBuild payVk stkVk nw mOutFp
    AddressBuildMultiSig {} -> runAddressBuildMultiSig
    AddressInfo txt mOFp -> firstExceptT ShelleyAddressCmdAddressInfoError $ runAddressInfo txt mOFp

runAddressKeyGen ::
  AddressKeyType ->
  VerificationKeyFile ->
  SigningKeyFile ->
  ExceptT ShelleyAddressCmdError IO ()
runAddressKeyGen kt (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
  case kt of
    AddressKeyShelley -> generateAndWriteKeyFiles AsPaymentKey
    AddressKeyShelleyExtended -> generateAndWriteKeyFiles AsPaymentExtendedKey
    AddressKeyByron -> generateAndWriteKeyFiles AsByronKey
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

runAddressKeyHash ::
  VerificationKeyFile ->
  Maybe OutputFile ->
  ExceptT ShelleyAddressCmdError IO ()
runAddressKeyHash vkeyPath mOutputFp = do
  vkey <-
    firstExceptT ShelleyAddressCmdReadFileError $
      readAddressVerificationKeyFile vkeyPath

  let hexKeyHash =
        foldSomeAddressVerificationKey
          (serialiseToRawBytesHex . verificationKeyHash)
          vkey

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runAddressBuild ::
  VerificationKeyFile ->
  Maybe VerificationKeyFile ->
  NetworkId ->
  Maybe OutputFile ->
  ExceptT ShelleyAddressCmdError IO ()
runAddressBuild payVkeyFp mstkVkeyFp nw mOutFp = do
  payVKey <-
    firstExceptT ShelleyAddressCmdReadFileError $
      readAddressVerificationKeyFile payVkeyFp

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
    Nothing -> liftIO $ Text.putStrLn addrText
  where
    buildShelleyAddress vkey = do
      mstakeVKey <-
        case mstkVkeyFp of
          Nothing -> pure Nothing
          Just (VerificationKeyFile stkVkeyFp) ->
            firstExceptT ShelleyAddressCmdReadFileError $
              fmap Just $
                newExceptT $
                  readFileTextEnvelope (AsVerificationKey AsStakeKey) stkVkeyFp

      let paymentCred = PaymentCredentialByKey (verificationKeyHash vkey)
          stakeAddrRef =
            maybe
              NoStakeAddress
              ( StakeAddressByValue . StakeCredentialByKey
                  . verificationKeyHash
              )
              mstakeVKey
          address = makeShelleyAddress nw paymentCred stakeAddrRef

      return address

--
-- Handling the variety of address key types
--

-- TODO: if we could make unions like this an instance of the Key class then
-- it would simplify some of the code above
data SomeAddressVerificationKey
  = AByronVerificationKey (VerificationKey ByronKey)
  | APaymentVerificationKey (VerificationKey PaymentKey)
  | APaymentExtendedVerificationKey (VerificationKey PaymentExtendedKey)
  | AGenesisUTxOVerificationKey (VerificationKey GenesisUTxOKey)

foldSomeAddressVerificationKey ::
  ( forall keyrole.
    Key keyrole =>
    VerificationKey keyrole ->
    a
  ) ->
  SomeAddressVerificationKey ->
  a
foldSomeAddressVerificationKey f (AByronVerificationKey vk) = f vk
foldSomeAddressVerificationKey f (APaymentVerificationKey vk) = f vk
foldSomeAddressVerificationKey f (APaymentExtendedVerificationKey vk) = f vk
foldSomeAddressVerificationKey f (AGenesisUTxOVerificationKey vk) = f vk

readAddressVerificationKeyFile ::
  VerificationKeyFile ->
  ExceptT (FileError TextEnvelopeError) IO SomeAddressVerificationKey
readAddressVerificationKeyFile (VerificationKeyFile vkfile) =
  newExceptT $
    readFileTextEnvelopeAnyOf fileTypes vkfile
  where
    fileTypes =
      [ FromSomeType
          (AsVerificationKey AsByronKey)
          AByronVerificationKey,
        FromSomeType
          (AsVerificationKey AsPaymentKey)
          APaymentVerificationKey,
        FromSomeType
          (AsVerificationKey AsPaymentExtendedKey)
          APaymentExtendedVerificationKey,
        FromSomeType
          (AsVerificationKey AsGenesisUTxOKey)
          AGenesisUTxOVerificationKey
      ]

--
-- Multisig addresses
--

runAddressBuildMultiSig :: ExceptT ShelleyAddressCmdError IO ()
runAddressBuildMultiSig =
  liftIO $ putStrLn ("runAddressBuildMultiSig: TODO")
