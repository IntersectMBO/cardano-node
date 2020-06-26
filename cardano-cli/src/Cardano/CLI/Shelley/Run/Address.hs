module Cardano.CLI.Shelley.Run.Address
  ( ShelleyAddressCmdError
  , renderShelleyAddressCmdError
  , runAddressCmd
  ) where

import           Cardano.Prelude hiding (putStrLn)
import           Prelude (putStrLn)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api
import           Cardano.Api.TextView (TextViewTitle (..))
import qualified Cardano.Api.Typed as Api (NetworkId (..))
import           Cardano.Api.Typed (AsType (..), Error (..), FileError,
                   Key (..), PaymentCredential (..), StakeCredential (..),
                   StakeAddressReference (..), StakeKey, TextEnvelopeError,
                   VerificationKey, generateSigningKey, getVerificationKey,
                   makeShelleyAddress, readFileTextEnvelope,
                   serialiseToRawBytesHex, writeFileTextEnvelope)

import           Cardano.CLI.Shelley.Parsers
                   (OutputFile (..), SigningKeyFile (..), VerificationKeyFile (..),
                    AddressCmd (..))
import           Cardano.CLI.Shelley.Run.Address.Info (ShelleyAddressInfoError, renderShelleyAddressInfoError,
                   runAddressInfo)

data ShelleyAddressCmdError
  = ShelleyAddressCmdAddressInfoError !ShelleyAddressInfoError
  | ShelleyAddressCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyAddressCmdWriteFileError !(FileError ())
  deriving Show

renderShelleyAddressCmdError :: ShelleyAddressCmdError -> Text
renderShelleyAddressCmdError err =
  case err of
    ShelleyAddressCmdAddressInfoError addrInfoErr ->
      "Error occurred while printing address info: " <> renderShelleyAddressInfoError addrInfoErr
    ShelleyAddressCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

runAddressCmd :: AddressCmd -> ExceptT ShelleyAddressCmdError IO ()
runAddressCmd cmd =
  case cmd of
    AddressKeyGen vkf skf -> runAddressKeyGen vkf skf
    AddressKeyHash vkf mOFp -> runAddressKeyHash vkf mOFp
    AddressBuild payVk stkVk nw mOutFp -> runAddressBuild payVk stkVk nw mOutFp
    AddressBuildMultiSig {} -> runAddressBuildMultiSig
    AddressInfo txt -> firstExceptT ShelleyAddressCmdAddressInfoError $ runAddressInfo txt

runAddressKeyGen :: VerificationKeyFile -> SigningKeyFile -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyGen (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsPaymentKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewTitle
    skeyDesc = TextViewTitle "Payment Signing Key"
    vkeyDesc = TextViewTitle "Payment Verification Key"

runAddressKeyHash :: VerificationKeyFile -> Maybe OutputFile -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyHash (VerificationKeyFile vkeyPath) mOutputFp = do
  paymentVerKey <- firstExceptT ShelleyAddressCmdReadFileError
    . newExceptT
    $ readFileTextEnvelope (AsVerificationKey AsPaymentKey) vkeyPath

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash paymentVerKey)

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runAddressBuild :: VerificationKeyFile
                -> Maybe VerificationKeyFile
                -> Network
                -> Maybe OutputFile
                -> ExceptT ShelleyAddressCmdError IO ()
runAddressBuild (VerificationKeyFile payVkeyFp) mstkVkeyFp nw mOutFp = do
    payVKey <- firstExceptT ShelleyAddressCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsPaymentKey) payVkeyFp
    let paymentCred = PaymentCredentialByKey (verificationKeyHash payVKey)

    stakeAddrRef <- firstExceptT ShelleyAddressCmdReadFileError $
      case mstkVkeyFp of
        Just (VerificationKeyFile stkVkeyFp) ->
          toStakeAddrRef
            <$> newExceptT (readFileTextEnvelope (AsVerificationKey AsStakeKey) stkVkeyFp)
        Nothing -> pure NoStakeAddress

    let addr = makeShelleyAddress nwId paymentCred stakeAddrRef
        hexAddr = serialiseToRawBytesHex addr

    case mOutFp of
      Just (OutputFile fpath) -> liftIO . BS.writeFile fpath $ hexAddr
      Nothing -> liftIO $ BS.putStrLn hexAddr
  where
    toStakeAddrRef :: VerificationKey StakeKey -> StakeAddressReference
    toStakeAddrRef = StakeAddressByValue . StakeCredentialByKey . verificationKeyHash

    -- TODO: Remove this once we remove usage of 'Cardano.Api.Types.Network'
    --       from this module.
    nwId :: Api.NetworkId
    nwId =
      case nw of
        Mainnet -> Api.Mainnet
        Testnet nm -> Api.Testnet nm

runAddressBuildMultiSig :: ExceptT ShelleyAddressCmdError IO ()
runAddressBuildMultiSig =
    liftIO $ putStrLn ("runAddressBuildMultiSig: TODO")
