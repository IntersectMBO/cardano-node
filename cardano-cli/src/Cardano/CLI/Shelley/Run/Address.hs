module Cardano.CLI.Shelley.Run.Address
  ( ShelleyAddressCmdError
  , renderShelleyAddressCmdError
  , runAddressCmd
  ) where

import           Cardano.Prelude hiding (putStrLn)
import           Prelude (putStrLn)

import qualified Data.ByteString.Char8 as BS

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT, right)

import           Cardano.Api

import qualified Cardano.Api.Typed as Typed

import           Cardano.CLI.Shelley.Parsers
                   (OutputFile (..), SigningKeyFile (..), VerificationKeyFile (..),
                    AddressCmd (..))
import           Cardano.CLI.Shelley.Run.Address.Info (ShelleyAddressInfoError, renderShelleyAddressInfoError,
                   runAddressInfo)

import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Shelley.Spec.Ledger.Keys as Ledger

data ShelleyAddressCmdError
  = ShelleyAddressCmdAddressInfoError !ShelleyAddressInfoError
  | ShelleyAddressCmdPaySigningKeyWriteErr !ApiError
  | ShelleyAddressCmdPayVerificationKeyReadErrr !ApiError
  | ShelleyAddressCmdPayVerificationKeyWriteErr !ApiError
  deriving Show

renderShelleyAddressCmdError :: ShelleyAddressCmdError -> Text
renderShelleyAddressCmdError err =
  case err of
    ShelleyAddressCmdAddressInfoError addrInfoErr ->
      "Error occurred while printing address info: " <> renderShelleyAddressInfoError addrInfoErr
    ShelleyAddressCmdPaySigningKeyWriteErr apiErr ->
      "Error occured while writing the payment signing key: " <> renderApiError apiErr
    ShelleyAddressCmdPayVerificationKeyReadErrr apiErr ->
      "Error occured while reading the payment verification key: " <> renderApiError apiErr
    ShelleyAddressCmdPayVerificationKeyWriteErr apiErr ->
      "Error occured while writing the payment verification key: " <> renderApiError apiErr

runAddressCmd :: AddressCmd -> ExceptT ShelleyAddressCmdError IO ()
runAddressCmd cmd =
  case cmd of
    AddressKeyGen vkf skf -> runAddressKeyGen  vkf skf
    AddressKeyHash vkf mOFp -> runAddressKeyHash vkf mOFp
    AddressBuild payVk stkVk nw mOutFp -> runAddressBuild payVk stkVk nw mOutFp
    AddressBuildMultiSig {} -> runAddressBuildMultiSig
    AddressInfo txt -> firstExceptT ShelleyAddressCmdAddressInfoError $ runAddressInfo txt

runAddressKeyGen :: VerificationKeyFile -> SigningKeyFile -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyGen (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    sk <- liftIO $ Typed.generateSigningKey Typed.AsPaymentKey
    let vk = getPaymentVerificationKey' sk :: Typed.VerificationKey Typed.PaymentKey

    firstExceptT ShelleyAddressCmdPayVerificationKeyWriteErr
      . ExceptT $ writeVerificationKey' vkeyPath vk
    firstExceptT ShelleyAddressCmdPaySigningKeyWriteErr
      . ExceptT $ writeSigningKey' skeyPath sk

runAddressKeyHash :: VerificationKeyFile -> Maybe OutputFile -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyHash (VerificationKeyFile vkeyPath) mOutputFp =
    firstExceptT ShelleyAddressCmdPayVerificationKeyReadErrr $ do
      PaymentVerificationKeyShelley vkey <- ExceptT $ readPaymentVerificationKey vkeyPath
      let Ledger.KeyHash khash = Ledger.hashKey vkey
          b16Hash = Crypto.getHashBytesAsHex khash
      case mOutputFp of
        Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath b16Hash
        Nothing -> liftIO $ BS.putStrLn b16Hash

{-
makeShelleyAddress :: NetworkId
                   -> PaymentCredential
                   -> StakeAddressReference
                   -> Address Shelley
-}

runAddressBuild :: VerificationKeyFile
                -> Maybe VerificationKeyFile
                -> Typed.NetworkId
                -> Maybe OutputFile
                -> ExceptT ShelleyAddressCmdError IO ()
runAddressBuild (VerificationKeyFile payVkeyFp) mstkVkeyFp nwId mOutFp =
  firstExceptT ShelleyAddressCmdPayVerificationKeyReadErrr $ do
    payVKey <- newExceptT $ readVerificationKey' (Typed.AsVerificationKey Typed.AsPaymentKey) payVkeyFp
    let paymentCred = Typed.PaymentCredentialByKey $ Typed.verificationKeyHash payVKey

    stkAddrRef <- case mstkVkeyFp of
                    Just (VerificationKeyFile stkVkeyFp) -> do
                      stkKey <- newExceptT $ readVerificationKey' (Typed.AsVerificationKey Typed.AsStakeKey) stkVkeyFp
                      right . Typed.StakeAddressByValue . Typed.StakeCredentialByKey $ Typed.verificationKeyHash stkKey
                    Nothing ->
                      right Typed.NoStakeAddress

    let addr = Typed.makeShelleyAddress nwId paymentCred stkAddrRef
        hexAddr = Typed.serialiseToRawBytesHex addr

    case mOutFp of
      Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexAddr
      Nothing -> liftIO . putTextLn $ textShow hexAddr

runAddressBuildMultiSig :: ExceptT ShelleyAddressCmdError IO ()
runAddressBuildMultiSig =
    liftIO $ putStrLn ("runAddressBuildMultiSig: TODO")
