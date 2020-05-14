module Cardano.CLI.Shelley.Run.Address
  ( runAddressCmd
  ) where

import           Cardano.Prelude hiding (putStrLn)
import           Prelude (putStrLn)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api

import           Cardano.CLI.Errors (CliError(..))
import           Cardano.CLI.Shelley.Parsers
                   (SigningKeyFile (..), VerificationKeyFile (..),
                    AddressCmd(..))
import           Cardano.CLI.Shelley.Run.Address.Info (runAddressInfo)

import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Shelley.Spec.Ledger.Keys as Ledger


runAddressCmd :: AddressCmd -> ExceptT CliError IO ()
runAddressCmd cmd =
  case cmd of
    AddressKeyGen vkf skf -> runAddressKeyGen  vkf skf
    AddressKeyHash vkf -> runAddressKeyHash vkf
    AddressBuild payVk stkVk -> runAddressBuild payVk stkVk
    AddressBuildMultiSig {} -> runAddressBuildMultiSig
    AddressInfo txt -> runAddressInfo txt

runAddressKeyGen :: VerificationKeyFile -> SigningKeyFile -> ExceptT CliError IO ()
runAddressKeyGen (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    sk <- liftIO shelleyGenSigningKey
    let vk = getPaymentVerificationKey sk
    firstExceptT CardanoApiError $ do
      ExceptT $ writePaymentVerificationKey vkeyPath vk
      ExceptT $ writeSigningKey skeyPath sk

runAddressKeyHash :: VerificationKeyFile -> ExceptT CliError IO ()
runAddressKeyHash (VerificationKeyFile vkeyPath) =
    firstExceptT CardanoApiError $ do
      PaymentVerificationKeyShelley vkey <- ExceptT $ readPaymentVerificationKey vkeyPath
      let Ledger.KeyHash khash = Ledger.hashKey vkey
      liftIO $ BS.putStrLn $ Crypto.getHashBytesAsHex khash

runAddressBuild :: VerificationKeyFile
                -> Maybe VerificationKeyFile
                -> ExceptT CliError IO ()
runAddressBuild (VerificationKeyFile payVkeyFp) mstkVkeyFp =
  firstExceptT CardanoApiError $ do
    payVKey <- newExceptT $ readPaymentVerificationKey payVkeyFp
    mstkVKey <- case mstkVkeyFp of
                  Just (VerificationKeyFile stkVkeyFp) ->
                    Just <$> newExceptT (readStakingVerificationKey stkVkeyFp)
                  Nothing ->
                    return Nothing
    let addr = shelleyVerificationKeyAddress payVKey mstkVKey
    liftIO $ Text.putStrLn $ addressToHex addr

runAddressBuildMultiSig :: ExceptT CliError IO ()
runAddressBuildMultiSig =
    liftIO $ putStrLn ("runAddressBuildMultiSig: TODO")
