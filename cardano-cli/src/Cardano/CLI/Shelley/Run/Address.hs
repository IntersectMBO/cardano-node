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
    AddressBuildStaking payVk stkVk -> runAddressBuildStaking payVk stkVk
    AddressBuildReward stkVk -> runAddressBuildReward stkVk
    AddressBuildEnterprise payVk -> runAddressBuildEnterprise payVk
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


runAddressBuildStaking :: VerificationKeyFile -> VerificationKeyFile -> ExceptT CliError IO ()
runAddressBuildStaking (VerificationKeyFile payVkeyFp) (VerificationKeyFile stkVkeyFp) =
  firstExceptT CardanoApiError $ do
    stkVKey <- newExceptT $ readStakingVerificationKey stkVkeyFp
    payVKey <- newExceptT $ readPaymentVerificationKey payVkeyFp
    let addr = shelleyVerificationKeyAddress payVKey (Just stkVKey)
    liftIO $ Text.putStrLn $ addressToHex addr

runAddressBuildReward :: VerificationKeyFile -> ExceptT CliError IO ()
runAddressBuildReward (VerificationKeyFile stkVkeyFp) =
  firstExceptT CardanoApiError $ do
    stkVKey <- ExceptT $ readStakingVerificationKey stkVkeyFp
    let rwdAddr = AddressShelleyReward $ shelleyVerificationKeyRewardAddress stkVKey
    liftIO . Text.putStrLn $ addressToHex rwdAddr

runAddressBuildEnterprise :: VerificationKeyFile -> ExceptT CliError IO ()
runAddressBuildEnterprise (VerificationKeyFile payVkeyFp) =
  firstExceptT CardanoApiError $ do
    payVKey <- newExceptT $ readPaymentVerificationKey payVkeyFp
    let addr = shelleyVerificationKeyAddress payVKey Nothing
    liftIO $ Text.putStrLn $ addressToHex addr

runAddressBuildMultiSig :: ExceptT CliError IO ()
runAddressBuildMultiSig =
    liftIO $ putStrLn ("runAddressBuildMultiSig: TODO")
