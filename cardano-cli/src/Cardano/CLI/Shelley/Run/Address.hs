module Cardano.CLI.Shelley.Run.Address
  ( runAddressCmd
  ) where

import           Cardano.Prelude hiding (putStrLn)
import           Prelude (putStrLn)

import qualified Data.ByteString.Char8 as BS

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.Api

import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers
                   (SigningKeyFile (..), VerificationKeyFile (..),
                    AddressCmd(..))

import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Shelley.Spec.Ledger.Keys as Ledger
--TODO: provide address rendering from the main API
--      we should not need to import this directly:
import           Shelley.Spec.Ledger.Address (serialiseAddr)
import qualified Data.ByteString.Base16 as B16


runAddressCmd :: AddressCmd -> ExceptT CliError IO ()
runAddressCmd (AddressKeyGen  vkf skf) = runAddressKeyGen  vkf skf
runAddressCmd (AddressKeyHash vkf)     = runAddressKeyHash vkf
runAddressCmd (AddressBuild   vkf)     = runAddressBuild   vkf
runAddressCmd AddressBuildMultiSig{}   = runAddressBuildMultiSig


runAddressKeyGen :: VerificationKeyFile -> SigningKeyFile -> ExceptT CliError IO ()
runAddressKeyGen (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    sk <- liftIO shelleyGenSigningKey
    let vk = getVerificationKey sk
    firstExceptT CardanoApiError $ do
      ExceptT $ writeVerificationKey vkeyPath vk
      ExceptT $ writeSigningKey skeyPath sk

runAddressKeyHash :: VerificationKeyFile -> ExceptT CliError IO ()
runAddressKeyHash (VerificationKeyFile vkeyPath) =
    firstExceptT CardanoApiError $ do
      VerificationKeyShelley vkey <- ExceptT $ readVerificationKey vkeyPath
      let Ledger.KeyHash khash = Ledger.hashKey vkey
      liftIO $ BS.putStrLn $ Crypto.getHashBytesAsHex khash


runAddressBuild :: VerificationKeyFile -> ExceptT CliError IO ()
runAddressBuild (VerificationKeyFile vkeyPath) =
    firstExceptT CardanoApiError $ do
      vkey <- ExceptT $ readVerificationKey vkeyPath
      let AddressShelley addr = shelleyVerificationKeyAddress vkey Mainnet
      --TODO: need address serialisation provided by the API
      liftIO $ BS.putStrLn $ B16.encode $ serialiseAddr addr


runAddressBuildMultiSig :: ExceptT CliError IO ()
runAddressBuildMultiSig =
    liftIO $ putStrLn ("runAddressBuildMultiSig: TODO")
