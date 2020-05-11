{-# LANGUAGE MultiWayIf #-}
module Cardano.CLI.Shelley.Run.Address.Info
  ( runAddressInfo
  ) where

import           Cardano.Prelude hiding (putStrLn)
import           Prelude (putStrLn)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)

import           Cardano.Api
import           Cardano.CLI.Ops (CliError (..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Base16 as Base16
import           Data.Char (isHexDigit)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text



runAddressInfo :: Text -> ExceptT CliError IO ()
runAddressInfo addrTxt = do
  liftIO $ Text.putStrLn $ "Address: " <> addrTxt
  if
    | Text.all isHexDigit addrTxt -> do
        liftIO $ putStrLn "Encoding: Hex"
        runAddressInfoHex addrTxt
    | Text.all isBase58Char addrTxt -> do
        liftIO $ putStrLn "Encoding: Base58"
        runAddressInfoBase58 addrTxt
    | otherwise -> left $ AddressDescribeError ("Unknown address type: " <> addrTxt)
  where
    isBase58Char :: Char -> Bool
    isBase58Char c = c `elem` BS.unpack (Base58.unAlphabet Base58.bitcoinAlphabet)

-- -------------------------------------------------------------------------------------------------

runAddressInfoHex :: Text -> ExceptT CliError IO ()
runAddressInfoHex addrTxt = do
  case addressFromHex addrTxt of
    Just addr -> describeAddr addr
    Nothing -> left $ AddressDescribeError "Failed Base16 decode. Impossible!"

runAddressInfoBase58 :: Text -> ExceptT CliError IO ()
runAddressInfoBase58 addrTxt = do
  case Base16.encode <$> Base58.decodeBase58 Base58.bitcoinAlphabet (Text.encodeUtf8 addrTxt) of
    Just hex -> do
      runAddressInfoHex $ Text.decodeUtf8 hex
      liftIO $ BS.putStrLn ("Hex: " <> hex)
    Nothing -> left $ AddressDescribeError "Failed Base58 decode. Impossible!"


describeAddr :: Address -> ExceptT CliError IO ()
describeAddr addr =
  case addr of
    AddressByron {} -> liftIO $ putStrLn "Era: Byron"
    AddressShelley {} -> liftIO $ putStrLn "Era: Shelley"
    AddressShelleyReward {} -> liftIO $ putStrLn "Era: Shelley"
