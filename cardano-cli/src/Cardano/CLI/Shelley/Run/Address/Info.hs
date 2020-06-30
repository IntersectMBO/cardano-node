{-# LANGUAGE GADTs #-}
module Cardano.CLI.Shelley.Run.Address.Info
  ( runAddressInfo
  , ShelleyAddressInfoError(..)
  ) where

import           Cardano.Prelude hiding (putStrLn)
import           Prelude (putStrLn)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)

import           Cardano.Api.Typed


data ShelleyAddressInfoError = ShelleyAddressInvalid Text
  deriving Show

instance Error ShelleyAddressInfoError where
  displayError (ShelleyAddressInvalid addrTxt) =
    "Invalid address: " <> show addrTxt

runAddressInfo :: Text -> ExceptT ShelleyAddressInfoError IO ()
runAddressInfo addrTxt =
    case (Left  <$> deserialiseAddress AsShelleyAddress addrTxt)
     <|> (Right <$> deserialiseAddress AsStakeAddress addrTxt) of

      Nothing ->
        left $ ShelleyAddressInvalid addrTxt

      Just (Left payaddr) -> liftIO $ do
        putStrLn "Type: Payment address"
        case payaddr of
          ByronAddress{}   -> do
            putStrLn "Era: Byron"
            putStrLn "Encoding: Base58"
          ShelleyAddress{} -> do
            putStrLn "Era: Shelley"
            putStrLn "Encoding: Bech32"

      Just (Right _stakeaddr) ->  liftIO $ do
        putStrLn "Type: Stake address"
        putStrLn "Era: Shelley"
        putStrLn "Encoding: Bech32"
