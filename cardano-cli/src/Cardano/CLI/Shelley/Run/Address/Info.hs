{-# LANGUAGE GADTs #-}
module Cardano.CLI.Shelley.Run.Address.Info
  ( runAddressInfo
  , ShelleyAddressInfoError(..)
  ) where

import           Cardano.Prelude

import           Data.Aeson (ToJSON (..), (.=), object)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)

import           Cardano.Api.Typed


data ShelleyAddressInfoError = ShelleyAddressInvalid Text
  deriving Show

instance Error ShelleyAddressInfoError where
  displayError (ShelleyAddressInvalid addrTxt) =
    "Invalid address: " <> show addrTxt

data AddressInfo = AddressInfo
  { aiType :: !Text
  , aiEra :: !Text
  , aiEncoding :: !Text
  , aiAddress :: !Text
  }

instance ToJSON AddressInfo where
  toJSON addrInfo =
    object
      [ "type" .= aiType addrInfo
      , "era" .= aiEra addrInfo
      , "encoding" .= aiEncoding addrInfo
      , "address" .= aiAddress addrInfo
      ]

runAddressInfo :: Text -> ExceptT ShelleyAddressInfoError IO ()
runAddressInfo addrTxt = do
    addrInfo <- case (Left  <$> deserialiseAddress AsShelleyAddress addrTxt)
                 <|> (Right <$> deserialiseAddress AsStakeAddress addrTxt) of

      Nothing ->
        left $ ShelleyAddressInvalid addrTxt

      Just (Left payaddr) ->
        case payaddr of
          ByronAddress{} ->
            pure $ AddressInfo
              { aiType = "payment"
              , aiEra = "byron"
              , aiEncoding = "base58"
              , aiAddress = addrTxt
              }
          ShelleyAddress{} ->
            pure $ AddressInfo
              { aiType = "payment"
              , aiEra = "shelley"
              , aiEncoding = "bech32"
              , aiAddress = addrTxt
              }

      Just (Right _stakeaddr) ->
        pure $ AddressInfo
          { aiType = "stake"
          , aiEra = "shelley"
          , aiEncoding = "bech32"
          , aiAddress = addrTxt
          }
    liftIO $ LBS.putStrLn $ encodePretty addrInfo
