{-# LANGUAGE GADTs #-}
module Cardano.CLI.Shelley.Run.Address.Info
  ( runAddressInfo
  , ShelleyAddressInfoError(..)
  ) where

import           Cardano.Api.Typed
import           Cardano.CLI.Shelley.Parsers (OutputFile (..))
import           Cardano.Prelude
import           Control.Monad.Trans.Except.Extra (left)
import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as Text

newtype ShelleyAddressInfoError = ShelleyAddressInvalid Text
  deriving Show

instance Error ShelleyAddressInfoError where
  displayError (ShelleyAddressInvalid addrTxt) =
    "Invalid address: " <> show addrTxt

data AddressInfo = AddressInfo
  { aiType :: !Text
  , aiEra :: !Text
  , aiEncoding :: !Text
  , aiAddress :: !Text
  , aiBase16 :: !Text
  }

instance ToJSON AddressInfo where
  toJSON addrInfo =
    object
      [ "type" .= aiType addrInfo
      , "era" .= aiEra addrInfo
      , "encoding" .= aiEncoding addrInfo
      , "address" .= aiAddress addrInfo
      , "base16" .= aiBase16 addrInfo
      ]

runAddressInfo :: Text -> Maybe OutputFile -> ExceptT ShelleyAddressInfoError IO ()
runAddressInfo addrTxt mOutputFp = do
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
              , aiBase16 = asBase16 payaddr
              }
          ShelleyAddress{} ->
            pure $ AddressInfo
              { aiType = "payment"
              , aiEra = "shelley"
              , aiEncoding = "bech32"
              , aiAddress = addrTxt
              , aiBase16 = asBase16 payaddr
              }

      Just (Right addr) ->
        pure $ AddressInfo
          { aiType = "stake"
          , aiEra = "shelley"
          , aiEncoding = "bech32"
          , aiAddress = addrTxt
          , aiBase16 = asBase16 addr
          }

    case mOutputFp of
      Just (OutputFile fpath) -> liftIO $ LBS.writeFile fpath $ encodePretty addrInfo
      Nothing -> liftIO $ LBS.putStrLn $ encodePretty addrInfo

  where
    asBase16 :: SerialiseAsRawBytes a => a -> Text
    asBase16 = Text.decodeUtf8 . serialiseToRawBytesHex
