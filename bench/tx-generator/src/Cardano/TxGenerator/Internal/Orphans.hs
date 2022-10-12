{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module  Cardano.TxGenerator.Internal.Orphans
        ()
        where

import           Data.Aeson

import qualified Ouroboros.Network.Magic as Ouroboros (NetworkMagic (..))

import           Cardano.Api (NetworkId (..))
import           Cardano.CLI.Types (SigningKeyFile (..))


instance ToJSON SigningKeyFile where
 toJSON (SigningKeyFile a) = toJSON a

instance FromJSON SigningKeyFile where
  parseJSON a = SigningKeyFile <$> parseJSON a


instance ToJSON NetworkId where
  toJSON Mainnet = "Mainnet"
  toJSON (Testnet (Ouroboros.NetworkMagic t)) = object ["Testnet" .= t]

instance FromJSON NetworkId where
  parseJSON j = case j of
    String "Mainnet" -> pure Mainnet
    Object v         -> v .:? "Testnet" >>= maybe failure (pure . Testnet . Ouroboros.NetworkMagic)
    _                -> failure
    where
      failure = fail $ "could not parse NetworkId: " <> show j
