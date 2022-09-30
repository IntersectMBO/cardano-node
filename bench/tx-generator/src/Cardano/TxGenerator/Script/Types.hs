{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-partial-fields -fno-warn-orphans #-}

-- | this module is currently experimental and serves as testing ground
-- for a new script design for the generator.
module Cardano.TxGenerator.Script.Types where

import           GHC.Generics (Generic)

import           Data.Aeson hiding (Key)
import qualified Data.ByteString.Char8 as BS (putStrLn)
import           Data.Text (Text)
import qualified Data.Yaml as Yaml (encode)

import           Cardano.TxGenerator.Types



type TxGenScript = [RootNode]

data RootNode =
    SigningKeys [NamedKey]
  | Wallets
  | Params TxGenTxParams
  | Script
  deriving (Generic)

data NamedKey =
    KeyLiteral  { name :: Text, base16Cbor :: Text }
  | KeyFile     { name :: Text, filePath :: FilePath}
  deriving (Generic)


jsonOptions :: Options
jsonOptions
  = defaultOptions {
      sumEncoding = ObjectWithSingleField
    , constructorTagModifier = camelTo2 '_'
    , omitNothingFields = True
    }

instance ToJSON NamedKey where
    toJSON     = genericToJSON jsonOptions
    toEncoding = genericToEncoding jsonOptions

instance ToJSON RootNode where
    toJSON     = genericToJSON jsonOptions
    toEncoding = genericToEncoding jsonOptions

deriving instance Generic TxGenTxParams

instance ToJSON TxGenTxParams where
    toJSON     = genericToJSON jsonOptions { fieldLabelModifier = drop 7 }
    toEncoding = genericToEncoding jsonOptions { fieldLabelModifier = drop 7 }


testRender :: IO ()
testRender
  = BS.putStrLn $ Yaml.encode testScript
  where
    testScript :: TxGenScript
    testScript
      = [ SigningKeys [
              KeyFile "key1" "foo/bar.skey"
            , KeyFile "key2" "bar/foo.skey"
            , KeyLiteral "key3" "2137d98af79ad87a28"
          ]
        , Params defaultTxGenTxParams
        ]
