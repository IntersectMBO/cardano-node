{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Cardano.Api

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hash.Class as CryptoClass

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.IO
import           Data.Aeson as Aeson (eitherDecodeStrict')
import           Data.ByteString as BS (ByteString, readFile)
import           System.Environment (getArgs)


newtype GenesisHash = GenesisHash (Crypto.Hash Crypto.Blake2b_256 ByteString)
  deriving newtype (Eq, Show)

instance Serialise ShelleyGenesis where
  encode = toCBOR
  decode = fromCBOR

main :: IO ()
main = getArgs >>= \case
    [fileName]  -> do
      putStrLn $ "--> processing: " ++ fileName
      (genesis, genesisHash) <- readGenesis fileName
      putStrLn $ "--> got hash: " ++ show genesisHash
      let outFileName = fileName ++ ".cbor"
      putStrLn $ "--> writing: " ++ outFileName
      writeFileSerialise outFileName genesis
    _ -> putStrLn "usage: genesis-to-cbor <genesis-shelley.json>"

readGenesis :: FilePath
            -> IO (ShelleyGenesis, GenesisHash)
readGenesis = readGenesisAny

readGenesisAny :: FromJSON genesis
               => FilePath
               -> IO (genesis, GenesisHash)
readGenesisAny file = do
    content <- BS.readFile file
    let genesisHash = GenesisHash $ CryptoClass.hashWith id content

    case Aeson.eitherDecodeStrict' content of
      Left err      -> fail err
      Right genesis -> pure (genesis, genesisHash)
