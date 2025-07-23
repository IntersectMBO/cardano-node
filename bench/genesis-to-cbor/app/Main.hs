{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Cardano.Api

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hash.Class as CryptoClass

import           Codec.Serialise (Serialise (..))
import           Control.Exception
import           Data.Aeson as Aeson (eitherDecodeStrict')
import           Data.ByteString as BS (ByteString, readFile)
import           System.Environment (getArgs)
import           System.IO.MMap
import           System.Mem

newtype GenesisHash = GenesisHash (Crypto.Hash Crypto.Blake2b_256 ByteString)
  deriving newtype (Eq, Show)

instance Serialise ShelleyGenesis where
  encode = toCBOR
  decode = fromCBOR

main :: IO ()
main = getArgs >>= \case
    [fileName]  -> do
      {-
      putStrLn $ "--> processing: " ++ fileName
      (genesis, genesisHash) <- readGenesis fileName
      putStrLn $ "--> got hash: " ++ show genesisHash
      let outFileName = fileName ++ ".cbor"
      putStrLn $ "--> writing: " ++ outFileName
      writeFileSerialise outFileName genesis
      -}
      !hash <- hashMmappedFile fileName
      performGC
      print hash
    _ -> putStrLn "usage: genesis-to-cbor <genesis-shelley.json>"


{- from the package docs:
  In case of ForeignPtr or ByteString functions the storage manager is used to free the mapped memory.
  When the garbage collector notices there are no further references to the mapped memory, a call to munmap is made.
  It is not necessary to do this yourself. In tight memory situations it may be profitable to use 
  performGC or finalizeForeignPtr to force an unmap action.
-}
hashMmappedFile :: FilePath -> IO GenesisHash
hashMmappedFile f =
  mmapFileByteString f Nothing >>= evaluate . GenesisHash . CryptoClass.hashWith id

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
