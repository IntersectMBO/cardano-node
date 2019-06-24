{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Run (
      runCLI
    ) where

import           Prelude (error)

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Codec.Serialise as Serialise (encode, decode)
import           Control.Exception
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad
import           Control.Tracer
import           Crypto.Random
import           Data.Functor.Contravariant (contramap)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup ((<>))
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import           System.IO.Error (isDoesNotExistError)
import           System.Directory (removeFile)

import qualified Data.Aeson.Encode.Pretty as AE
  (Config(..), Indent(..), NumberFormat(..), encodePretty', keyOrder)

import qualified Text.JSON.Canonical as CanonicalJSON

import           Cardano.Prelude hiding (option)

import           Cardano.Crypto.Random (runSecureRandom)
import           Cardano.Crypto.Signing
  (keyGen, SigningKey(..), fromCBORXPrv, toCBORXPrv, toVerification)

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (Trace, appendName)

import           Cardano.Chain.Genesis
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

import           CLI

-- Stolen from: cardano-prelude/test/Test/Cardano/Prelude/Tripping.hs
canonicalEncPre
  :: forall a . CanonicalJSON.ToJSON Identity a => a -> LB.ByteString
canonicalEncPre x =
  LB.fromStrict
    . encodeUtf8
    . toS
    $ CanonicalJSON.prettyCanonicalJSON
    $ runIdentity
    $ CanonicalJSON.toJSON x

-- Stolen from: cardano-prelude/test/Test/Cardano/Prelude/Tripping.hs
canonicalDecPre
  :: forall a
   . CanonicalJSON.FromJSON (Either SchemaError) a
  => LB.ByteString
  -> Either Text a
canonicalDecPre bs = do
  eVal <- first toS (CanonicalJSON.parseCanonicalJSON bs)
  first show (CanonicalJSON.fromJSON eVal :: Either SchemaError a)

runCLI :: CLI -> IO ()
runCLI cli@CLI{..} = do
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of
      DummyGenesis out -> do
        putStrLn ("Dummy genesis requested" :: Text)
        let genesisDataJSON = canonicalEncPre Dummy.dummyGenesisData
        -- genesisDataJSON <- toJSON dummy

        putStrLn $ genesisDataJSON

      RTByronGenesis input -> do
        bs <- LB.readFile input
        let gd :: Either Text GenesisData = canonicalDecPre bs
            genesisDataJSON = case gd of
                                Left err -> error $ T.unpack err
                                Right x  -> canonicalEncPre x
        putStrLn $ genesisDataJSON

      FullByronGenesis -> do
        putStrLn ("Full Byron genesis requested -- not implemented yet." :: Text)

      KeyGen sigPath vrfPath -> do
        (vrf, SigningKey sig) <- runSecureRandom $ keyGen
        -- Write private key as raw CBOR, public as JSON.
        LB.writeFile (fromVerificationPath vrfPath) $ canonicalEncPre vrfBs
        LB.writeFile (fromSigningPath      sigPath) $ toLazyByteString $ toCBORXPrv sig
