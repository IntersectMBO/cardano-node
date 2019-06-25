{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
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

import           Prelude (error, id)

import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.ByteString.Lazy as LB
import           System.Posix.Files (ownerReadMode, setFileMode)
import           System.Directory (createDirectory, doesPathExist)

import qualified Text.JSON.Canonical as CanonicalJSON

import           Cardano.Prelude hiding (option)

import           Cardano.Crypto.Random (runSecureRandom)
import           Cardano.Crypto.Signing
  (keyGen, SigningKey(..), toCBORXPrv)

import           Cardano.Chain.Genesis

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
runCLI CLI{..} = do
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of
      KeyGen sigPath vrfPath -> do
        (vrf, SigningKey sig) <- runSecureRandom $ keyGen
        -- Write private key as raw CBOR, public as JSON.
        LB.writeFile (fromVerificationPath vrfPath) $ canonicalEncPre vrf
        LB.writeFile (fromSigningPath      sigPath) $ toLazyByteString $ toCBORXPrv sig

      FullByronGenesis
        outDir
        startTime
        protocolParametersFile
        blockCount
        protocolMagic
        giTestBalance
        giFakeAvvmBalance
        giAvvmBalanceFactor
        giSeed
        -> do

        exists <- doesPathExist outDir
        if exists
          then error $ "Genesis output directory must not already exist: " <> outDir
          else createDirectory outDir

        protoParamsRaw <- LB.readFile protocolParametersFile
        let protocolParameters = either (error . show) id $ canonicalDecPre protoParamsRaw

        -- We're relying on the generator to fake AVVM and delegation.
        mGenesisDelegation <- runExceptT $ mkGenesisDelegation []
        let genesisDelegation   = either (error . show) id mGenesisDelegation
            genesisAvvmBalances = GenesisAvvmBalances mempty

        let mGenesisSpec =
              mkGenesisSpec
              genesisAvvmBalances -- :: !GenesisAvvmBalances
              genesisDelegation   -- :: !GenesisDelegation
              protocolParameters  -- :: !ProtocolParameters
              blockCount          -- :: !BlockCount
              protocolMagic       -- :: !ProtocolMagic
              genesisInitializer  -- :: !GenesisInitializer
            genesisInitializer =
              GenesisInitializer
              giTestBalance       -- :: !TestnetBalanceOptions
              giFakeAvvmBalance   -- :: !FakeAvvmOptions
              giAvvmBalanceFactor -- :: !LovelacePortion
              giUseHeavyDlg       -- :: !Bool
              giSeed              -- :: !Integer
            giUseHeavyDlg =
              True

        let genesisSpec = either (error . show) id mGenesisSpec

        res <- runExceptT $ generateGenesisData startTime genesisSpec
        let (gd, GeneratedSecrets{..}) = either (error . show) id res

        let outJSONFile = outDir <> "/genesis.json"
            serialiseSigningKey (SigningKey x) = toLazyByteString $ toCBORXPrv x
            writeSecrets :: FilePath -> [a] -> (a -> LB.ByteString) -> IO ()
            writeSecrets prefix xs f =
              forM_ (zip xs $ [0::Int ..]) $
              \(key, nr)-> do
                let filename = outDir <> "/" <> prefix <> "." <> show nr <> ".key"
                LB.writeFile   filename (f key)
                setFileMode filename ownerReadMode
        LB.writeFile outJSONFile $ canonicalEncPre gd
        writeSecrets "dlg-issuer"  gsDlgIssuersSecrets $ serialiseSigningKey
        writeSecrets "rich-secret" gsRichSecrets       $ serialiseSigningKey
        writeSecrets "poor-secret" gsPoorSecrets       $ serialiseSigningKey . poorSecretToKey
        writeSecrets "avvm-seed"   gsFakeAvvmSeeds     $ LB.fromStrict
