{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Run (
    decideKeyMaterialOps
  , runCommand
  ) where

import           Prelude (String, error, id)

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Formatting as F
import           System.Directory (createDirectory, doesPathExist)
import           System.FilePath ((</>))
import           Text.Printf (printf)
#ifdef UNIX
import           System.Posix.Files (ownerReadMode, setFileMode)
#else
import           System.Directory (emptyPermissions, readable, setPermissions)
#endif

import qualified Crypto.SCRAPE as Scrape

import           Cardano.Prelude hiding (option)

import qualified Cardano.Chain.Common as CC
import           Cardano.Chain.Delegation (Certificate, ACertificate (..))
import           Cardano.Crypto (SigningKey (..))
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy
import qualified Cardano.Crypto.Random as CCr
import qualified Cardano.Crypto.Hashing as CCr
import qualified Cardano.Crypto.Signing as CCr

import           Cardano.Chain.Genesis

import           Cardano.Node.CanonicalJSON

import qualified Byron.Legacy as Legacy
import           CLI

runCommand :: KeyMaterialOps IO -> Command -> IO ()
runCommand kmo@KeyMaterialOps{..}
           (Genesis
             outDir
             startTime
             protocolParametersFile
             blockCount
             protocolMagic
             giTestBalance
             giFakeAvvmBalance
             giAvvmBalanceFactor
             giSeed) = do
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
          True                -- Not using delegate keys unsupported.

    let genesisSpec = either (error . show) id mGenesisSpec

    -- Generate (mostly)
    res <- runExceptT $ generateGenesisData startTime genesisSpec
    let (genesisData, generatedSecrets) = either (error . show) id res

    dumpGenesis kmo outDir genesisData generatedSecrets

runCommand KeyMaterialOps{..} (PrettySigningKeyPublic secretPath) =
    putStrLn =<< T.unpack
               . prettySigningKeyPub
               . kmoDeserialiseDelegateKey
             <$> LB.readFile secretPath

runCommand kmo (MigrateDelegateKeyFrom
                  fromVer
                  secretPathTo
                  secretPathFrom) =
        LB.writeFile secretPathTo
    =<< kmoSerialiseDelegateKey kmo
      . kmoDeserialiseDelegateKey fromKMO
    =<< LB.readFile secretPathFrom
  where
    fromKMO = decideKeyMaterialOps fromVer

runCommand kmo (DumpHardcodedGenesis outDir) =
    dumpGenesis kmo outDir
                (configGenesisData Dummy.dummyConfig)
                generatedSecrets
  where
    Just generatedSecrets = configGeneratedSecrets Dummy.dummyConfig

runCommand KeyMaterialOps{..} (PrintGenesisHash secretPath) =
    putStrLn . F.format CCr.hashHexF
             . unGenesisHash
             . snd . either (error . show) id
           =<< runExceptT (readGenesisData secretPath)

runCommand KeyMaterialOps{..} (PrintSigningKeyAddress networkMagic secretPath) =
    putStrLn . T.unpack . prettyAddress
             . CC.makeVerKeyAddress networkMagic
             . CCr.toVerification
             . kmoDeserialiseDelegateKey
           =<< LB.readFile secretPath

{-------------------------------------------------------------------------------
  Supporting functions
-------------------------------------------------------------------------------}

dumpGenesis :: KeyMaterialOps IO -> FilePath -> GenesisData -> GeneratedSecrets -> IO ()
dumpGenesis KeyMaterialOps{..} outDir genesisData GeneratedSecrets{..} = do
    exists <- doesPathExist outDir
    if exists
      then error $ "Genesis output directory must not already exist: " <> outDir
      else createDirectory outDir

    let genesisJSONFile = outDir <> "/genesis.json"
    LB.writeFile genesisJSONFile =<< kmoSerialiseGenesis genesisData

    let dlgCertMap = unGenesisDelegation $ gdHeavyDelegation genesisData
        isCertForSK :: SigningKey -> Certificate -> Bool
        isCertForSK sk UnsafeACertificate{..} = delegateVK == CCr.toVerification sk
        findDelegateCert :: SigningKey -> Certificate
        findDelegateCert sk =
          fromMaybe (error . T.unpack $ "Invariant failed: no delegation for key in genesis:\n"<> prettySigningKeyPub sk)
          . flip find (Map.elems dlgCertMap) . isCertForSK $ sk

    writeSecrets outDir "genesis-keys"       "key"  kmoSerialiseGenesisKey        gsDlgIssuersSecrets
    writeSecrets outDir "delegate-keys"      "key"  kmoSerialiseDelegateKey       gsRichSecrets
    writeSecrets outDir "poor-keys"          "key"  kmoSerialisePoorKey           gsPoorSecrets
    writeSecrets outDir "delegation-cert"    "json" kmoSerialiseDelegationCert    (findDelegateCert <$> gsRichSecrets)
    writeSecrets outDir "avvm-seed"          "seed" (pure . LB.fromStrict)        gsFakeAvvmSeeds

prettySigningKeyPub :: SigningKey -> Text
prettySigningKeyPub (CCr.toVerification -> vk) = TL.toStrict
                          $  "public key hash: " <> (F.format CCr.hashHexF . CC.addressHash $ vk) <> "\n"
                          <> "     public key: " <> (Builder.toLazyText . CCr.formatFullVerificationKey $ vk)

prettyAddress :: CC.Address -> Text
prettyAddress addr = TL.toStrict
                     $  F.format CC.addressF addr <> "\n"
                     <> F.format CC.addressDetailedF addr

decideKeyMaterialOps :: SystemVersion -> KeyMaterialOps IO
decideKeyMaterialOps =
  let serialiseSigningKey (SigningKey x) = toLazyByteString $ CCr.toCBORXPrv x
  in \case
  ByronLegacy ->
    KeyMaterialOps
    { kmoSerialiseGenesisKey          = pure . serialiseSigningKey
    , kmoSerialiseDelegateKey         = \sk->
        toLazyByteString . Legacy.encodeLegacyDelegateKey . Legacy.LegacyDelegateKey sk
        <$> CCr.runSecureRandom Scrape.keyPairGenerate
    , kmoSerialisePoorKey             = pure . serialiseSigningKey . poorSecretToKey
    , kmoSerialiseGenesis             = pure . canonicalEncPre
    , kmoSerialiseDelegationCert      = pure . canonicalEncPre
    , kmoDeserialiseDelegateKey       = Legacy.lrkSigningKey . snd . either (error . show) id . deserialiseFromBytes Legacy.decodeLegacyDelegateKey
    }
  ByronPBFT ->
    KeyMaterialOps
    { kmoSerialiseGenesisKey          = pure . serialiseSigningKey
    , kmoSerialiseDelegateKey         = pure . serialiseSigningKey
    , kmoSerialisePoorKey             = pure . serialiseSigningKey . poorSecretToKey
    , kmoSerialiseGenesis             = pure . canonicalEncPre
    , kmoSerialiseDelegationCert      = pure . canonicalEncPre
    , kmoDeserialiseDelegateKey       = SigningKey . snd . either (error . show) id . deserialiseFromBytes CCr.fromCBORXPrv
    }

writeSecrets :: FilePath -> String -> String -> (a -> IO LB.ByteString) -> [a] -> IO ()
writeSecrets outDir prefix suffix secretOp xs =
  forM_ (zip xs $ [0::Int ..]) $
  \(secret, nr)-> do
    let filename = outDir </> prefix <> "." <> printf "%03d" nr <> "." <> suffix
    secretOp secret >>= LB.writeFile filename
#ifdef UNIX
    setFileMode                      filename ownerReadMode
#else
    setPermissions filename (emptyPermissions {readable = True})
#endif
