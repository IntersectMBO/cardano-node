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
import           Cardano.Crypto (SigningKey (..))
import qualified Cardano.Crypto.Hashing as CCr
import qualified Cardano.Crypto.Random as CCr
import qualified Cardano.Crypto.Signing as CCr

import           Cardano.Chain.Genesis

import           Cardano.Node.CanonicalJSON

import qualified Byron.Legacy as Legacy
import           CLI

runCommand :: KeyMaterialOps IO -> Command -> IO ()
runCommand
  KeyMaterialOps{..}
  (Genesis
    outDir
    startTime
    protocolParametersFile
    blockCount
    protocolMagic
    giTestBalance
    giFakeAvvmBalance
    giAvvmBalanceFactor
    giSeed)
  = do

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
          True                -- Not using delegate keys unsupported.

    let genesisSpec = either (error . show) id mGenesisSpec

    -- Generate (mostly)
    res <- runExceptT $ generateGenesisData startTime genesisSpec
    let (genesisData, GeneratedSecrets{..}) = either (error . show) id res

    -- Write out (mostly)
    let genesisJSONFile = outDir </> "genesis.json"
    LB.writeFile genesisJSONFile =<< kmoSerialiseGenesis genesisData

    writeSecrets outDir "genesis-keys"  "key"  kmoSerialiseGenesisKey   gsDlgIssuersSecrets
    writeSecrets outDir "delegate-keys" "key"  kmoSerialiseDelegateKey  gsRichSecrets
    writeSecrets outDir "poor-keys"     "key"  kmoSerialisePoorKey      gsPoorSecrets
    writeSecrets outDir "avvm-seed"     "seed" (pure . LB.fromStrict)   gsFakeAvvmSeeds

runCommand
  KeyMaterialOps{..}
  (PrettySecretKeyPublic
    secretPath)
  =
  putStrLn =<< T.unpack . prettySigningKeyPub . kmoDeserialiseDelegateKey <$> LB.readFile secretPath
  where
      prettySigningKeyPub :: SigningKey -> Text
      prettySigningKeyPub (CCr.toVerification -> vk) = TL.toStrict
                                $  "public key hash: " <> (F.format CCr.hashHexF . CC.addressHash $ vk) <> "\n"
                                <> "     public key: " <> (Builder.toLazyText . CCr.formatFullVerificationKey $ vk)

runCommand
  toKMO
  (MigrateDelegateKeyFrom
    fromVer
    secretPathTo
    secretPathFrom)
  =
  LB.writeFile secretPathTo =<< kmoSerialiseDelegateKey toKMO =<< kmoDeserialiseDelegateKey fromKMO <$> LB.readFile secretPathFrom
  where
    fromKMO = decideKeyMaterialOps fromVer

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
    , kmoDeserialiseDelegateKey       = Legacy.lrkSigningKey . snd . either (error . show) id . deserialiseFromBytes Legacy.decodeLegacyDelegateKey
    }
  ByronPBFT ->
    KeyMaterialOps
    { kmoSerialiseGenesisKey          = pure . serialiseSigningKey
    , kmoSerialiseDelegateKey         = pure . serialiseSigningKey
    , kmoSerialisePoorKey             = pure . serialiseSigningKey . poorSecretToKey
    , kmoSerialiseGenesis             = pure . canonicalEncPre
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
