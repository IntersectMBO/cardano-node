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

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Run (
      runCLI
    ) where

import           Prelude (error, id)

import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Lens (LensLike, _Left)
import           Control.Monad
import qualified Data.Binary as Binary
import           Data.Coerce
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import           System.Posix.Files (ownerReadMode, setFileMode)
import           System.Directory (createDirectory, doesPathExist)

import qualified Text.JSON.Canonical as CanonicalJSON

import qualified Crypto.SCRAPE as Scrape

import           Cardano.Prelude hiding (option)

import qualified Cardano.Crypto.Wallet as CC
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

-- LegacyRichmenKey is a subset of the UserSecret's from the legacy codebase:
-- 1. the VSS keypair must be present
-- 2. the signing key must be present
-- 3. the rest must be absent (Nothing)
--
-- Legacy reference: https://github.com/input-output-hk/cardano-sl/blob/release/3.0.1/lib/src/Pos/Util/UserSecret.hs#L189
data LegacyRichmenKey
  =  LegacyRichmenKey
  { lrkSigningKey :: SigningKey
  , lrkVSSKeyPair :: Scrape.KeyPair
  }

-- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
encodeBinary :: Binary.Binary a => a -> E.Encoding
encodeBinary = E.encodeBytes . LB.toStrict . Binary.encode

-- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
decodeBinary :: Binary.Binary a => D.Decoder s a
decodeBinary = do
    x <- D.decodeBytesCanonical
    toCborError $ case Binary.decodeOrFail (LB.fromStrict x) of
        Left (_, _, err) -> Left (T.pack err)
        Right (bs, _, res)
            | LB.null bs -> Right res
            | otherwise  -> Left "decodeBinary: unconsumed input"

encodeXPrv :: CC.XPrv -> E.Encoding
encodeXPrv a = E.encodeBytes $ CC.unXPrv a

decodeXPrv :: D.Decoder s CC.XPrv
decodeXPrv = toCborError . over _Left T.pack . CC.xprv =<< D.decodeBytesCanonical
  where over :: LensLike Identity s t a b -> (a -> b) -> s -> t
        over = coerce

-- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
-- | Enforces that the input size is the same as the decoded one, failing in
-- case it's not.
enforceSize :: Text -> Int -> D.Decoder s ()
enforceSize lbl requestedSize = D.decodeListLenCanonical >>= matchSize requestedSize lbl

-- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
-- | Compare two sizes, failing if they are not equal.
matchSize :: Int -> Text -> Int -> D.Decoder s ()
matchSize requestedSize lbl actualSize =
  when (actualSize /= requestedSize) $
    cborError (lbl <> " failed the size check. Expected " <> show requestedSize <> ", found " <> show actualSize)

-- Reverse-engineered from cardano-sl legacy codebase.
encodeLegacyRichmenKey :: LegacyRichmenKey -> E.Encoding
encodeLegacyRichmenKey LegacyRichmenKey{lrkSigningKey=(SigningKey sk),..}
  =  E.encodeListLen 4
  <> E.encodeListLen 1 <> encodeBinary lrkVSSKeyPair
  <> E.encodeListLen 1 <> encodeXPrv sk
  <> E.encodeListLenIndef <> E.encodeBreak
  <> E.encodeListLen 0

-- Reverse-engineered from cardano-sl legacy codebase.
decodeLegacyRichmenKey :: D.Decoder s LegacyRichmenKey
decodeLegacyRichmenKey = do
    enforceSize "UserSecret" 4
    vss  <- do
      enforceSize "vss" 1
      decodeBinary
    pkey <- do
      enforceSize "pkey" 1
      SigningKey <$> decodeXPrv
    _    <- do
      D.decodeListLenIndef
      D.decodeSequenceLenIndef (flip (:)) [] reverse D.decodeNull
    _    <- do
      enforceSize "wallet" 0
    pure $ LegacyRichmenKey pkey vss

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

      RtByronLegacyRichmanKey src dst -> do
        bytes <- LB.readFile src
        let (,) _ key = either (error . show) id $ deserialiseFromBytes decodeLegacyRichmenKey bytes
        LB.writeFile dst $ toLazyByteString $ encodeLegacyRichmenKey key

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

        richmenSecrets <- zipWith LegacyRichmenKey gsRichSecrets
                       <$> (runSecureRandom $ flip replicateM Scrape.keyPairGenerate
                                            $ length gsRichSecrets)

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
        writeSecrets "rich-secret" richmenSecrets      $ toLazyByteString . encodeLegacyRichmenKey
        writeSecrets "poor-secret" gsPoorSecrets       $ serialiseSigningKey . poorSecretToKey
        writeSecrets "avvm-seed"   gsFakeAvvmSeeds     $ LB.fromStrict
