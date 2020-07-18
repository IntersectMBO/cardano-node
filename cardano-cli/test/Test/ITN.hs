{-# LANGUAGE OverloadedStrings #-}

module Test.ITN
  ( tests
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Base16 as Base16
import qualified Codec.Binary.Bech32 as Bech32

import           Cardano.CLI.Shelley.Run.Key (decodeBech32Key)

import           Hedgehog (Property, (===))
import qualified Hedgehog as H

import           Test.OptParse


-- | Bech32 verification key
itnVerKey :: Text
itnVerKey = "ed25519_pk1demeytzdadayd4qrqeg2raadp2eceg3mrdmefxyfxx73q60hg4xsjjyzyq"

-- | Bech32 signing key
itnSignKey :: Text
itnSignKey = "ed25519_sk1yhnetcmla9pskrvp5z5ff2v8gkenhmluy736jd6nrxrlxcgn70zsy94f7k"

-- | 1. Convert a bech32 ITN key pair to a haskell stake verification key and signing key
--   2. Derive the haskell verification key from the haskell signing key.
prop_convertITNKeys :: Property
prop_convertITNKeys =
  propertyOnce $ do

        -- ITN input file paths
    let itnVerKeyFp = "itnVerKey.key"
        itnSignKeyFp = "itnSignKey.key"

        -- Converted keys output file paths
        outputHaskellVerKeyFp = "haskell-verification-key.key"
        outputHaskellSignKeyFp = "haskell-signing-key.key"

        allFiles = [itnVerKeyFp, itnSignKeyFp, outputHaskellVerKeyFp, outputHaskellSignKeyFp]

    -- Write ITN keys to disk
    liftIO $ writeFile itnVerKeyFp itnVerKey
    liftIO $ writeFile itnSignKeyFp itnSignKey
    assertFilesExist [itnVerKeyFp, itnSignKeyFp]

    -- Generate haskell stake verification key
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","key","convert-itn-key"
                               , "--itn-verification-key-file", itnVerKeyFp
                               , "--out-file", outputHaskellVerKeyFp
                               ]
    -- Generate haskell signing key
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","key","convert-itn-key"
                               , "--itn-signing-key-file", itnSignKeyFp
                               , "--out-file", outputHaskellSignKeyFp
                               ]

    -- Check for existence of the converted ITN keys
    assertFilesExist [outputHaskellVerKeyFp, outputHaskellSignKeyFp]

    liftIO $ fileCleanup allFiles

    H.success

-- | We check our 'decodeBech32Key' outputs against https://slowli.github.io/bech32-buffer/
-- using 'itnVerKey' & 'itnSignKey' as inputs.
golden_bech32Decode :: Property
golden_bech32Decode = propertyOnce $ do
    (vHumReadPart, vDataPart , _) <- H.evalEither $ decodeBech32Key itnVerKey
    Just vDataPartBase16 <- pure (dataPartToBase16 vDataPart)

    (sHumReadPart, sDataPart , _) <- H.evalEither $ decodeBech32Key itnSignKey
    Just sDataPartBase16 <- pure (dataPartToBase16 sDataPart)

    -- Based on https://slowli.github.io/bech32-buffer/ which are in Base16
    let expectedHumanReadPartVerificationKey = "ed25519_pk"
        expectedDataPartVerificationKey = "6e77922c4deb7a46d4030650a1f7ad0ab38ca23b1b7794988931bd1069f7454d"
        expectedHumanReadPartSigningKey = "ed25519_sk"
        expectedDataPartSigningKey = "25e795e37fe9430b0d81a0a894a98745b33beffc27a3a937531987f36113f3c5"


    -- ITN Verification key decode check
    expectedHumanReadPartVerificationKey === Bech32.humanReadablePartToText vHumReadPart
    expectedDataPartVerificationKey ===  vDataPartBase16


    -- ITN Signing key decode check
    expectedHumanReadPartSigningKey === Bech32.humanReadablePartToText sHumReadPart
    expectedDataPartSigningKey === sDataPartBase16
    H.success
  where
    dataPartToBase16 :: Bech32.DataPart -> Maybe ByteString
    dataPartToBase16 = fmap Base16.encode . Bech32.dataPartToBytes

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "ITN key conversion"
        [ ("prop_convertITNKeys", prop_convertITNKeys)
        , ("golden_bech32Decode", golden_bech32Decode)
        ]
