{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.CLI.Shelley.Golden.Genesis.Create
  ( golden_shelleyGenesisCreate
  ) where

import Cardano.Prelude hiding (to)

import Prelude(String)

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Time.Clock as DT
import qualified Data.Time.Format as DT
import qualified System.Directory as IO
import qualified System.IO as IO

import Hedgehog (Property, forAll, (===))

import qualified Hedgehog as H
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import qualified Test.OptParse as OP
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

{- HLINT ignore "Use camelCase" -}

-- | Assert the file contains the given number of occurences of the given string
assertFileOccurences :: Int -> String -> FilePath -> H.PropertyT IO ()
assertFileOccurences n s fp = do
  signingKeyContents <- liftIO $ IO.readFile fp

  length (filter (s `L.isInfixOf`) (L.lines signingKeyContents)) === n

-- | Format the given time as an ISO 8601 date-time string
formatIso8601 :: DT.UTCTime -> String
formatIso8601 = DT.formatTime DT.defaultTimeLocale (DT.iso8601DateFormat (Just "%H:%M:%SZ"))

-- | Return the supply value with the result of the supplied function as a tuple
withSnd :: (a -> b) -> a -> (a, b)
withSnd f a = (a, f a)

parseMaxLovelaceSupply :: J.Value -> J.Parser Int
parseMaxLovelaceSupply = J.withObject "Object" $ \o -> o J..: "maxLovelaceSupply"

parseSystemStart :: J.Value -> J.Parser String
parseSystemStart = J.withObject "Object" $ \o -> o J..: "systemStart"

parseHashMap :: J.Value -> J.Parser (HM.HashMap String J.Value)
parseHashMap (J.Object hm) = pure $ HM.fromList $ fmap (first T.unpack) (HM.toList (hm))
parseHashMap v = J.typeMismatch "Object" v

parseDelegateCount :: J.Value -> J.Parser Int
parseDelegateCount = J.withObject "Object" $ \o -> do
  delegates <- (o J..: "genDelegs") >>= parseHashMap
  pure $ HM.size delegates

parseDelegateKey :: J.Value -> J.Parser String
parseDelegateKey = J.withObject "Object" $ \o -> o J..: "delegate"

parseDelegateKeys :: J.Value -> J.Parser [String]
parseDelegateKeys = J.withObject "Object" $ \o -> do
  delegates <- (o J..: "genDelegs") >>= parseHashMap
  sequence $ fmap (parseDelegateKey . snd) (HM.toList delegates)

parseHashKeys :: J.Value -> J.Parser [String]
parseHashKeys = J.withObject "Object" $ \o -> do
  delegates <- (o J..: "genDelegs") >>= parseHashMap
  pure $ fmap fst (HM.toList delegates)

parseTotalSupply :: J.Value -> J.Parser Int
parseTotalSupply = J.withObject "Object" $ \ o -> do
  initialFunds <- (o J..: "initialFunds") >>= parseHashMap
  fmap sum (sequence (fmap (J.parseJSON @Int . snd) (HM.toList initialFunds)))

golden_shelleyGenesisCreate :: Property
golden_shelleyGenesisCreate = OP.propertyOnce $ do
  OP.workspace "tmp/genesis-create" $ \tempDir -> do
    let sourceGenesisSpecFile = "test/Test/golden/shelley/genesis/genesis.spec.json"

    liftIO $ IO.copyFile sourceGenesisSpecFile (tempDir <> "/genesis.spec.json")

    let genesisFile = tempDir <> "/genesis.json"

    fmtStartTime <- fmap formatIso8601 $ liftIO DT.getCurrentTime

    (supply, fmtSupply) <- fmap (withSnd show) $ forAll $ G.int (R.linear 10000000 4000000000)
    (delegateCount, fmtDelegateCount) <- fmap (withSnd show) $ forAll $ G.int (R.linear 4 19)
    (utxoCount, fmtUtxoCount) <- fmap (withSnd show) $ forAll $ G.int (R.linear 4 19)

    -- Create the genesis json file and required keys
    void $ liftIO $ OP.execCardanoCLI
        [ "shelley","genesis","create"
        , "--testnet-magic", "12"
        , "--start-time", fmtStartTime
        , "--supply", fmtSupply
        , "--gen-genesis-keys", fmtDelegateCount
        , "--gen-utxo-keys", fmtUtxoCount
        , "--genesis-dir", tempDir
        ]

    OP.assertFilesExist [genesisFile]

    genesisContents <- liftIO $ LBS.readFile genesisFile

    actualJson <- H.evalEither $ J.eitherDecode genesisContents
    actualSupply <- H.evalEither $ J.parseEither parseMaxLovelaceSupply actualJson
    actualStartTime <- H.evalEither $ J.parseEither parseSystemStart actualJson
    actualDelegateCount <- H.evalEither $ J.parseEither parseDelegateCount actualJson
    actualTotalSupply <- H.evalEither $ J.parseEither parseTotalSupply actualJson
    actualHashKeys <- H.evalEither $ J.parseEither parseHashKeys actualJson
    actualDelegateKeys <- H.evalEither $ J.parseEither parseDelegateKeys actualJson

    actualSupply === supply
    actualStartTime === fmtStartTime
    actualDelegateCount === delegateCount
    actualDelegateCount === utxoCount
    actualTotalSupply === supply -- Check that the sum of the initial fund amounts matches the total supply

    -- Check uniqueness and count of hash keys
    S.size (S.fromList actualHashKeys) === length actualHashKeys -- This isn't strictly necessary because we use aeson which guarantees uniqueness of keys
    S.size (S.fromList actualHashKeys) === delegateCount

    -- Check uniqueness and count of hash keys
    S.size (S.fromList actualDelegateKeys) === length actualDelegateKeys
    S.size (S.fromList actualDelegateKeys) === delegateCount

    for_ [1 .. delegateCount] $ \i -> do
      -- Check Genesis keys
      assertFileOccurences 1 "GenesisSigningKey_ed25519" $ tempDir <> "/genesis-keys/genesis" <> show i <> ".skey"
      assertFileOccurences 1 "GenesisVerificationKey_ed25519" $ tempDir <> "/genesis-keys/genesis" <> show i <> ".vkey"

      -- Check delegate keys
      assertFileOccurences 1 "GenesisDelegateSigningKey_ed25519" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".skey"
      assertFileOccurences 1 "GenesisDelegateVerificationKey_ed25519" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".vkey"
      assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".counter"

      -- Check utxo keys
      assertFileOccurences 1 "GenesisUTxOSigningKey_ed25519" $ tempDir <> "/utxo-keys/utxo" <> show i <> ".skey"
      assertFileOccurences 1 "GenesisUTxOVerificationKey_ed25519"  $ tempDir <> "/utxo-keys/utxo" <> show i <> ".vkey"

  OP.workspace "tmp/genesis-create" $ \tempDir -> do
    let genesisFile = tempDir <> "/genesis.json"

    fmtStartTime <- fmap formatIso8601 $ liftIO DT.getCurrentTime

    (supply, fmtSupply) <- fmap (withSnd show) $ forAll $ G.int (R.linear 10000000 4000000000)
    (delegateCount, fmtDelegateCount) <- fmap (withSnd show) $ forAll $ G.int (R.linear 4 19)
    (utxoCount, fmtUtxoCount) <- fmap (withSnd show) $ forAll $ G.int (R.linear 4 19)

    -- Create the genesis json file and required keys
    void $ liftIO $ OP.execCardanoCLI
        [ "shelley","genesis","create"
        , "--testnet-magic", "12"
        , "--start-time", fmtStartTime
        , "--supply", fmtSupply
        , "--gen-genesis-keys", fmtDelegateCount
        , "--gen-utxo-keys", fmtUtxoCount
        , "--genesis-dir", tempDir
        ]

    OP.assertFilesExist [genesisFile]

    genesisContents <- liftIO $ LBS.readFile genesisFile

    actualJson <- H.evalEither $ J.eitherDecode genesisContents
    actualSupply <- H.evalEither $ J.parseEither parseMaxLovelaceSupply actualJson
    actualStartTime <- H.evalEither $ J.parseEither parseSystemStart actualJson
    actualDelegateCount <- H.evalEither $ J.parseEither parseDelegateCount actualJson
    actualTotalSupply <- H.evalEither $ J.parseEither parseTotalSupply actualJson
    actualHashKeys <- H.evalEither $ J.parseEither parseHashKeys actualJson
    actualDelegateKeys <- H.evalEither $ J.parseEither parseDelegateKeys actualJson

    actualSupply === supply
    actualStartTime === fmtStartTime
    actualDelegateCount === delegateCount
    actualDelegateCount === utxoCount
    actualTotalSupply === supply -- Check that the sum of the initial fund amounts matches the total supply

    -- Check uniqueness and count of hash keys
    S.size (S.fromList actualHashKeys) === length actualHashKeys -- This isn't strictly necessary because we use aeson which guarantees uniqueness of keys
    S.size (S.fromList actualHashKeys) === delegateCount

    -- Check uniqueness and count of hash keys
    S.size (S.fromList actualDelegateKeys) === length actualDelegateKeys
    S.size (S.fromList actualDelegateKeys) === delegateCount

    for_ [1 .. delegateCount] $ \i -> do
      -- Check Genesis keys
      assertFileOccurences 1 "GenesisSigningKey_ed25519" $ tempDir <> "/genesis-keys/genesis" <> show i <> ".skey"
      assertFileOccurences 1 "GenesisVerificationKey_ed25519" $ tempDir <> "/genesis-keys/genesis" <> show i <> ".vkey"

      -- Check delegate keys
      assertFileOccurences 1 "GenesisDelegateSigningKey_ed25519" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".skey"
      assertFileOccurences 1 "GenesisDelegateVerificationKey_ed25519" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".vkey"
      assertFileOccurences 1 "NodeOperationalCertificateIssueCounter" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".counter"

      -- Check utxo keys
      assertFileOccurences 1 "GenesisUTxOSigningKey_ed25519" $ tempDir <> "/utxo-keys/utxo" <> show i <> ".skey"
      assertFileOccurences 1 "GenesisUTxOVerificationKey_ed25519"  $ tempDir <> "/utxo-keys/utxo" <> show i <> ".vkey"
