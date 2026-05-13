module Cardano.Testnet.Test.Parser
  ( prop_parseNodeSpecs_roundtrip
  , prop_parseNodeSpecs_counts
  , prop_relay_before_spo_rejected
  , unit_valid_mixed_specs
  , unit_quoted_paths
  ) where

import           Prelude

import           Data.Either (isLeft)
import           Data.List (intercalate)
import qualified Data.List.NonEmpty as NEL

import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Parsers.Cardano (parseNodeSpecs)
import           Testnet.Start.Types (NodeWithOptions (..), TestnetNodesWithOptions (..))

prop_parseNodeSpecs_roundtrip :: H.Property
prop_parseNodeSpecs_roundtrip = H.property $ do
  specs <- H.forAll genValidSpecs
  let input = encodeSpecs specs
      (spoSpecs, relaySpecs) = span isSpo specs
  result <- H.leftFail $ parseNodeSpecs input
  H.footnote $ "Input: " ++ input
  map nodeBin (NEL.toList (optSpoNodes result)) H.=== map specPath spoSpecs
  map nodeBin (optRelayNodes result) H.=== map specPath relaySpecs

prop_parseNodeSpecs_counts :: H.Property
prop_parseNodeSpecs_counts = H.property $ do
  specs <- H.forAll genValidSpecs
  let input = encodeSpecs specs
      nSpos = length $ filter isSpo specs
      nRelays = length $ filter (not . isSpo) specs
  result <- H.leftFail $ parseNodeSpecs input
  H.footnote $ "Input: " ++ input
  length (NEL.toList (optSpoNodes result)) H.=== nSpos
  length (optRelayNodes result) H.=== nRelays

prop_relay_before_spo_rejected :: H.Property
prop_relay_before_spo_rejected = H.property $ do
  specs <- H.forAll genBadOrderSpecs
  let input = encodeSpecs specs
  H.footnote $ "Input: " ++ input
  H.assert $ isLeft $ parseNodeSpecs input

-- | Valid mixed specs parse correctly.
unit_valid_mixed_specs :: H.Property
unit_valid_mixed_specs = H.withTests 1 . H.property $ do
  result1 <- H.leftFail $ parseNodeSpecs "spo,spo,relay,relay"
  length (NEL.toList (optSpoNodes result1)) H.=== 2
  length (optRelayNodes result1) H.=== 2

  result2 <- H.leftFail $ parseNodeSpecs "spo"
  length (NEL.toList (optSpoNodes result2)) H.=== 1
  length (optRelayNodes result2) H.=== 0

  result3 <- H.leftFail $ parseNodeSpecs "spo:node-bin=/usr/bin/cardano-node,relay"
  nodeBin (NEL.head (optSpoNodes result3)) H.=== Just "/usr/bin/cardano-node"
  length (optRelayNodes result3) H.=== 1

-- | Quoted paths with commas, colons, backslashes, and quotes parse correctly.
unit_quoted_paths :: H.Property
unit_quoted_paths = H.withTests 1 . H.property $ do
  r1 <- H.leftFail $ parseNodeSpecs "spo:node-bin=\"/path,with,commas\""
  nodeBin (NEL.head (optSpoNodes r1)) H.=== Just "/path,with,commas"

  r2 <- H.leftFail $ parseNodeSpecs "spo:node-bin=\"/path:with:colons\""
  nodeBin (NEL.head (optSpoNodes r2)) H.=== Just "/path:with:colons"

  r3 <- H.leftFail $ parseNodeSpecs "spo:node-bin=\"/path\\\\with\\\\backslashes\""
  nodeBin (NEL.head (optSpoNodes r3)) H.=== Just "/path\\with\\backslashes"

  r4 <- H.leftFail $ parseNodeSpecs "spo:node-bin=\"/path\\\"with\\\"quotes\""
  nodeBin (NEL.head (optSpoNodes r4)) H.=== Just "/path\"with\"quotes"

  -- Misplaced quotes must fail
  H.assert $ isLeft $ parseNodeSpecs "spo:node-bin=\"/unclosed"       -- opening quote, no close
  H.assert $ isLeft $ parseNodeSpecs "spo:node-bin=/closed\""         -- no opening, quote at end
  H.assert $ isLeft $ parseNodeSpecs "spo:node-bin=/mid\"dle"         -- quote in the middle

-- Adversarial substrings that could confuse the parser
adversarialFragments :: [String]
adversarialFragments =
  [ ":"
  , "spo"
  , "relay"
  , "node-bin"
  , "\\\""
  , "\\\\"
  , "spo:node-bin=\"\""
  , "relay,spo,relay"
  , "\",spo:node-bin=evil\""
  , "node-bin=gotcha"
  , "spo:node-bin=\"foo,bar\",relay"
  , ","
  , "="
  , "\""
  , "\\"
  ]

genPathSegment :: H.Gen String
genPathSegment = Gen.choice
  [ Gen.element adversarialFragments
  , Gen.string (Range.linear 1 10) (Gen.frequency [(9, Gen.alphaNum), (1, pure '.')])
  ]

genPath :: H.Gen FilePath
genPath = do
  segments <- Gen.list (Range.linear 1 5) genPathSegment
  pure $ "/" ++ intercalate "/" segments

data Role = RSpo | RRelay deriving Show

data Spec = Spec Role (Maybe FilePath) deriving Show

genSpec :: Role -> H.Gen Spec
genSpec role = Spec role <$> Gen.maybe genPath

genValidSpecs :: H.Gen [Spec]
genValidSpecs = do
  spos <- Gen.list (Range.linear 1 4) (genSpec RSpo)
  relays <- Gen.list (Range.linear 0 4) (genSpec RRelay)
  pure $ spos ++ relays

encodePath :: FilePath -> String
encodePath path
  | any (`elem` path) (",:\\\"" :: String) = "\"" ++ concatMap escapeChar path ++ "\""
  | otherwise = path
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]

encodeSpec :: Spec -> String
encodeSpec (Spec role mpath) =
  roleStr ++ maybe "" (\p -> ":node-bin=" ++ encodePath p) mpath
  where
    roleStr = case role of
      RSpo -> "spo"
      RRelay -> "relay"

encodeSpecs :: [Spec] -> String
encodeSpecs = intercalate "," . map encodeSpec

specPath :: Spec -> Maybe FilePath
specPath (Spec _ mp) = mp

isSpo :: Spec -> Bool
isSpo (Spec RSpo _) = True
isSpo _ = False

-- | Generate specs matching: spo*, relay+, (relay|spo)*, spo+
-- i.e. at least one relay appears before at least one spo.
genBadOrderSpecs :: H.Gen [Spec]
genBadOrderSpecs = do
  before <- Gen.list (Range.linear 0 10) (genSpec RSpo)
  relay <- genSpec RRelay
  middle <- Gen.list (Range.linear 0 10) (genSpec =<< Gen.element [RSpo, RRelay])
  after <- genSpec RSpo
  pure $ before ++ [relay] ++ middle ++ [after]
