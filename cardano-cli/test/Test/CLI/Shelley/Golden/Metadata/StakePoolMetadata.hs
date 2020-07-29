{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Metadata.StakePoolMetadata
  ( golden_stakePoolMetadataHash
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

golden_stakePoolMetadataHash :: Property
golden_stakePoolMetadataHash = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  referenceStakePoolMetaData <- OP.noteInputFile "test/Test/golden/shelley/metadata/stake_pool_metadata_hash"

  stakePoolMetadataFile <- OP.noteTempFile tempDir "stake-pool-metadata.json"
  outputStakePoolMetadataHashFp <- OP.noteTempFile tempDir "stake-pool-metadata-hash.txt"

  -- Write the example stake pool metadata to disk
  liftIO $ writeFile stakePoolMetadataFile exampleStakePoolMetadata

  -- Hash the stake pool metadata
  void $ OP.execCardanoCLI
    [ "shelley","stake-pool","metadata-hash"
    , "--pool-metadata-file", stakePoolMetadataFile
    , "--out-file", outputStakePoolMetadataHashFp
    ]

  -- Check that the stake pool metadata hash file content is correct.
  expectedStakePoolMetadataHash <- OP.readFile referenceStakePoolMetaData
  actualStakePoolMetadataHash <- OP.readFile outputStakePoolMetadataHashFp

  OP.equivalence expectedStakePoolMetadataHash actualStakePoolMetadataHash
  where
    exampleStakePoolMetadata :: Text
    exampleStakePoolMetadata = "{\"homepage\":\"https://iohk.io\",\"name\":\"Genesis Pool C\",\"ticker\":\"GPC\",\"description\":\"Lorem Ipsum Dolor Sit Amet.\"}"
