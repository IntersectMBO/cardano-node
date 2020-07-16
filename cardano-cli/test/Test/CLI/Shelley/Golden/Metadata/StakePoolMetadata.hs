{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Metadata.StakePoolMetadata
  ( golden_stakePoolMetadataHash
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse

golden_stakePoolMetadataHash :: Property
golden_stakePoolMetadataHash =
    propertyOnce $ do

      let referenceStakePoolMetaData = "test/Test/golden/shelley/metadata/stake_pool_metadata_hash"


      let stakePoolMetadataFp = "stake-pool-metadata.json"
          outputStakePoolMetadataHashFp = "stake-pool-metadata-hash.txt"
          allFiles = [stakePoolMetadataFp, outputStakePoolMetadataHashFp]

      -- Write the example stake pool metadata to disk
      liftIO $ writeFile stakePoolMetadataFp exampleStakePoolMetadata
      assertFilesExist [stakePoolMetadataFp]

      -- Hash the stake pool metadata
      execCardanoCLIParser
        allFiles
          $ evalCardanoCLIParser [ "shelley","stake-pool","metadata-hash"
                                , "--pool-metadata-file", stakePoolMetadataFp
                                , "--out-file", outputStakePoolMetadataHashFp
                                ]

      -- Check for existence of the stake pool metadata hash file.
      assertFilesExist [outputStakePoolMetadataHashFp]

      -- Check that the stake pool metadata hash file content is correct.
      expectedStakePoolMetadataHash <- liftIO $ readFile referenceStakePoolMetaData
      actualStakePoolMetadataHash <- liftIO $ readFile outputStakePoolMetadataHashFp

      equivalence allFiles expectedStakePoolMetadataHash actualStakePoolMetadataHash

      liftIO $ fileCleanup allFiles

      H.success
  where
    exampleStakePoolMetadata :: Text
    exampleStakePoolMetadata = "{\"homepage\":\"https://iohk.io\",\"name\":\"Genesis Pool C\",\"ticker\":\"GPC\",\"description\":\"Lorem Ipsum Dolor Sit Amet.\"}"
