{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cli.JSON where

import           Cardano.Prelude hiding (filter)

import           Cardano.Api.Shelley
import           Gen.Cardano.Api.Typed (genStakeAddress, genLovelace, genVerificationKeyHash)

import           Data.Aeson
import qualified Data.Map.Strict as Map

import           Cardano.CLI.Shelley.Run.Query

import           Hedgehog (Property, checkSequential, discover, forAll, property, tripping)
import           Hedgehog (Gen)
import           Hedgehog.Gen as Gen
import           Hedgehog.Range as Range

-- TODO: Move to cardano-api
prop_json_roundtrip_delegations_and_rewards :: Property
prop_json_roundtrip_delegations_and_rewards =
  property $ do
    dAndG <- forAll genDelegationsAndRewards
    tripping dAndG encode eitherDecode

genDelegationsAndRewards :: Gen DelegationsAndRewards
genDelegationsAndRewards = do
  let r = Range.constant 0 3
  sAddrs <- Gen.list r  genStakeAddress
  sLovelace <- Gen.list r genLovelace
  let delegMapAmt = Map.fromList $ zip sAddrs sLovelace
  poolIDs <- Gen.list r genPoolId
  let delegMapPool = Map.fromList $ zip sAddrs poolIDs
  return $ DelegationsAndRewards (delegMapAmt,delegMapPool)

genPoolId :: Gen (Hash StakePoolKey)
genPoolId = genVerificationKeyHash AsStakePoolKey

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  checkSequential $$discover
