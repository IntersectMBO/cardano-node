module Cardano.CLI.Shelley.Run.Address
  ( runAddressGen
  ) where

import           Cardano.Prelude

import           Cardano.Api (mkShelleyPubKeyAddress, shelleyKeyPair)


runAddressGen :: IO ()
runAddressGen = do
  payKeyPair <- shelleyKeyPair
  stakeKeyPair <- shelleyKeyPair
  let sPa = mkShelleyPubKeyAddress payKeyPair stakeKeyPair
  print sPa


