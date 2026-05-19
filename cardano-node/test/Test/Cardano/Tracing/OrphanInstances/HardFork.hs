-- | Test the JSON encoding of instances in Cardano.Tracing.OrphanInstances.HardFork
--
-- The golden files are stored in the path given by 'addPrefix'.
--
-- If a new test is added and no golden file exists for it it will be created.
-- This new file needs to be committed.
--
-- For now we added a couple of representative examples, however the tests are
-- not exhaustive.
--
-- The examples can be best viewed using a tool like 'jq'.
module Test.Cardano.Tracing.OrphanInstances.HardFork (tests) where

import           Cardano.Protocol.Crypto (StandardCrypto)
import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()
import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion as Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano.Block as Consensus.Cardano
import qualified Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion as Consensus.Cardano

import qualified Data.Aeson as Aeson
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.SOP.Strict (NP (Nil, (:*)))

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H.Base
import qualified Hedgehog.Extras.Test.Golden as H.Golden
import           Hedgehog.Internal.Property (PropertyName (PropertyName))

tests :: IO Bool
tests = H.checkSequential
      $ H.Group "HardForkNodeToClientVersion JSON instances"
      $ test
     <$> [ (  ntcByronOnly
           , "ntcByronOnly.json")
         , (  ntc_HFV3_allDisabled
           , "ntc_HFV3_allDisabled.json")
         , (  ntc_HFV3_ByronV1
           , "ntc_HFV3_ByronV1.json")
         , (  ntc_HFV3_ByronV1_ShelleyV8
           , "ntc_HFV3_ByronV1_ShelleyV8.json")
         , (  ntc_HFV3_ByronV1_ShelleyV8_ConwayV2
           , "ntc_HFV3_ByronV1_ShelleyV8_ConwayV2.json")
         ]
  where
    test (actualValue, goldenBaseName) =
        (PropertyName goldenBaseName, goldenTestJSON actualValue goldenBaseName)

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

ntcByronOnly ::
  Consensus.HardForkNodeToClientVersion
    (Consensus.Cardano.CardanoEras StandardCrypto)
ntcByronOnly =
    Consensus.HardForkNodeToClientDisabled
      Consensus.Cardano.ByronNodeToClientVersion1

ntc_HFV3_allDisabled ::
  Consensus.HardForkNodeToClientVersion
    (Consensus.Cardano.CardanoEras StandardCrypto)
ntc_HFV3_allDisabled =
  Consensus.HardForkNodeToClientEnabled
    Consensus.HardForkSpecificNodeToClientVersion3
    (    Consensus.EraNodeToClientDisabled -- Byron
      :* Consensus.EraNodeToClientDisabled -- Shelley
      :* Consensus.EraNodeToClientDisabled -- Allegra
      :* Consensus.EraNodeToClientDisabled -- Mary
      :* Consensus.EraNodeToClientDisabled -- Alonzo
      :* Consensus.EraNodeToClientDisabled -- Babbage
      :* Consensus.EraNodeToClientDisabled -- Conway
      :* Consensus.EraNodeToClientDisabled -- Dijkstra
      :* Nil
    )

ntc_HFV3_ByronV1 ::
  Consensus.HardForkNodeToClientVersion
    (Consensus.Cardano.CardanoEras StandardCrypto)
ntc_HFV3_ByronV1 =
  Consensus.HardForkNodeToClientEnabled
    Consensus.HardForkSpecificNodeToClientVersion3
    (    Consensus.EraNodeToClientEnabled Consensus.Cardano.ByronNodeToClientVersion1 -- Byron
      :* Consensus.EraNodeToClientDisabled -- Shelley
      :* Consensus.EraNodeToClientDisabled -- Allegra
      :* Consensus.EraNodeToClientDisabled -- Mary
      :* Consensus.EraNodeToClientDisabled -- Alonzo
      :* Consensus.EraNodeToClientDisabled -- Babbage
      :* Consensus.EraNodeToClientDisabled -- Conway
      :* Consensus.EraNodeToClientDisabled -- Dijkstra
      :* Nil
    )

ntc_HFV3_ByronV1_ShelleyV8 ::
  Consensus.HardForkNodeToClientVersion
    (Consensus.Cardano.CardanoEras StandardCrypto)
ntc_HFV3_ByronV1_ShelleyV8 =
  Consensus.HardForkNodeToClientEnabled
    Consensus.HardForkSpecificNodeToClientVersion3
    (    Consensus.EraNodeToClientEnabled Consensus.Cardano.ByronNodeToClientVersion1   -- Byron
      :* Consensus.EraNodeToClientEnabled Consensus.Cardano.ShelleyNodeToClientVersion8 -- Shelley
      :* Consensus.EraNodeToClientDisabled -- Allegra
      :* Consensus.EraNodeToClientDisabled -- Mary
      :* Consensus.EraNodeToClientDisabled -- Alonzo
      :* Consensus.EraNodeToClientDisabled -- Babbage
      :* Consensus.EraNodeToClientDisabled -- Conway
      :* Consensus.EraNodeToClientDisabled -- Dijkstra
      :* Nil
    )

ntc_HFV3_ByronV1_ShelleyV8_ConwayV2 ::
  Consensus.HardForkNodeToClientVersion
    (Consensus.Cardano.CardanoEras StandardCrypto)
ntc_HFV3_ByronV1_ShelleyV8_ConwayV2 =
  Consensus.HardForkNodeToClientEnabled
    Consensus.HardForkSpecificNodeToClientVersion3
    (    Consensus.EraNodeToClientEnabled Consensus.Cardano.ByronNodeToClientVersion1   -- Byron
      :* Consensus.EraNodeToClientEnabled Consensus.Cardano.ShelleyNodeToClientVersion8 -- Shelley
      :* Consensus.EraNodeToClientDisabled -- Allegra
      :* Consensus.EraNodeToClientDisabled -- Mary
      :* Consensus.EraNodeToClientDisabled -- Alonzo
      :* Consensus.EraNodeToClientDisabled -- Babbage
      :* Consensus.EraNodeToClientEnabled Consensus.Cardano.ShelleyNodeToClientVersion8 -- Conway
      :* Consensus.EraNodeToClientEnabled Consensus.Cardano.ShelleyNodeToClientVersion8 -- Dijkstra
      :* Nil
    )

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

goldenTestJSON :: Aeson.ToJSON a => a -> FilePath -> Property
goldenTestJSON valueToEncode goldenFileBaseName =
    H.withTests 1 $ H.withShrinks 0 $ H.property $ do
      goldenFp    <- H.Base.note $ addPrefix goldenFileBaseName
      let actualValue = unpack $ Aeson.encode valueToEncode
      H.Golden.diffVsGoldenFile actualValue goldenFp

-- | NB: this function is only used in 'goldenTestJSON' but it is defined at the
-- top level so that we can refer to it in the documentation of this module.
addPrefix :: FilePath -> FilePath
addPrefix fname = "test/Test/Cardano/Tracing/OrphanInstances/data/" <> fname
