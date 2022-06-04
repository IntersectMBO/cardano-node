module Test.Cardano.Api.Typed.Script
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley
import           Cardano.Prelude
import           Data.Aeson
import           Gen.Cardano.Api.Typed
import           Hedgehog (Property, (===))
import           Hedgehog.Extras.Aeson
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

exampleSimpleScriptV1_All :: SimpleScript SimpleScriptV1
exampleSimpleScriptV1_All =
  RequireAllOf
    [ RequireSignature "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
    , RequireSignature "a687dcc24e00dd3caafbeb5e68f97ca8ef269cb6fe971345eb951756"
    , RequireSignature "0bd1d702b2e6188fe0857a6dc7ffb0675229bab58c86638ffa87ed6d"
    , RequireSignature "dd0044a26cf7d4491ecea720fda11afb59d5725b53afa605fdf695e6"
    , RequireSignature "cf223afe150cc8e89f11edaacbbd55b011ba44fbedef66fbd37d8c9d"
    , RequireSignature "372643e7ef4b41fd2649ada30a89d35cb90b7c14cb5de252e6ce6cb7"
    , RequireSignature "aa453dc184c5037d60e3fbbadb023f4a41bac112f249b76be9bb37ad"
    , RequireSignature "6b732c60c267bab894854d6dd57a04a94e603fcc4c36274c9ed75952"
    ]

exampleSimpleScriptV1_Any :: SimpleScript SimpleScriptV1
exampleSimpleScriptV1_Any =
  RequireAnyOf
    [ RequireSignature "d92b712d1882c3b0f75b6f677e0b2cbef4fbc8b8121bb9dde324ff09"
    , RequireSignature "4d780ed1bfc88cbd4da3f48de91fe728c3530d662564bf5a284b5321"
    , RequireSignature "3a94d6d4e786a3f5d439939cafc0536f6abc324fb8404084d6034bf8"
    , RequireSignature "b12e094d1db7c0fba5121f22db193d0060efed8be43654f861bb68ae"
    , RequireSignature "9be49d56442b4b8b16cab4e43e238bbdefc6c803d554c82fcd5facc3"
    , RequireSignature "622be5fab3b5c3f371a50a535e4d3349c942a98cecee93b24e2fd11d"
    ]

exampleSimpleScriptV1_MofN :: SimpleScript SimpleScriptV1
exampleSimpleScriptV1_MofN =
  RequireMOf 2
    [ RequireSignature "2f3d4cf10d0471a1db9f2d2907de867968c27bca6272f062cd1c2413"
    , RequireSignature "f856c0c5839bab22673747d53f1ae9eed84afafb085f086e8e988614"
    , RequireSignature "b275b08c999097247f7c17e77007c7010cd19f20cc086ad99d398538"
    , RequireSignature "686024aecb5884d73a11b9ae4e63931112ba737e878d74638b78513a"
    ]


exampleSimpleScriptV2_All :: SimpleScript SimpleScriptV2
exampleSimpleScriptV2_All =
  RequireAllOf
    [ RequireSignature "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
    , RequireTimeBefore TimeLocksInSimpleScriptV2 (SlotNo 42)
    ]

exampleSimpleScriptV2_Any :: SimpleScript SimpleScriptV2
exampleSimpleScriptV2_Any =
  RequireAnyOf
    [ RequireSignature "d92b712d1882c3b0f75b6f677e0b2cbef4fbc8b8121bb9dde324ff09"
    , RequireTimeAfter TimeLocksInSimpleScriptV2 (SlotNo 42)
    ]

exampleSimpleScriptV2_MofN :: SimpleScript SimpleScriptV2
exampleSimpleScriptV2_MofN =
  RequireMOf 1
    [ RequireSignature "2f3d4cf10d0471a1db9f2d2907de867968c27bca6272f062cd1c2413"
    , RequireSignature "f856c0c5839bab22673747d53f1ae9eed84afafb085f086e8e988614"
    , RequireTimeBefore TimeLocksInSimpleScriptV2 (SlotNo 42)
    ]

-- -----------------------------------------------------------------------------

prop_golden_SimpleScriptV1_All :: Property
prop_golden_SimpleScriptV1_All =
  goldenTestJsonValuePretty exampleSimpleScriptV1_All
                            "test/Golden/Script/SimpleV1/all"

prop_golden_SimpleScriptV1_Any :: Property
prop_golden_SimpleScriptV1_Any =
  goldenTestJsonValuePretty exampleSimpleScriptV1_Any
                            "test/Golden/Script/SimpleV1/any"

prop_golden_SimpleScriptV1_MofN :: Property
prop_golden_SimpleScriptV1_MofN =
  goldenTestJsonValuePretty exampleSimpleScriptV1_MofN
                            "test/Golden/Script/SimpleV1/atleast"

prop_golden_SimpleScriptV2_All :: Property
prop_golden_SimpleScriptV2_All =
  goldenTestJsonValuePretty exampleSimpleScriptV2_All
                            "test/Golden/Script/SimpleV2/all"

prop_golden_SimpleScriptV2_Any :: Property
prop_golden_SimpleScriptV2_Any =
  goldenTestJsonValuePretty exampleSimpleScriptV2_Any
                            "test/Golden/Script/SimpleV2/any"

prop_golden_SimpleScriptV2_MofN :: Property
prop_golden_SimpleScriptV2_MofN =
  goldenTestJsonValuePretty exampleSimpleScriptV2_MofN
                            "test/Golden/Script/SimpleV2/atleast"


prop_roundtrip_SimpleScriptV1_JSON :: Property
prop_roundtrip_SimpleScriptV1_JSON =
  H.property $ do
    mss <- H.forAll $ genSimpleScript SimpleScriptV1
    H.tripping mss encode eitherDecode

prop_roundtrip_SimpleScriptV2_JSON :: Property
prop_roundtrip_SimpleScriptV2_JSON =
  H.property $ do
    mss <- H.forAll $ genSimpleScript SimpleScriptV2
    H.tripping mss encode eitherDecode

prop_roundtrip_ScriptData :: Property
prop_roundtrip_ScriptData =
  H.property $ do
    sData <- H.forAll genScriptData
    sData === fromAlonzoData (toAlonzoData sData)

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Typed.Script"
  [ testPropertyNamed "golden SimpleScriptV1 All"     "golden SimpleScriptV1 All"     prop_golden_SimpleScriptV1_All
  , testPropertyNamed "golden SimpleScriptV1 Any"     "golden SimpleScriptV1 Any"     prop_golden_SimpleScriptV1_Any
  , testPropertyNamed "golden SimpleScriptV1 MofN"    "golden SimpleScriptV1 MofN"    prop_golden_SimpleScriptV1_MofN
  , testPropertyNamed "golden SimpleScriptV2 All"     "golden SimpleScriptV2 All"     prop_golden_SimpleScriptV2_All
  , testPropertyNamed "golden SimpleScriptV2 Any"     "golden SimpleScriptV2 Any"     prop_golden_SimpleScriptV2_Any
  , testPropertyNamed "golden SimpleScriptV2 MofN"    "golden SimpleScriptV2 MofN"    prop_golden_SimpleScriptV2_MofN
  , testPropertyNamed "roundtrip SimpleScriptV1 JSON" "roundtrip SimpleScriptV1 JSON" prop_roundtrip_SimpleScriptV1_JSON
  , testPropertyNamed "roundtrip SimpleScriptV2 JSON" "roundtrip SimpleScriptV2 JSON" prop_roundtrip_SimpleScriptV2_JSON
  , testPropertyNamed "roundtrip ScriptData"          "roundtrip ScriptData"          prop_roundtrip_ScriptData
  ]
