{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Cardano.Api.Examples
  ( exampleAllShelley
  , exampleAnyShelley
  , exampleMofNShelley
  , exampleAllAllegra
  , exampleAnyAllegra
  , exampleMofNAllegra
  , exampleAllMary
  , exampleAnyMary
  , exampleMofNMary
  ) where

import           Cardano.Api as Api


exampleAllShelley :: ScriptInEra ShelleyEra
exampleAllShelley =
    ScriptInEra SimpleScriptV1InShelley
                (SimpleScript SimpleScriptV1 exampleAllSimpleScriptV1)

exampleAnyShelley :: ScriptInEra ShelleyEra
exampleAnyShelley =
    ScriptInEra SimpleScriptV1InShelley
                (SimpleScript SimpleScriptV1 exampleAnySimpleScriptV1)

exampleMofNShelley :: ScriptInEra ShelleyEra
exampleMofNShelley =
    ScriptInEra SimpleScriptV1InShelley
                (SimpleScript SimpleScriptV1 exampleMofNSimpleScriptV1)

exampleAllAllegra :: ScriptInEra AllegraEra
exampleAllAllegra =
    ScriptInEra SimpleScriptV2InAllegra
                (SimpleScript SimpleScriptV2 exampleAllSimpleScriptV2)

exampleAnyAllegra :: ScriptInEra AllegraEra
exampleAnyAllegra =
    ScriptInEra SimpleScriptV2InAllegra
                (SimpleScript SimpleScriptV2 exampleAnySimpleScriptV2)

exampleMofNAllegra :: ScriptInEra AllegraEra
exampleMofNAllegra =
    ScriptInEra SimpleScriptV2InAllegra
                (SimpleScript SimpleScriptV2 exampleMofNSimpleScriptV2)

exampleAllMary :: ScriptInEra MaryEra
exampleAllMary =
    ScriptInEra SimpleScriptV2InMary
                (SimpleScript SimpleScriptV2 exampleAllSimpleScriptV2)

exampleAnyMary :: ScriptInEra MaryEra
exampleAnyMary =
    ScriptInEra SimpleScriptV2InMary
                (SimpleScript SimpleScriptV2 exampleAnySimpleScriptV2)

exampleMofNMary :: ScriptInEra MaryEra
exampleMofNMary =
    ScriptInEra SimpleScriptV2InMary
                (SimpleScript SimpleScriptV2 exampleMofNSimpleScriptV2)


exampleAllSimpleScriptV1 :: SimpleScript SimpleScriptV1
exampleAllSimpleScriptV1 =
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


exampleAnySimpleScriptV1 :: SimpleScript SimpleScriptV1
exampleAnySimpleScriptV1 =
  RequireAnyOf
    [ RequireSignature "d92b712d1882c3b0f75b6f677e0b2cbef4fbc8b8121bb9dde324ff09"
    , RequireSignature "4d780ed1bfc88cbd4da3f48de91fe728c3530d662564bf5a284b5321"
    , RequireSignature "3a94d6d4e786a3f5d439939cafc0536f6abc324fb8404084d6034bf8"
    , RequireSignature "b12e094d1db7c0fba5121f22db193d0060efed8be43654f861bb68ae"
    , RequireSignature "9be49d56442b4b8b16cab4e43e238bbdefc6c803d554c82fcd5facc3"
    , RequireSignature "622be5fab3b5c3f371a50a535e4d3349c942a98cecee93b24e2fd11d"
    ]

exampleMofNSimpleScriptV1 :: SimpleScript SimpleScriptV1
exampleMofNSimpleScriptV1 =
  RequireMOf 2
    [ RequireSignature "2f3d4cf10d0471a1db9f2d2907de867968c27bca6272f062cd1c2413"
    , RequireSignature "f856c0c5839bab22673747d53f1ae9eed84afafb085f086e8e988614"
    , RequireSignature "b275b08c999097247f7c17e77007c7010cd19f20cc086ad99d398538"
    , RequireSignature "686024aecb5884d73a11b9ae4e63931112ba737e878d74638b78513a"
    ]

exampleAllSimpleScriptV2 :: SimpleScript SimpleScriptV2
exampleAllSimpleScriptV2 =
  RequireAllOf
    [ RequireSignature "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
    , RequireTimeBefore TimeLocksInSimpleScriptV2 (SlotNo 42)
    ]


exampleAnySimpleScriptV2 :: SimpleScript SimpleScriptV2
exampleAnySimpleScriptV2 =
  RequireAnyOf
    [ RequireSignature "d92b712d1882c3b0f75b6f677e0b2cbef4fbc8b8121bb9dde324ff09"
    , RequireTimeAfter TimeLocksInSimpleScriptV2 (SlotNo 42)
    ]

exampleMofNSimpleScriptV2 :: SimpleScript SimpleScriptV2
exampleMofNSimpleScriptV2 =
  RequireMOf 1
    [ RequireSignature "2f3d4cf10d0471a1db9f2d2907de867968c27bca6272f062cd1c2413"
    , RequireSignature "f856c0c5839bab22673747d53f1ae9eed84afafb085f086e8e988614"
    , RequireTimeBefore TimeLocksInSimpleScriptV2 (SlotNo 42)
    ]

