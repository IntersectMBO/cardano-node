{-# LANGUAGE OverloadedStrings #-}

-- | Test the JSON encoding of instances in Cardano.Tracing.OrphanInstances.Shelley
--
-- The golden files are stored in the path given by 'addPrefix'.
--
-- If a new test is added and no golden file exists for it it will be created.
-- This new file needs to be commited.
--
-- For now we added a couple of representative examples, however the tests are
-- not exhaustive.
--
-- The examples can be best viewed using a tool like 'jq'.
module Test.Cardano.Tracing.OrphanInstances.Shelley (tests) where

import           Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import           Cardano.Ledger.Alonzo.Rules (FailureDescription (..), TagMismatchDescription (..))
import           Cardano.Ledger.Alonzo.Tx (IsValid (..))
import           Cardano.Ledger.BaseTypes (SlotNo (..), StrictMaybe (..))
import           Cardano.Tracing.OrphanInstances.Shelley ()

import qualified Data.Aeson as Aeson
import           Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.List.NonEmpty as NE

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H.Base
import qualified Hedgehog.Extras.Test.Golden as H.Golden
import           Hedgehog.Internal.Property (PropertyName (PropertyName))

tests :: IO Bool
tests = H.checkSequential
      $ H.Group "Shelley JSON instances"
        [ test
            (  validityInterval
            , "validityInterval.json")
        , test
            (  isValid
            , "isValid.json")
        , test
            (  failureDescription
            , "failureDescription.json")
        , test
            (  tagMismatchDescription
            , "tagMismatchDescription.json")
        ]
  where
    test (actualValue, goldenBaseName) =
        (PropertyName goldenBaseName, goldenTestJSON actualValue goldenBaseName)

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

validityInterval :: [ValidityInterval]
validityInterval =
  [ ValidityInterval
      { invalidBefore = SNothing
      , invalidHereafter = SNothing
      }
  , ValidityInterval
      { invalidBefore = SJust (SlotNo 12345)
      , invalidHereafter = SNothing
      }
  , ValidityInterval
      { invalidBefore = SNothing
      , invalidHereafter = SJust (SlotNo 12354)
      }
  , ValidityInterval
      { invalidBefore = SJust (SlotNo 12345)
      , invalidHereafter = SJust (SlotNo 12354)
      }
  ]

isValid :: [IsValid]
isValid =
  [ IsValid True
  , IsValid False
  ]

failureDescription :: [FailureDescription]
failureDescription =
  [ PlutusFailure "A description" "A reconstruction"
  ]

tagMismatchDescription :: [TagMismatchDescription]
tagMismatchDescription =
  [ PassedUnexpectedly
  , FailedUnexpectedly (NE.fromList failureDescription)
  ]

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
