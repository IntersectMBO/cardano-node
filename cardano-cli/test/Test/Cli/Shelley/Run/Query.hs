module Test.Cli.Shelley.Run.Query
  ( tests
  ) where

import           Cardano.Slotting.Time (RelativeTime (..))
import           Hedgehog (Property, (===))

import qualified Cardano.CLI.Shelley.Run.Query as Q
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H

unit_percentage :: Property
unit_percentage = H.propertyOnce $ do
  Q.percentage (RelativeTime 10) (RelativeTime 1000) (RelativeTime 1000) === "100.00"
  Q.percentage (RelativeTime 10) (RelativeTime 990) (RelativeTime 1000) === "100.00"
  Q.percentage (RelativeTime 10) (RelativeTime 980) (RelativeTime 1000) === "99.00"
  Q.percentage (RelativeTime 10) (RelativeTime 500) (RelativeTime 1000) === "51.05"
  Q.percentage (RelativeTime 10) (RelativeTime 0) (RelativeTime 1000) === "1.10"
  return ()

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 6"
        [ ("prop_createZeroLovelaceTxOutTransaction", unit_percentage)
        ]
