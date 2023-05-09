{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Help
  ( helpTests
  ) where

import           Hedgehog (Property)
import           Test.Hedgehog.Golden (diffVsGoldenFile)
import           Test.OptParse (execCardanoCLI, propertyOnce)

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H

import           Text.Regex (Regex, mkRegex, subRegex)

ansiRegex :: Regex
ansiRegex = mkRegex "\\[[0-9]+m"

filterAnsi :: String -> String
filterAnsi line = subRegex ansiRegex stripped ""
  where stripped = filter (/= '\ESC') line

{- HLINT ignore "Use camelCase" -}

-- | Test that converting a @cardano-address@ Byron signing key yields the
-- expected result.
golden_Help :: Property
golden_Help =
  propertyOnce . H.moduleWorkspace "help" $ \_ -> do
    helpFp <- H.note "golden/files/golden/help.cli"

    help <- filterAnsi <$> execCardanoCLI
      [ "help"
      ]

    diffVsGoldenFile help helpFp

helpTests :: IO Bool
helpTests =
  H.checkSequential $ H.Group "Key command group"
    [ ( "golden_Help"
      , Test.Golden.Help.golden_Help
      )
    ]
