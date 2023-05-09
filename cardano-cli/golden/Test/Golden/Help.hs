{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Redundant id" -}

module Test.Golden.Help
  ( helpTests
  ) where

import           Prelude hiding (lines)

import           Control.Monad (forM_)
import           Data.Text (Text)
import           Hedgehog (Property)
import           System.FilePath ((</>))
import           Test.Hedgehog.Golden (diffVsGoldenFile)
import           Test.OptParse (execCardanoCLI, propertyOnce)
import           Text.Regex (Regex, mkRegex, subRegex)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Test.OptParse as H

ansiRegex :: Regex
ansiRegex = mkRegex "\\[[0-9]+m"

filterAnsi :: String -> String
filterAnsi line = subRegex ansiRegex stripped ""
  where stripped = filter (/= '\ESC') line

{- HLINT ignore "Use camelCase" -}

  -- . Text.takeWhile (\c -> Char.isAlphaNum c || c == '-')

extractCmd :: Text -> [Text]
extractCmd = id
  . takeWhile nonSwitch
  . Text.split Char.isSpace
  . Text.strip
  where nonSwitch :: Text -> Bool
        nonSwitch s =
          case Text.unpack (Text.take 1 s) of
            (c:_) -> Char.isAlpha c
            [] -> False

-- | Test that converting a @cardano-address@ Byron signing key yields the
-- expected result.
golden_HelpAll :: Property
golden_HelpAll =
  propertyOnce . H.moduleWorkspace "help" $ \_ -> do
    helpFp <- H.note "golden/files/golden/help.cli"

    help <- filterAnsi <$> execCardanoCLI
      [ "help"
      ]

    diffVsGoldenFile help helpFp

third :: (a, b, c) -> c
third (_, _, c) = c

golden_HelpCmds :: Property
golden_HelpCmds =
  propertyOnce . H.moduleWorkspace "help-commands" $ \_ -> do
    help <- filterAnsi <$> execCardanoCLI
      [ "help"
      ]

    let lines = Text.lines $ Text.pack help
    let usages = List.filter (not . null) $ extractCmd . Text.drop 19 <$> List.filter (Text.isPrefixOf "Usage: cardano-cli ") lines

    forM_ usages $ \usage -> do
      H.noteShow_ usage

      let expectedCmdHelpFp = "golden/files/golden/help" </> Text.unpack (Text.intercalate "_" usage) <> ".cli"

      cmdHelp <- filterAnsi . third <$> H.execDetailCardanoCli (fmap Text.unpack usage)

      diffVsGoldenFile cmdHelp expectedCmdHelpFp

helpTests :: IO Bool
helpTests =
  H.checkSequential $ H.Group "Help"
    [ ( "golden_HelpAll"
      , Test.Golden.Help.golden_HelpAll
      )
    , ( "golden_HelpCmds"
      , Test.Golden.Help.golden_HelpCmds
      )
    ]
