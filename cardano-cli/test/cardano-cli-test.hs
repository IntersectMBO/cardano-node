module Main where

import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import qualified Test.Cli.CliIntermediateFormat
import qualified Test.Cli.FilePermissions
import qualified Test.Cli.ITN
import qualified Test.Cli.JSON
import qualified Test.Cli.MultiAssetParsing
import qualified Test.Cli.Pioneers.Exercise1
import qualified Test.Cli.Pioneers.Exercise2
import qualified Test.Cli.Pioneers.Exercise3
import qualified Test.Cli.Pioneers.Exercise4
import qualified Test.Cli.Pipes
import qualified Test.Cli.Shelley.Run.Query
import qualified Test.Config.Mainnet

import           Hedgehog.Extras.Stock.OS (isWin32)
import           Hedgehog.Main (defaultMain)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  defaultMain
    [ Test.Cli.CliIntermediateFormat.tests
    , Test.Cli.FilePermissions.tests
    , Test.Cli.ITN.tests
    , Test.Cli.JSON.tests
    , Test.Cli.MultiAssetParsing.tests
    , ignoreOnWindows Test.Cli.Pipes.tests
    , Test.Cli.Pioneers.Exercise1.tests
    , Test.Cli.Pioneers.Exercise2.tests
    , Test.Cli.Pioneers.Exercise3.tests
    , Test.Cli.Pioneers.Exercise4.tests
    , Test.Cli.Shelley.Run.Query.tests
    , Test.Config.Mainnet.tests
    ]

ignoreOnWindows :: IO Bool -> IO Bool
ignoreOnWindows test =
  if isWin32
  then return True
  else test
