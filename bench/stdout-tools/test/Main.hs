{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main (main) where

import           Data.Either (fromRight)

-- package: text.
import qualified Data.Text as Text
-- package: text-iso8601-0.1
import qualified Data.Time.FromText as ParseTime
-- package: containers.
import qualified Data.Sequence as Seq

import qualified Test.Tasty           as Tasty
import           Test.Tasty.HUnit

import qualified Data.Log as Log
import qualified Cardano.Tracer.Trace as Trace
import qualified Cardano.Tracer.Reducer as Reducer

import qualified Paths_stdout_tools as Paths

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =  Tasty.testGroup "stdout-tools"
  [
      reducers
  ]

-- Allow to `fold'` through the log file but in JSON format.
lineFoldl' :: (a -> (Either Text.Text Trace.Trace) -> a) -> a -> FilePath -> IO a
lineFoldl' f initialAcc filePath = do
  Log.lineFoldl'
    (\acc textLine -> f acc (Trace.fromJson textLine))
    initialAcc
    filePath

-- Create what profiles use and compare with previous profiles outputs.
reducers :: Tasty.TestTree
reducers = Tasty.testGroup
  "Cardano.Tracer.Reducer"
  [ testCase "CountLines" $ do
      fp <- Paths.getDataFileName "data/500-FLSLCP.stdout"
      let reducer = Reducer.CountLines
      ans <- lineFoldl'
        (Reducer.reducerOf reducer)
        (Reducer.initialOf reducer)
        fp
      assertEqual
        ("Count lines (\"" ++ fp ++ "\")")
        ans
        {--
          wc -l bench/stdout-tools/data/500-FLSLCP.stdout
          15383 bench/stdout-tools/data/500-FLSLCP.stdout
        --}
        15383
  ,  testCase "CountTraces" $ do
      fp <- Paths.getDataFileName "data/500-FLSLCP.stdout"
      let reducer = Reducer.CountTraces
      ans <- lineFoldl'
        (Reducer.reducerOf reducer)
        (Reducer.initialOf reducer)
        fp
      assertEqual
        ("Count provably \"valid\" traces (\"" ++ fp ++ "\")")
        ans
        {--
          grep -E "^{.*" bench/stdout-tools/data/500-FLSLCP.stdout | wc -l
          15354
        --}
        15354
  , testCase "CountNS" $ do
      fp <- Paths.getDataFileName "data/500-FLSLCP.stdout"
      let reducer = Reducer.CountNS "Forge.Loop.StartLeadershipCheckPlus"
      ans <- lineFoldl'
        (Reducer.reducerOf reducer)
        (Reducer.initialOf reducer)
        fp
      assertEqual
        ("Count Forge.Loop.StartLeadershipCheckPlus (\"" ++ fp ++ "\")")
        ans
        {--
          grep -E "^{.*" bench/stdout-tools/data/500-FLSLCP.stdout | grep '"ns":"Forge.Loop.StartLeadershipCheckPlus"' | wc -l
          500
        --}
        500
  , testCase "ResourcesChanges" $ do
      fp <- Paths.getDataFileName "data/500-FLSLCP.stdout"
      let reducer = Reducer.ResourcesChanges (Trace.resourcesHeap)
      ans <- lineFoldl'
        (Reducer.reducerOf reducer)
        (Reducer.initialOf reducer)
        fp
      assertEqual
        ("Heap memory changes (\"" ++ fp ++ "\")")
        (snd ans)
        {--
          grep -E "^{.*" bench/stdout-tools/data/500-FLSLCP.stdout | grep '"ns":"Resources"' | jq .data.Heap | uniq
          41894805504
          46861910016
          grep -E "^{.*" bench/stdout-tools/data/500-FLSLCP.stdout | grep '"ns":"Resources"' | jq '(.data.Heap | tostring) + " " + .at'
          "41894805504 2024-04-05T23:01:44.095438578Z"
          ...
          "41894805504 2024-04-05T23:01:55.14162815Z"
          "46861910016 2024-04-05T23:02:13.314911559Z"
          ...
          "46861910016 2024-04-05T23:22:01.043941852Z"
        --}
        (Seq.fromList [
            ( fromRight (error "") $
                ParseTime.parseUTCTime "2024-04-05T23:01:44.095438578Z"
            , 41894805504
            )
          , ( fromRight (error "") $
                ParseTime.parseUTCTime "2024-04-05T23:02:13.314911559Z"
            , 46861910016
            )
        ])
  ]
