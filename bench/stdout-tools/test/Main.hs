{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main (main) where

import           Data.Either (fromRight)

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

-- Create what profiles use and compare with previous profiles outputs.
reducers :: Tasty.TestTree
reducers = Tasty.testGroup
  "Cardano.Tracer.Reducer"
  [ testCase "CountLines" $ do
      fp <- Paths.getDataFileName "data/500-FLSLCP.stdout"
      let reducer = Reducer.CountLines
      ans <- Log.lineFoldl'
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
      ans <- Log.lineFoldl'
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
      ans <- Log.lineFoldl'
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
      let reducer = Reducer.ResourcesChanges Trace.resourcesHeap
      ans <- Log.lineFoldl'
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
  , testCase "UtxoSize" $ do
      fp <- Paths.getDataFileName "data/500-FLSLCP.stdout"
      let reducer = Reducer.UtxoSize
      ans <- Log.lineFoldl'
        (Reducer.reducerOf reducer)
        (Reducer.initialOf reducer)
        fp
      assertEqual
        ("UTxO set size changes (\"" ++ fp ++ "\")")
        (snd ans)
        {--
          grep -E "^{.*" bench/stdout-tools/data/500-FLSLCP.stdout | grep '"ns":"Forge.Loop.StartLeadershipCheckPlus"' | jq .data.utxoSize | uniq
          41002003
          41002005
          41002063
          41003716
          41005746
          41007776
          41009806
          41011836
          41013866
          grep -E "^{.*" bench/stdout-tools/data/500-FLSLCP.stdout | grep '"ns":"Forge.Loop.StartLeadershipCheckPlus"' | jq '(.data.utxoSize | tostring) + " " + .at'
          "41002003 2024-04-05T23:13:43.425867818Z"
          ...
          "41002003 2024-04-05T23:17:55.000688231Z"
          "41002005 2024-04-05T23:17:56.00030498Z"
          ...
          "41002005 2024-04-05T23:18:53.00094089Z"
          "41002063 2024-04-05T23:18:54.000842713Z"
          ...
          "41002063 2024-04-05T23:19:08.000337381Z"
          "41003716 2024-04-05T23:19:09.000213217Z"
          ...
          "41003716 2024-04-05T23:20:29.000247453Z"
          "41005746 2024-04-05T23:20:30.001060768Z"
          ...
          "41005746 2024-04-05T23:20:35.000530287Z"
          "41007776 2024-04-05T23:20:36.001087979Z"
          ...
          "41007776 2024-04-05T23:21:33.001090265Z"
          "41009806 2024-04-05T23:21:34.002470652Z"
          ...
          "41009806 2024-04-05T23:21:39.00082744Z"
          "41011836 2024-04-05T23:21:40.000691826Z"
          ...
          "41011836 2024-04-05T23:21:46.000887265Z"
          "41013866 2024-04-05T23:21:47.00015339Z"
          ...
          "41013866 2024-04-05T23:22:02.000434745Z"
        --}
        (Seq.fromList [
            ( fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:13:43.425867818Z"
            , 41002003
            )
          , ( fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:17:56.00030498Z"
            , 41002005
            )
          , ( fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:18:54.000842713Z"
            , 41002063
            )
          , ( fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:19:09.000213217Z"
            , 41003716
            )
          , ( fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:20:30.001060768Z"
            , 41005746
            )
          , ( fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:20:36.001087979Z"
            , 41007776
            )
          , ( fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:21:34.002470652Z"
            , 41009806
            )
          , ( fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:21:40.000691826Z"
            , 41011836
            )
          , ( fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:21:47.00015339Z"
            , 41013866
            )
        ])
  , testCase "Silences" $ do
      fp <- Paths.getDataFileName "data/500-FLSLCP.stdout"
      let reducer = Reducer.Silences 2
      ans <- Log.lineFoldl'
        (Reducer.reducerOf reducer)
        (Reducer.initialOf reducer)
        fp
      assertEqual
        ("Show silences equal or greater than 2 seconds (\"" ++ fp ++ "\")")
        (snd ans)
        {--
          grep -E "^{.*" bench/stdout-tools/data/500-FLSLCP.stdout | jq -r --compact-output '.at[:20] | strptime("%Y-%m-%dT%H:%M:%S.")[4:6]' | uniq
          [1,44]
          ...
          [1,55]
          [2,13]
          ...
          [5,26]
          [5,44]
          ...
          [8,48]
          [9,10]
          ...
        --}
        (Seq.fromList [
            ( 18.173283409
            , fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:01:55.14162815Z"
            , fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:02:13.314911559Z"
            )
          , ( 18.175087624
            , fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:05:26.18240123Z"
            , fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:05:44.357488854Z"
            )
          , ( 22.01487267
            , fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:08:48.243125617Z"
            , fromRight (error "parseUTCTime") $
                ParseTime.parseUTCTime "2024-04-05T23:09:10.257998287Z"
            )
        ])
  ]
