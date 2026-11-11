{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module GrowthTest (tests) where

import Cardano.Api qualified as Api
import Cardano.Benchmarking.TxFirehose.Growth.State
import Control.Exception (bracket, try)
import Control.Monad (forM_, void)
import Data.Aeson qualified as JSON
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Set qualified as Set
import Growth qualified as G
import Options.Applicative qualified as O
import System.Directory
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "growth drain"
    [ testCase "pending WAL recovery and rollback never allocate, normal resume can" $
        withFixture $ \path journal batch -> do
          let lane = head (lanes journal)
              delta = LaneDelta 1 0 lane{pending = [batch]}
          BSL.writeFile path (JSON.encode journal)
          -- Existing schema-3 WAL, including an interrupted final append.
          BSL.writeFile (path ++ ".wal") (JSON.encode delta <> "\n{\"nextRevision\":")
          recovered <- G.withJournal (options path) (identity journal) noInitialize pure
          revision recovered @?= 1
          pendingGrowth recovered @?= countAsInteger batch
          remainingToAllocate True recovered @?= 0
          finished True recovered @?= False
          -- A rollback to the original anchor requires replay of the same recipe.
          observedPrefix (refs [anchor lane]) (head (lanes recovered)) @?= Right 0
          pending (head (lanes recovered)) @?= [batch]
          waiting <- right $ reconcile (refs [anchor lane]) recovered
          waiting @?= recovered
          remainingToAllocate True waiting @?= 0
          done <- right $ reconcile (refs [change batch]) waiting
          confirmed done @?= countAsInteger batch
          pendingGrowth done @?= 0
          finished True done @?= True
          finished False done @?= False
          remainingToAllocate False done @?= target (identity journal) - countAsInteger batch
          identity done @?= identity journal
          -- Drain is not persisted: the same checkpoint resumes in ordinary mode.
          BSL.writeFile path (JSON.encode done)
          BSL.writeFile (path ++ ".wal") ""
          resumed <- G.withJournal (options path){G.drain = False} (identity journal) noInitialize pure
          resumed @?= done
    , testCase "already drained below target, including an untouched prefunded journal" $
        withFixture $ \path journal _ -> do
          forM_ [journal, journal{confirmed = 10}] $ \j -> do
            BSL.writeFile path (JSON.encode j)
            BSL.writeFile (path ++ ".wal") ""
            loaded <- G.withJournal (options path) (identity j) noInitialize pure
            stable <- right $ reconcile (refs (map anchor (lanes loaded))) loaded
            finished True stable @?= True
            finished False stable @?= False
            remainingToAllocate True stable @?= 0
    , testCase "drain at the original target is still a drain stopping condition" $
        withFixture $ \_ journal _ -> do
          let done = journal{confirmed = target (identity journal)}
          finished True done @?= True
          finished False done @?= True
    , testCase "lane initialization must become immutable, even with zero net growth" $
        withFixture $ \_ journal batch -> do
          let lane = (head (lanes journal)){anchor = change batch, finalizedAnchor = False}
              setup = journal{seeded = False, seed = Just (SeedPlan (anchor (head (lanes journal))) 5), lanes = [lane]}
          pendingGrowth setup @?= 0
          finished True setup @?= False
          waiting <- right $ reconcile Set.empty setup
          waiting @?= setup
          remainingToAllocate True waiting @?= 0
          done <- right $ reconcile (refs [anchor lane]) waiting
          seeded done @?= True
          confirmed done @?= 0
          finished True done @?= True
          -- Splitting into multiple lanes counts only the setup's net growth.
          let otherLane = lane{anchor = anchor (head (lanes journal))}
              split = setup{lanes = [lane, otherLane]}
          pendingGrowth split @?= 1
          partial <- right $ reconcile (refs [anchor lane]) split
          finished True partial @?= False
          splitDone <- right $ reconcile (refs [anchor lane, anchor otherLane]) partial
          confirmed splitDone @?= 1
          finished True splitDone @?= True
    , testCase "missing checkpoint creates no directory, checkpoint, WAL or lock" $
        withFixture $ \path journal _ -> do
          let missing = path </> "missing" </> "journal.json"
          expectFailure $ G.withJournal (options missing) (identity journal) noInitialize pure
          doesPathExist path >>= (@?= False)
    , testCase "missing WAL and orphaned WAL are rejected without altering files" $
        withFixture $ \path journal _ -> do
          BSL.writeFile path (JSON.encode journal)
          expectFailure $ G.withJournal (options path) (identity journal) noInitialize pure
          BSL.readFile path >>= (@?= JSON.encode journal)
          doesPathExist (path ++ ".wal") >>= (@?= False)
          doesPathExist (path ++ ".lock") >>= (@?= False)
          removeFile path
          BSL.writeFile (path ++ ".wal") "orphaned"
          expectFailure $ G.withJournal (options path) (identity journal) noInitialize pure
          doesPathExist path >>= (@?= False)
          BSL.readFile (path ++ ".wal") >>= (@?= "orphaned")
          doesPathExist (path ++ ".lock") >>= (@?= False)
    , testCase "all original identity checks still reject before recovery can write" $
        withFixture $ \path journal _ -> do
          let ident = identity journal
          BSL.writeFile path (JSON.encode journal)
          BSL.writeFile (path ++ ".wal") "{\"nextRevision\":"
          forM_
            [ ident{target = target ident + 1}
            , ident{outputsRequested = outputsRequested ident + 1}
            , ident{retainedAmount = retainedAmount ident + 1}
            , ident{fundingAddress = "different funding key"}
            , ident{retainedAddress = "different retained key"}
            , ident{network = "other network"}
            , ident{era = "other era"}
            , ident{colour = "other colour"}
            , ident{lanesRequested = 2}
            , ident{schema = 4}
            ]
            $ \changed -> do
              expectFailure $ G.withJournal (options path) changed noInitialize (const noInitialize)
              BSL.readFile path >>= (@?= JSON.encode journal)
              BSL.readFile (path ++ ".wal") >>= (@?= "{\"nextRevision\":")
    , testCase "CLI accepts drain but rejects drain plus dry-run before state creation" $
        withFixture $ \path journal _ -> do
          let args =
                [ "--grow-by"
                , "100"
                , "--growth-state"
                , path
                , "--retained-verification-key-file"
                , "retained.vkey"
                , "--growth-drain"
                ]
          case O.execParserPure O.defaultPrefs (O.info G.parser mempty) args of
            O.Success opts -> do
              G.drain opts @?= True
              G.dryRun opts @?= False
              G.validateOptions opts
            _ -> assertFailure "drain CLI should parse"
          case O.execParserPure O.defaultPrefs (O.info G.parser mempty) (args ++ ["--growth-dry-run"]) of
            O.Success opts -> do
              expectFailure $ G.validateOptions opts
              expectFailure $ G.withJournal opts (identity journal) noInitialize pure
              doesPathExist path >>= (@?= False)
            _ -> assertFailure "switches should parse, then fail option validation"
    ]

options :: FilePath -> G.Options
options path = G.Options 100 path "retained.vkey" 1 20 1000000 64 1 False True

noInitialize :: IO Journal
noInitialize = assertFailure "drain must not initialize or enter a rejected journal" >> fail "unreachable"

expectFailure :: IO a -> IO ()
expectFailure action = do
  result <- try (void action)
  case result of
    Left (ExitFailure _) -> pure ()
    Left other -> assertFailure ("Unexpected exit: " ++ show other)
    Right () -> assertFailure "Expected rejection"

right :: Show e => Either e a -> IO a
right = either (\err -> assertFailure (show err) >> fail "unreachable") pure

refs :: [SavedFund] -> Set.Set Api.TxIn
refs = Set.fromList . map reference

countAsInteger :: Batch -> Integer
countAsInteger = toInteger . count

withFixture :: (FilePath -> Journal -> Batch -> IO a) -> IO a
withFixture action = bracket makeTemp removeDirectoryRecursive $ \dir -> do
  txid <- right $ Api.deserialiseFromRawBytes Api.AsTxId (BS.replicate 32 1)
  let fund ix = SavedFund (Api.TxIn txid (Api.TxIx ix)) 1000000000
      ident = Identity 3 "network" "era" "funding" "retained" "colour" 100 1 20 1000000
      journal = Journal 0 ident Nothing True 0 [Lane (fund 0) [] True]
      batch = Batch 20 5 (fund 1)
  action (dir </> "journal.json") journal batch
 where
  makeTemp = do
    tmp <- getTemporaryDirectory
    (path, handle) <- openTempFile tmp "firehose-drain-test"
    hClose handle
    removeFile path
    createDirectory path
    pure path
