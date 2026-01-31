{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Testnet.Property.Assert
  ( assertByDeadlineIOCustom
  , readJsonLines
  , assertChainExtended
  , getRelevantSlots
  , assertExpectedSposInLedgerState
  , assertErasEqual
  ) where


import           Cardano.Api hiding (Value)

import           Prelude hiding (lines)

import qualified Control.Concurrent as IO
import           Control.Monad
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson (Value, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import           Data.Maybe (mapMaybe)
import qualified Data.Maybe as Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Time.Clock as DTC
import           Data.Type.Equality
import           Data.Word (Word8)
import           GHC.Stack as GHC
import           RIO (throwString)

import           Testnet.Process.RunIO
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import           Hedgehog.Extras.Internal.Test.Integration (IntegrationState)
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Test.Base as H
import           Hedgehog.Extras.Test.Process (ExecConfig)

newlineBytes :: Word8
newlineBytes = 10

readJsonLines :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m [Value]
readJsonLines fp = withFrozenCallStack $ mapMaybe (Aeson.decode @Value) . LBS.split newlineBytes <$> H.evalIO (LBS.readFile fp)

fileJsonGrep :: FilePath -> (Value -> Bool) -> IO Bool
fileJsonGrep fp f = do
  lines <- LBS.split newlineBytes <$> LBS.readFile fp
  let jsons = mapMaybe (Aeson.decode @Value) lines
  return $ L.any f jsons

assertByDeadlineIOCustom
  :: (MonadIO m, HasCallStack)
  => String -> DTC.UTCTime -> IO Bool -> m ()
assertByDeadlineIOCustom str deadline f = withFrozenCallStack $ do
  success <- liftIOAnnotated f
  unless success $ do
    currentTime <- liftIOAnnotated DTC.getCurrentTime
    if currentTime < deadline
      then do
        liftIOAnnotated $ IO.threadDelay 1_000_000
        assertByDeadlineIOCustom str deadline f
      else do
        throwString $ "Condition not met by deadline: " <> str

-- | A sanity check that confirms that there are the expected number of SPOs in the ledger state
assertExpectedSposInLedgerState
  :: HasCallStack
  => MonadIO m
  => FilePath -- ^ Stake pools query output filepath
  -> NumPools
  -> ExecConfig
  -> m ()
assertExpectedSposInLedgerState output (NumPools numExpectedPools) execConfig = withFrozenCallStack $ do
  void $ execCli' execConfig
      [ "latest", "query", "stake-pools"
      , "--out-file", output
      ]

  ePoolSet <-  liftIOAnnotated (Aeson.eitherDecodeFileStrict' @(Set PoolId) output)
  case ePoolSet of
    Left err -> 
      throwString $ "Failed to decode stake pools from ledger state: " <> err
    Right poolSet -> do
      let numPoolsInLedgerState = Set.size poolSet
      unless (numPoolsInLedgerState == numExpectedPools) $
        throwString $ unlines 
          [ "Expected number of stake pools not found in ledger state"
          , "Expected: ", show numExpectedPools
          , "Actual: ", show numPoolsInLedgerState
          ]

assertChainExtended
  :: HasCallStack
  => MonadIO m
  => DTC.UTCTime
  -> NodeLoggingFormat
  -> TestnetNode
  -> m ()
assertChainExtended deadline nodeLoggingFormat TestnetNode{nodeName, nodeStdout} = withFrozenCallStack $
  assertByDeadlineIOCustom ("Chain not extended in " <> nodeName) deadline $ do
    case nodeLoggingFormat of
      NodeLoggingFormatAsText -> IO.fileContains "Chain extended, new tip" nodeStdout
      NodeLoggingFormatAsJson -> fileJsonGrep nodeStdout $ \v ->
                                    Aeson.parseMaybe (Aeson.parseJSON @(LogEntry Kind)) v == Just (LogEntry (Kind "AddedToCurrentChain"))

newtype LogEntry a = LogEntry
  { unLogEntry :: a
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (LogEntry a) where
  parseJSON = Aeson.withObject "LogEntry" $ \v ->
    LogEntry <$> v .: "data"

newtype Kind = Kind
  { kind :: Text
  } deriving (Eq, Show)

data TraceNode
  = TraceNode
    { isLeader :: !Bool
    , kind     :: !Text
    , slot     :: !Int
    }
  deriving (Eq, Show)

instance FromJSON TraceNode where
  parseJSON = Aeson.withObject "TraceNode" $ \v -> do
    kind' <- v .: "val" >>= (.: "kind")
    let slotP = v .: "val" >>= (.: "slot")
    case kind' of
      "TraceNodeIsLeader" -> TraceNode True kind' <$> slotP
      "TraceNodeNotLeader" -> TraceNode False kind' <$> slotP
      _ -> fail $ "Expected kind was TraceNodeIsLeader, found " <> show kind' <> "instead"

instance FromJSON Kind where
  parseJSON = Aeson.withObject "Kind" $ \v ->
    Kind <$> v .: "kind"

getRelevantSlots :: FilePath -> Int -> H.PropertyT (ReaderT IntegrationState (ResourceT IO)) ([Int], [Int])
getRelevantSlots poolNodeStdoutFile slotLowerBound = do
  vs <- readJsonLines poolNodeStdoutFile
  let slots = L.map unLogEntry $ Maybe.mapMaybe (Aeson.parseMaybe Aeson.parseJSON) vs

  leaderSlots <- H.noteShow
    $ map slot
    $ filter isLeader slots
  notLeaderSlots <- H.noteShow
    $ map slot
    $ filter (not . isLeader) slots

  relevantLeaderSlots <- H.noteShow
    $ L.filter       (>= slotLowerBound)
    leaderSlots
  relevantNotLeaderSlots <- H.noteShow
    $ L.filter       (>= slotLowerBound)
    notLeaderSlots

  pure (relevantLeaderSlots, relevantNotLeaderSlots)

assertErasEqual
  :: HasCallStack
  => TestEquality eon
  => Show (eon expectedEra)
  => Show (eon receivedEra)
  => MonadError String m
  => eon expectedEra
  -> eon receivedEra
  -> m (expectedEra :~: receivedEra)
assertErasEqual expectedEra receivedEra = withFrozenCallStack $
  case testEquality expectedEra receivedEra of
    Just Refl -> pure Refl
    Nothing ->
      throwError $ "Eras mismatch! expected: " <> show expectedEra <> ", received era: " <> show receivedEra
