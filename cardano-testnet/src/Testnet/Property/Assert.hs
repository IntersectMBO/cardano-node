{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Redundant return" -}

module Testnet.Property.Assert
  ( assertByDeadlineIOCustom
  , readJsonLines
  , assertChainExtended
  , getRelevantSlots
  ) where

import           Prelude hiding (lines)

import qualified Control.Concurrent as IO
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson (FromJSON (..), Value, (.:))
import           Data.Text (Text)
import           Data.Word (Word8)
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Internal.Test.Integration (IntegrationState)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import           Data.Maybe (mapMaybe)
import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Test.Base as H
import           Testnet.Runtime (NodeLoggingFormat (..))

newlineBytes :: Word8
newlineBytes = 10

readJsonLines :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m [Value]
readJsonLines fp = mapMaybe (Aeson.decode @Value) . LBS.split newlineBytes <$> H.evalIO (LBS.readFile fp)

fileJsonGrep :: FilePath -> (Value -> Bool) -> IO Bool
fileJsonGrep fp f = do
  lines <- LBS.split newlineBytes <$> LBS.readFile fp
  let jsons = mapMaybe (Aeson.decode @Value) lines
  return $ L.any f jsons

assertByDeadlineIOCustom
  :: (MonadTest m, MonadIO m, HasCallStack)
  => String -> DTC.UTCTime -> IO Bool -> m ()
assertByDeadlineIOCustom str deadline f = GHC.withFrozenCallStack $ do
  success <- H.evalIO f
  unless success $ do
    currentTime <- H.evalIO DTC.getCurrentTime
    if currentTime < deadline
      then do
        H.evalIO $ IO.threadDelay 1000000
        assertByDeadlineIOCustom str deadline f
      else do
        H.annotateShow currentTime
        H.failMessage GHC.callStack $ "Condition not met by deadline: " <> str

assertChainExtended :: (H.MonadTest m, MonadIO m)
  => DTC.UTCTime
  -> NodeLoggingFormat
  -> FilePath
  -> m ()
assertChainExtended deadline nodeLoggingFormat nodeStdoutFile =
  assertByDeadlineIOCustom "Chain not extended" deadline $ do
    case nodeLoggingFormat of
      NodeLoggingFormatAsText -> IO.fileContains "Chain extended, new tip" nodeStdoutFile
      NodeLoggingFormatAsJson -> fileJsonGrep nodeStdoutFile $ \v ->
                                    Aeson.parseMaybe (Aeson.parseJSON @(LogEntry Kind)) v == Just (LogEntry (Kind "TraceAddBlockEvent.AddedToCurrentChain"))

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
