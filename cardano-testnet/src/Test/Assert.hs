{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Redundant return" -}

module Test.Assert
  ( readJsonLines
  , assertChainExtended
  , getRelevantLeaderSlots
  ) where

import           Control.Applicative ((<*>))
import           Control.Monad (Monad (..))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson (FromJSON (..), Value, (.:))
import           Data.Bool (Bool (..))
import           Data.Eq (Eq (..))
import           Data.Function (($), (.))
import           Data.Functor ((<$>))
import           Data.Int (Int)
import           Data.Maybe (Maybe (..), mapMaybe)
import           Data.Ord (Ord (..))
import           Data.Text (Text)
import           Data.Word (Word8)
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Internal.Test.Integration (IntegrationState)
import           System.FilePath (FilePath)
import           System.IO (IO)
import           Test.Runtime (NodeLoggingFormat (..))
import           Text.Show (Show (..))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Test.Process as H

newlineBytes :: Word8
newlineBytes = 10

readJsonLines :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m [Value]
readJsonLines fp = mapMaybe (Aeson.decode @Value) . LBS.split newlineBytes <$> H.evalIO (LBS.readFile fp)

fileJsonGrep :: FilePath -> (Value -> Bool) -> IO Bool
fileJsonGrep fp f = do
  lines <- LBS.split newlineBytes <$> LBS.readFile fp
  let jsons = mapMaybe (Aeson.decode @Value) lines
  return $ L.any f jsons

assertChainExtended :: (H.MonadTest m, MonadIO m)
  => DTC.UTCTime
  -> NodeLoggingFormat
  -> FilePath
  -> m ()
assertChainExtended deadline nodeLoggingFormat nodeStdoutFile =
  H.assertByDeadlineIOCustom "Chain not extended" deadline $ do
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

data TraceNodeIsLeader = TraceNodeIsLeader
  { kind :: Text
  , slot :: Int
  } deriving (Eq, Show)

instance FromJSON TraceNodeIsLeader where
  parseJSON = Aeson.withObject "TraceNodeIsLeader" $ \v ->
    TraceNodeIsLeader
      <$> v .: "kind"
      <*> v .: "slot"

instance FromJSON Kind where
  parseJSON = Aeson.withObject "Kind" $ \v ->
    Kind <$> v .: "kind"

getRelevantLeaderSlots :: FilePath -> Int -> H.PropertyT (ReaderT IntegrationState (ResourceT IO)) [Int]
getRelevantLeaderSlots poolNodeStdoutFile slotLowerBound = do
  vs <- readJsonLines poolNodeStdoutFile
  leaderSlots <- H.noteShow
    $ L.map (slot . unLogEntry)
    $ Maybe.mapMaybe (Aeson.parseMaybe (Aeson.parseJSON @(LogEntry TraceNodeIsLeader)))
    vs
  relevantLeaderSlots <- H.noteShow
    $ L.filter       (>= slotLowerBound)
    leaderSlots
  return relevantLeaderSlots
