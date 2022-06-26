{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Redundant return" -}

module Test.Assert
  ( readJsonLines
  , assertChainExtended
  , getRelevantLeaderSlots
  ) where

import           Control.Lens (to, (^.), (^?))
import           Control.Monad (Monad (..))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson (Value)
import           Data.Bool (Bool (..))
import           Data.Eq (Eq (..))
import           Data.Function (($), (.))
import           Data.Functor ((<$>))
import           Data.Int (Int)
import           Data.Maybe (mapMaybe)
import           Data.Ord (Ord (..))
import           Data.Word (Word8)
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Internal.Test.Integration (IntegrationState)
import           Prelude (fromIntegral)
import           System.FilePath (FilePath)
import           System.IO (IO)
import           Test.Runtime (NodeLoggingFormat (..))

import qualified Data.Aeson as J
import qualified Data.Aeson.Lens as J
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
readJsonLines fp = mapMaybe (J.decode @Value) . LBS.split newlineBytes <$> H.evalIO (LBS.readFile fp)

fileJsonGrep :: FilePath -> (Value -> Bool) -> IO Bool
fileJsonGrep fp f = do
  lines <- LBS.split newlineBytes <$> LBS.readFile fp
  let jsons = mapMaybe (J.decode @Value) lines
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
      NodeLoggingFormatAsJson -> fileJsonGrep nodeStdoutFile (\v -> v ^. J.key "data" . J.key "kind" . J._String == "")

getRelevantLeaderSlots :: FilePath -> Int -> H.PropertyT (ReaderT IntegrationState (ResourceT IO)) [Int]
getRelevantLeaderSlots poolNodeStdoutFile slotLowerBound = do
  vs <- readJsonLines poolNodeStdoutFile
  leaderSlots <- H.noteShow
    $ Maybe.mapMaybe (\v -> v ^? J.key "data" . J.key "val" . J.key "slot" . J._Integer. to fromIntegral)
    $ L.filter       (\v -> v ^. J.key "data" . J.key "val" . J.key "kind" . J._String == "TraceNodeIsLeader")
    vs
  relevantLeaderSlots <- H.noteShow
    $ L.filter       (>= slotLowerBound)
    leaderSlots
  return relevantLeaderSlots
