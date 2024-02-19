{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.State.Historical
  ( BlockchainHistory (..)
  , DataName (..)
  , History
  , HistoricalData
  , HistoricalPoint
  , HistoricalPoints
  , POSIXTime
  , ResourcesHistory (..)
  , TransactionsHistory (..)
  , ValueH (..)
  , addHistoricalData
  , getHistoricalData
  , initBlockchainHistory
  , initResourcesHistory
  , initTransactionsHistory
  , readValueI
  , readValueD
  ) where

import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types (NodeId)

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Control.Monad (mzero)
import qualified Data.ByteString.Char8 as BSC
import           Data.Csv (FromField (..), ToField (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text, isInfixOf)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Read (decimal, double)
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)
import           Text.Printf (printf)

-- | A lot of information received from the node is useful as historical data.
--   It means that such an information should be displayed on time charts,
--   where X axis is a time in UTC. An example: resource metrics, chain information,
--   tx information, etc.
type POSIXTime = Word64

data ValueH
  = ValueD Double
  | ValueI Int
  deriving (Eq, Ord)

instance Show ValueH where
  show (ValueD d) = show d
  show (ValueI i) = show i

instance Num ValueH where
  (+) (ValueI i1) (ValueI i2) = ValueI (i1 + i2)
  (+) (ValueD d1) (ValueD d2) = ValueD (d1 + d2)
  (+) (ValueI i1) (ValueD d1) = ValueD (fromIntegral i1 + d1)
  (+) (ValueD d1) (ValueI i1) = ValueD (fromIntegral i1 + d1)

  (-) (ValueI i1) (ValueI i2) = ValueI (i1 - i2)
  (-) (ValueD d1) (ValueD d2) = ValueD (d1 - d2)
  (-) (ValueI i1) (ValueD d1) = ValueD (fromIntegral i1 - d1)
  (-) (ValueD d1) (ValueI i1) = ValueD (fromIntegral i1 - d1)

  (*) (ValueI i1) (ValueI i2) = ValueI (i1 * i2)
  (*) (ValueD d1) (ValueD d2) = ValueD (d1 * d2)
  (*) (ValueI i1) (ValueD d1) = ValueD (fromIntegral i1 * d1)
  (*) (ValueD d1) (ValueI i1) = ValueD (fromIntegral i1 * d1)

  abs (ValueI i) = ValueI (abs i)
  abs (ValueD d) = ValueD (abs d)

  signum (ValueI i) = ValueI (signum i)
  signum (ValueD d) = ValueD (signum d)

  fromInteger i = ValueI (fromInteger i)

type HistoricalPoint = (POSIXTime, ValueH)

instance FromField ValueH where
  parseField s =
    let t = decodeUtf8 s in
    if "." `isInfixOf` t
      then either (const mzero) (return . ValueD . fst) $ double t
      else either (const mzero) (return . ValueI . fst) $ decimal t

instance ToField ValueH where
  toField (ValueI i) = toField i
  toField (ValueD d) = BSC.pack $ printf "%.3f" d

type HistoricalPoints = Set HistoricalPoint

-- | Historical points for particular data.
data DataName
  = CPUData
  | MemoryData
  | GCMajorNumData
  | GCMinorNumData
  | GCLiveMemoryData
  | CPUTimeGCData
  | CPUTimeAppData
  | ThreadsNumData
  -- Chain
  | ChainDensityData
  | SlotNumData
  | BlockNumData
  | SlotInEpochData
  | EpochData
  | NodeCannotForgeData
  | ForgedSlotLastData
  | NodeIsLeaderData
  | NodeIsNotLeaderData
  | ForgedInvalidSlotLastData
  | AdoptedSlotLastData
  | NotAdoptedSlotLastData
  | AboutToLeadSlotLastData
  | CouldNotForgeSlotLastData
  -- TX
  | TxsProcessedNumData
  | MempoolBytesData
  | TxsInMempoolData
  deriving (Eq, Ord, Read, Show)

type HistoricalData = Map DataName HistoricalPoints
type History        = TVar (Map NodeId HistoricalData)

newtype BlockchainHistory   = ChainHistory History
newtype ResourcesHistory    = ResHistory   History
newtype TransactionsHistory = TXHistory    History

initBlockchainHistory :: IO BlockchainHistory
initBlockchainHistory = ChainHistory <$> newTVarIO M.empty

initResourcesHistory :: IO ResourcesHistory
initResourcesHistory = ResHistory <$> newTVarIO M.empty

initTransactionsHistory :: IO TransactionsHistory
initTransactionsHistory = TXHistory <$> newTVarIO M.empty

addHistoricalData
  :: History
  -> NodeId
  -> UTCTime
  -> DataName
  -> ValueH
  -> IO ()
addHistoricalData history nodeId now dataName valueH = atomically $
  modifyTVar' history $ \currentHistory ->
    case M.lookup nodeId currentHistory of
      Nothing ->
        -- There is no historical data for this node yet.
        let firstPoint = S.singleton (utc2s now, valueH)
            newDataForNode = M.singleton dataName firstPoint
        in M.insert nodeId newDataForNode currentHistory
      Just dataForNode ->
        let newDataForNode =
              case M.lookup dataName dataForNode of
                Nothing ->
                  -- There is no historical points for this dataName yet.
                  let firstPoint = S.singleton (utc2s now, valueH)
                  in M.insert dataName firstPoint dataForNode
                Just points ->
                  let pointsWeKeep = S.fromList . deleteOutdated . S.toAscList $ points
                      newPoints = S.insert (utc2s now, valueH) pointsWeKeep
                  in M.adjust (const newPoints) dataName dataForNode
        in M.adjust (const newDataForNode) nodeId currentHistory
 where
  -- All points that older than 'minAge' should be deleted.
  deleteOutdated = go
   where
    go [] = []
    go (point@(tsInSec, _):otherPoints) =
      if tsInSec < minAge
        then
          -- This point is too old, do not keep it anymore.
          go otherPoints
        else
          -- This point should be kept.
          -- Since the points were converted to asc list, all the next points
          -- are definitely newer (have bigger 'tsInSec'), so there is no need
          -- to check them.
          point : otherPoints

  !minAge = utc2s now - pointsAgeInSec
  pointsAgeInSec = 12 * 60 * 60

getHistoricalData
  :: History
  -> NodeId
  -> DataName
  -> IO [(POSIXTime, ValueH)]
getHistoricalData history nodeId dataName = do
  history' <- readTVarIO history
  case M.lookup nodeId history' of
    Nothing -> return []
    Just dataForNode ->
      case M.lookup dataName dataForNode of
        Nothing -> return []
        Just points -> return $ S.toAscList points

readValueI
  :: Monad m
  => Text
  -> (ValueH -> m ())
  -> m ()
readValueI t f =
  case decimal t of
    Left _ -> return ()
    Right (i, _) -> f (ValueI i)

readValueD
  :: Monad m
  => Text
  -> (ValueH -> m ())
  -> m ()
readValueD t f =
  case double t of
    Left _ -> return ()
    Right (d, _) -> f (ValueD d)
