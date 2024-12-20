{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Analysis.Reducer.Util
       (module Cardano.Analysis.Reducer.Util)
       where

import           Cardano.Api (SlotNo)

import           Cardano.Unlog.LogObject (HostLogs (..), LOBody (..), LogObject (..), RunLogs (..))
import           Cardano.Util (StrictMaybe (..))

import           Data.List.Split (chop)
import           Data.Maybe
import           Data.Reducer


newtype BySlot a = BySlot (SlotNo, [a])
        deriving newtype Show

bySlot :: (a -> Maybe SlotNo) -> (a -> b) -> Bool -> [a] -> [BySlot b]
bySlot newSlot proj includeSeparator xs =
  case dropWhile (isNothing . newSlot) xs of
    [] -> []
    ys -> chop go ys
  where
    go (lo:los) = let
                    (inSlot_, rest) = span (isNothing . newSlot) los
                    inSlot          = if includeSeparator then lo:inSlot_ else inSlot_
                  in (BySlot (fromJust $ newSlot lo, map proj inSlot), rest)
    go []       = error "bySlot/chop: empty list"

bySlotLogObjects :: [LogObject] -> [BySlot LogObject]
bySlotLogObjects =
  bySlot
    (\LogObject{loBody} -> case loBody of { LOTraceStartLeadershipCheck s _ _ -> Just s; _ -> Nothing })
    id
    True

reduceHostLogs ::
  ( Reducer r
  , Foldable f
  , Elem r ~ a
  , Result r ~ b
  ) => r -> HostLogs (f a) -> HostLogs b
reduceHostLogs r HostLogs{hlLogs = (file, logs), ..} =
  HostLogs{hlLogs = (file, reduce r logs), ..}

reduceRunLogs ::
  ( Reducer r
  , Foldable f
  , Elem r ~ a
  , Result r ~ b
  ) => r -> RunLogs (f a) -> RunLogs b
reduceRunLogs r RunLogs{rlHostLogs = logs, ..} =
  RunLogs{rlHostLogs = reduceHostLogs r <$> logs, ..}

safeLast :: [a] -> [a]
safeLast [] = []
safeLast xs = [last xs]

{-# INLINE unsafeFromSJust #-}
unsafeFromSJust :: StrictMaybe a -> a
unsafeFromSJust = \case
  SJust a   -> a
  SNothing  -> error "unsafeFromSJust: got SNothing"
