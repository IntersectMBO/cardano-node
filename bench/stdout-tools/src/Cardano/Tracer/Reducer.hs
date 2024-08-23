{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------

module Cardano.Tracer.Reducer
  (
    Reducer (..)

  -- Plain counters (do not inspect the trace message)
  , Count (..)

  -- Basic time reducers (only inspect the timestamp).
  , Silences (..)

  -- Others (inspect the JSON data past the timestamp and namespace).
  , MissedSlots (..)

  -- Changes of a data point with timestamp.
  , Changes (..)

  , CpuTicks (..)

  ) where

--------------------------------------------------------------------------------

-- base.
import           Control.Monad (when, foldM)
import           Data.Kind (Type)
import           Data.Foldable (toList)
-- package: time.
import           Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
import           Data.Time.Format.ISO8601 (iso8601Show)
-- package: text.
import qualified Data.Text as Text
-- package: containers.
import qualified Data.Sequence as Seq
-- library.
import qualified Cardano.Tracer.Trace as Trace

--------------------------------------------------------------------------------

class Reducer r where
  type family Elem  r :: Type
  type family Accum r :: Type
  initialOf :: r -> Accum r
  reducerOf :: r -> Accum r -> Elem r -> Accum r
  showAns   :: r -> Accum r -> String
  printAns  :: r -> Accum r -> IO ()
  printAns r acc = putStrLn $ showAns r acc

--------------------------------------------------------------------------------

data Count t = Count
  deriving Show

newtype Silences = Silences NominalDiffTime
  deriving Show

data MissedSlots = MissedSlots
  deriving Show

newtype Changes t = Changes (t -> Integer)

instance Show (Changes t) where
  show _ = "Changes"

data CpuTicks = CpuTicks
  deriving Show

--------------------------------------------------------------------------------

instance Reducer (Count t) where
  type instance Elem  (Count t) = t
  type instance Accum (Count t) = Int
  initialOf _ = 0
  reducerOf _ l _ = l + 1
  showAns   _ = show

instance Reducer Silences where
  type instance Elem  Silences = (UTCTime, Text.Text)
  type instance Accum Silences = (Maybe UTCTime, Seq.Seq (NominalDiffTime, UTCTime, UTCTime))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf (Silences s) (maybePrevAt, sq) (thisTraceAt, _) = do
    case maybePrevAt of
      Nothing -> (Just thisTraceAt, sq)
      (Just prevTraceAt) ->
        let diffTime = diffUTCTime thisTraceAt prevTraceAt
        in  if diffTime >= s
        then (Just thisTraceAt, sq Seq.|> (diffTime, prevTraceAt, thisTraceAt))
        else (Just thisTraceAt, sq)
  showAns   _ = show
  printAns _ (_,sq) = mapM_
    (\(ndt, t1, _) ->
      putStrLn $ show ndt ++ " (" ++ show t1 ++ ")"
    )
    (toList sq)

instance Reducer MissedSlots where
  type instance Elem  MissedSlots = Trace.Remainder Trace.DataWithSlot
  type instance Accum MissedSlots = (Maybe Integer, Seq.Seq Integer)
  initialOf _ = (Nothing, Seq.empty)
  -- TODO: Bench this strictness annotation!
  reducerOf MissedSlots (maybePrevSlot, !sq) aeson = do
    -- TODO: Use `unsnoc` when available
    let actualSlot = (Trace.slot . Trace.remainderData) aeson
    case maybePrevSlot of
      Nothing -> (Just actualSlot, Seq.empty)
      (Just prevSlot) ->
        if actualSlot == prevSlot + 1
        then (Just actualSlot, sq)
        else (Just actualSlot, sq Seq.>< Seq.fromList [(prevSlot+1)..(actualSlot-1)])
  showAns _ = show
  printAns _ (_, sq) = do
    ans <- foldM
      (\maybePrevSlot lostSlot ->
        case maybePrevSlot of
          Nothing -> putStr (show lostSlot) >> return (Just lostSlot)
          (Just prevSlot) ->
            if prevSlot + 1 == lostSlot
            then return (Just lostSlot)
            else putStrLn (".." ++ show prevSlot) >> return Nothing
      )
      Nothing
      (toList sq)
    when (ans /= Nothing) (putStrLn "")

instance Reducer (Changes t) where
  type instance Elem  (Changes t) = (UTCTime, t)
  type instance Accum (Changes t) = (Maybe (UTCTime, Integer), Seq.Seq (UTCTime, Integer))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf (Changes f) (maybePrev, sq) (at, eason) =
    let actual = f eason
        sq' = case maybePrev of
                Nothing -> Seq.singleton (at, actual)
                (Just (_,prev)) ->
                  if actual == prev
                  then sq
                  else sq Seq.|> (at, actual)
    in (Just (at, actual), sq')
  showAns _ = show
  printAns _ (maybeLast, sq) = do
    let (t0, _) = Seq.index sq 0
    let sq' = case maybeLast of
                Nothing -> sq
                (Just (t,h)) -> sq Seq.|> (t, h)
    putStrLn "# X Y"
    mapM_
      (\(t,h) ->
        --let showT = show t
        --    size  = length showT
        --    extra = if size < 33 -- "2024-04-06 05:55:39.600335766 UTC"
        --            then replicate (33 - size) ' '
        --            else ""
        --in putStrLn $ showT ++ "; " ++ extra ++ show h
        let diffTime = diffUTCTime t t0
        in putStrLn $ show (realToFrac diffTime :: Double) ++ " " ++ show h
      )
      (toList sq')

instance Reducer CpuTicks where
  type instance Elem  CpuTicks = (UTCTime, Trace.Remainder Trace.DataResources)
  type instance Accum CpuTicks = (Maybe (UTCTime,Integer), Seq.Seq (UTCTime, NominalDiffTime, Integer))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf CpuTicks ans@(maybePrev, sq) (at, resources) = do
    case maybePrev of
      Nothing ->
        let actualCpu = (Trace.resourcesCentiCpu . Trace.remainderData) resources
        in (
             Just (at, actualCpu)
           , Seq.singleton (
                             at
                           , 0
                           , actualCpu
                           --, (Trace.resourcesCentiGC  . Trace.remainderData) resources
                           --, (Trace.resourcesCentiMut . Trace.remainderData) resources
                           )
           )
      (Just (prevAt,_prevCpu)) ->
        let actualCpu = (Trace.resourcesCentiCpu . Trace.remainderData) resources
        in if False
           then ans
           else (
                  Just (at, actualCpu)
                , sq Seq.|> (
                              at
                            , diffUTCTime at prevAt
                            , actualCpu
                            --, (Trace.resourcesCentiGC  . Trace.remainderData) resources
                            --, (Trace.resourcesCentiMut . Trace.remainderData) resources
                            )
                )
  showAns _ = show
  printAns _ (_, sq) = mapM_
    (\(t,nt,cpu) ->
      putStrLn $
                  iso8601Show t
        ++ "," ++ show (realToFrac nt :: Double)
        ++ "," ++ show cpu
        -- ++ "," ++ show gc
        -- ++ "," ++ show mut
    )
    (toList sq)
